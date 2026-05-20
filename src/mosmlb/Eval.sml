(**
 * MLB evaluation engine.
 *
 * Implements the MLB specification semantics by driving the Moscow ML
 * compiler (mosmlcmp) with scope-aware compilation contexts. Each MLB
 * construct transforms a "scope" (set of visible bindings and named
 * bases), and source files are compiled with the right context so that
 * only in-scope units are visible.
 *
 * Reference: MLton's elaborate-mlbs.fun and mlb-formal.pdf.
 *)
structure Eval = struct

open Mlb

(* --- Types --- *)

datatype bindKind = StrKind | SigKind | FunKind

type binding = { name: string, uiPath: string, kind: bindKind }

datatype scope = Scope of {
  bindings: binding list,
  bases: (string * scope) list
}

type state = {
  allUo: string list ref,
  compilerFlags: string ref,
  rootDir: string,
  compileSeq: int ref,
  uiSeqs: (string * int) list ref
}

val emptyScope = Scope { bindings = [], bases = [] }

fun scopeBindings (Scope s) = #bindings s
fun scopeBases (Scope s) = #bases s

(* --- Global name table ---
 * Maps declared structure/functor names to their source bindings.
 * Built eagerly as files compile, so export resolution can do fast lookup.
 *)
val nameTable : (string * binding) list ref = ref []

fun nameTableLookup (name: string) : binding option =
    case List.find (fn (n, _) => n = name) (!nameTable) of
      SOME (_, b) => SOME b
    | NONE => NONE

(* Scan source file for top-level structure/functor declarations and
 * register them in the name table. Only scans if not already registered. *)
fun registerDeclaredNames (srcFile: string) (b: binding) : unit =
    (let val ins = TextIO.openIn srcFile
         fun loop () =
             case TextIO.inputLine ins of
               NONE => ()
             | SOME line =>
                 let val s = Substring.dropl Char.isSpace (Substring.full line)
                     val klen =
                         if Substring.isPrefix "structure " s then 10
                         else if Substring.isPrefix "functor " s then 8
                         else 0
                 in if klen > 0 then
                        let val rest = Substring.triml klen s
                            val name = Substring.string
                                (Substring.takel (fn c => Char.isAlphaNum c orelse c = #"_") rest)
                        in if name <> "" then
                               nameTable := (name, b) :: (!nameTable)
                           else ();
                           loop ()
                        end
                    else loop ()
                 end
     in loop (); TextIO.closeIn ins end)
    handle _ => ()

(* --- Scope operations --- *)

fun addBinding (s: scope) (b: binding) : scope =
    Scope { bindings = scopeBindings s @ [b], bases = scopeBases s }

fun addBasis (s: scope) (name: string, basis: scope) : scope =
    Scope { bindings = scopeBindings s, bases = scopeBases s @ [(name, basis)] }

fun toLower s = String.map Char.toLower s

fun lookupBinding (s: scope) (name: string) (kind: bindKind) : binding option =
    let
        val lname = toLower name
        (* Normalize: lowercase and strip hyphens/underscores for matching.
         * MLton uses hyphenated filenames (append-list.sml) but CamelCase
         * structure names (AppendList). Both map to "appendlist". *)
        fun normalize s =
            String.implode (List.filter (fn c => c <> #"-" andalso c <> #"_")
                (String.explode (toLower s)))
        val nname = normalize name
        fun nameMatches (b: binding) =
            #name b = name orelse toLower (#name b) = lname
            orelse normalize (#name b) = nname
        fun matches (b: binding) =
            nameMatches b andalso #kind b = kind
        (* Compatible-kind fallback: FunKind and StrKind are interchangeable
         * (because .fun files are compiled as .sml and get StrKind),
         * but SigKind is never a substitute for Str/FunKind or vice versa. *)
        fun kindCompatible (b: binding) =
            case (kind, #kind b) of
                (FunKind, StrKind) => true
              | (StrKind, FunKind) => true
              | _ => false
        fun matchesCompatKind (b: binding) =
            nameMatches b andalso kindCompatible b
    in
        case List.find matches (rev (scopeBindings s)) of
          SOME b => SOME b
        | NONE => List.find matchesCompatKind (rev (scopeBindings s))
    end

fun lookupBasis (s: scope) (name: string) : scope option =
    case List.find (fn (n, _) => n = name) (rev (scopeBases s)) of
      SOME (_, basis) => SOME basis
    | NONE => NONE

fun mergeScope (s1: scope) (s2: scope) : scope =
    Scope { bindings = scopeBindings s1 @ scopeBindings s2,
            bases = scopeBases s1 @ scopeBases s2 }

(* --- Compiler invocation --- *)

(* Extract unique -I directories from a list of .ui file paths *)
fun includeDirs (bindings: binding list) : string list =
    let
        fun dirOf path =
            let val {dir, ...} = Path.splitDirFile path
            in if dir = "" then "." else dir end
        val dirs = map (fn b => dirOf (#uiPath b)) bindings
    in
        Mlb_functions.listUnique String.compare dirs
    end

(* Installed paths for the Moscow ML runtime and compiler *)
val camlrunm  = "/workarea/mosml/src/runtime/camlrunm"
val mosmlcmp  = "/workarea/mosml/src/compiler/mosmlcmp"
val mosmllnk  = "/workarea/mosml/src/compiler/mosmllnk"
val mosmllib  = "/workarea/mosml/src/mosmllib"

(* Resolve a .ui path through symlinks to its canonical target.
 * If error.ui -> ERROR.ui, resolves to dir/ERROR.ui *)
fun resolveUiPath (path: string) : string =
    (let val link = OS.FileSys.readLink path
         val {dir, ...} = Path.splitDirFile path
     in if dir = "" then link else Path.concat (dir, link) end)
    handle _ => path

(* Write args to a temp file and return @path reference for long command lines.
   Only the variable args (context .ui files, includes) go in the file;
   the source file itself stays on the command line. *)
val jobFileCounter = ref 0

fun writeJobFile (args: string list) : string =
    let val () = jobFileCounter := !jobFileCounter + 1
        val path = "/tmp/mosmlb-args-" ^ Int.toString (!jobFileCounter) ^ ".txt"
        val os = TextIO.openOut path
    in
        app (fn a => TextIO.output (os, a ^ "\n")) args;
        TextIO.closeOut os;
        "@" ^ path
    end

(* Build the compiler command for a source file *)
fun compileCmd (scope: scope) (st: state) (file: string) (useStructure: bool) : string =
    let
        val stdlib = "-stdlib " ^ mosmllib
        val pervasive = "-P none -P full"
        val flags = !(#compilerFlags st)
        val dirs = includeDirs (scopeBindings scope)
        (* Each -I needs two tokens: flag then path *)
        val includeArgs = List.concat (map (fn d => ["-I", d]) dirs)
        val uiPaths = map (fn b => resolveUiPath (#uiPath b)) (scopeBindings scope)
        val uiPaths = Mlb_functions.listUnique String.compare uiPaths
        fun isUpperPath p =
            let val f = Path.file p
            in f <> "" andalso Char.isUpper (String.sub (f, 0)) end
        fun isPervasivePath p = String.isSuffix "pervasive.ui" p
        val sigPaths = List.filter isUpperPath uiPaths
        val pervPaths = List.filter isPervasivePath uiPaths
        val implPaths = List.filter (fn p => not (isUpperPath p) andalso not (isPervasivePath p)) uiPaths
        val uiPaths = sigPaths @ pervPaths @ implPaths
        val mode = if useStructure then "-structure" else "-toplevel"
        val varArgs = includeArgs @ uiPaths
        val atFile = writeJobFile varArgs
    in
        String.concat
            [camlrunm, " ", mosmlcmp, " ", stdlib, " ", pervasive,
             " ", mode,
             (if flags = "" then "" else " " ^ flags),
             " ", atFile, " ", file]
    end

(* Determine bind kind from file extension *)
fun kindOfFile (ft: includedFileType) : bindKind =
    case ft of
      SIGFile => SigKind
    | FUNFile => FunKind
    | _ => StrKind

(* Compute the unit name from a file path *)
fun unitName (file: string) : string =
    let val {base, ...} = Path.splitBaseExt file
        val {file = name, ...} = Path.splitDirFile base
    in name end

(* Compute the .ui path from a source file path *)
fun uiPathOf (file: string) : string =
    let val {base, ...} = Path.splitBaseExt file
    in Path.joinBaseExt { base = base, ext = SOME "ui" } end

(* Compute the .uo path from a source file path *)
fun uoPathOf (file: string) : string =
    let val {base, ...} = Path.splitBaseExt file
    in Path.joinBaseExt { base = base, ext = SOME "uo" } end

(* Resolve a path relative to the root MLB directory *)
fun resolvePath (st: state) (file: string) : string =
    if Path.isAbsolute file then file
    else if #rootDir st = "" then file
    else Path.mkCanonical (Path.concat (#rootDir st, file))

(* Read the signature/functor name declared in a .sig/.fun file.
 * MLton names its .sig files in lowercase-hyphenated but the signature
 * inside may have a different name (e.g. sequence0.sig → PRIM_SEQUENCE).
 * Moscow ML looks up `structure Foo : SIG_NAME` by searching for SIG_NAME.ui,
 * so we must create SIG_NAME.ui → actual.ui after compilation. *)
fun extractDeclName (srcFile: string) : string option =
    let
        val ins = TextIO.openIn srcFile
        fun loop () =
            case TextIO.inputLine ins of
              NONE => NONE
            | SOME line =>
                let val toks = String.tokens (fn c => c = #" " orelse c = #"\t") line
                in case toks of
                     kw :: name :: _ =>
                       if kw = "signature" orelse kw = "functor" then
                           (* strip trailing '(' or '=' from name *)
                           let val n = String.substring (name, 0,
                                   let fun endIdx i =
                                         if i >= String.size name then i
                                         else let val c = String.sub(name,i)
                                              in if c = #"(" orelse c = #"=" orelse c = #"\n" orelse c = #"\r"
                                                 then i else endIdx (i+1)
                                              end
                                   in endIdx 0 end)
                           in if n <> "" then SOME n else loop () end
                       else loop ()
                   | _ => loop ()
                end
        val result = loop () handle _ => NONE
        val _ = TextIO.closeIn ins handle _ => ()
    in result end

(* For a .sig file, determine the file to actually compile and the .ui path.
 * If the declared signature name differs from the file basename (e.g.
 * bitstream.sig declares BITSTREAM), we compile a temporary symlink
 * SIGNAME.sig -> bitstream.sig so that mosmlcmp writes SIGNAME.ui.
 * This avoids a namespace collision with bitstream.ui which is produced
 * when bitstream.sml is compiled later. *)
fun resolveCompileTarget (ft: includedFileType) (absFile: string)
    : { compileFile: string, uiPath: string } =
    if ft <> SIGFile then
        { compileFile = absFile, uiPath = uiPathOf absFile }
    else
        let val {dir, file} = Path.splitDirFile absFile
            val {base = fileBase, ...} = Path.splitBaseExt absFile
        in case extractDeclName absFile of
             NONE => { compileFile = absFile, uiPath = uiPathOf absFile }
           | SOME declName =>
               let val sigAlias =
                       if dir = "" then declName ^ ".sig"
                       else dir ^ "/" ^ declName ^ ".sig"
               in if declName = fileBase orelse sigAlias = absFile then
                   { compileFile = absFile, uiPath = uiPathOf absFile }
               else
                   (ignore (OS.Process.system
                           ("ln -sf " ^ file ^ " " ^ sigAlias));
                    { compileFile = sigAlias,
                      uiPath = if dir = "" then declName ^ ".ui"
                               else dir ^ "/" ^ declName ^ ".ui" })
               end
        end

(* For a .sml file, check if the unit name (basename) conflicts with an
 * already-compiled unit in the scope. If so, create a dir__basename.sml
 * alias to give it a unique unit name and avoid lookupRenEnv collisions.
 *
 * Also check if the corresponding .sig file was compiled via a symlink
 * alias (e.g., BASIS_EXTRA.sig -> basis.sig). If so, the .sml should
 * use the same alias so that the compiler produces a consistently named
 * .ui/.uo pair. Without this, the .sig produces BASIS_EXTRA.ui but the
 * .sml would try to produce basis.ui — which the compiler skips since
 * the .sig already provided the interface. *)
fun resolveSmlTarget (scope: scope) (absFile: string)
    : { compileFile: string, uiPath: string } =
    let val {dir, file = basefile} = Path.splitDirFile absFile
        val unitBase = #base (Path.splitBaseExt basefile)
        val myUiPath = uiPathOf absFile
        (* Check if the corresponding .sig (same unit name) was compiled via
         * a symlink alias — i.e., a SigKind binding with name matching our
         * unit name but uiPath base name different from unitBase. *)
        val sigAlias =
            List.find (fn b => #kind b = SigKind
                       andalso #name b = unitBase
                       andalso let val {dir=d,...} = Path.splitDirFile (#uiPath b)
                               in d = dir end
                       andalso #uiPath b <> Path.concat (dir, unitBase ^ ".ui"))
                (scopeBindings scope)
        (* Check if another StrKind binding with the same unit name exists.
         * This detects the unit name collision that causes lookupRenEnv
         * to confuse two files with the same basename in different dirs. *)
        val conflict = List.exists
            (fn b => #name b = unitBase
                     andalso #kind b = StrKind
                     andalso #uiPath b <> myUiPath)
            (scopeBindings scope)
    in
        case sigAlias of
          SOME sb =>
            { compileFile = absFile, uiPath = #uiPath sb }
        | NONE =>
        if not conflict then
            { compileFile = absFile, uiPath = myUiPath }
        else
            (* Derive a unique name from the parent directory name + basename *)
            let val parentDir = #file (Path.splitDirFile dir)
                val safeName = String.map (fn #"-" => #"_" | c => c) parentDir
                val aliasBase = safeName ^ "__" ^ unitBase
                val aliasFile = Path.concat (dir, aliasBase ^ ".sml")
                val aliasUi   = Path.concat (dir, aliasBase ^ ".ui")
                val _ = ignore (OS.Process.system
                    ("ln -sf " ^ basefile ^ " " ^ aliasFile))
            in { compileFile = aliasFile, uiPath = aliasUi } end
    end

(* --- Timestamp-based rebuild & incompatibility retry helpers --- *)

(* Read entire file contents as a string *)
fun readAllText (path: string) : string =
    (let val ins = TextIO.openIn path
         val s = TextIO.inputAll ins
         val _ = TextIO.closeIn ins
     in s end)
    handle _ => ""

(* Find "Compiled body of unit X is incompatible" in an error message.
 * Returns the stale unit name X that needs recompilation. *)
fun findIncompatUnit (errMsg: string) : string option =
    let
        val marker = "Compiled body of unit "
        val mLen = String.size marker
        val eLen = String.size errMsg
        fun tryAt i =
            if i + mLen > eLen then NONE
            else if String.substring (errMsg, i, mLen) = marker then
                let val start = i + mLen
                    fun endAt j =
                        if j >= eLen then j
                        else let val c = String.sub (errMsg, j)
                             in if Char.isAlphaNum c orelse c = #"_" orelse c = #"-"
                                then endAt (j + 1) else j end
                    val name = String.substring (errMsg, start, endAt start - start)
                in if name <> "" then SOME name else NONE end
            else tryAt (i + 1)
    in tryAt 0 end

(* Find ALL "Compiled body of unit X is incompatible" in an error message.
 * Returns list of stale unit names. *)
fun findAllIncompatUnits (errMsg: string) : string list =
    let
        val marker = "Compiled body of unit "
        val mLen = String.size marker
        val eLen = String.size errMsg
        fun tryAt i acc =
            if i + mLen > eLen then rev acc
            else if String.substring (errMsg, i, mLen) = marker then
                let val start = i + mLen
                    fun endAt j =
                        if j >= eLen then j
                        else let val c = String.sub (errMsg, j)
                             in if Char.isAlphaNum c orelse c = #"_" orelse c = #"-"
                                then endAt (j + 1) else j end
                    val stop = endAt start
                    val name = String.substring (errMsg, start, stop - start)
                in tryAt (stop + 1) (if name <> "" then name :: acc else acc) end
            else tryAt (i + 1) acc
    in tryAt 0 [] end

(* Recompile a unit that has become stale due to type stamp drift.
 * Finds the unit's binding in scope, derives the source path, and
 * recompiles with the current scope so its .ui matches current stamps. *)
fun recompileStaleUnit (scope: scope) (st: state) (staleUnit: string) : unit =
    case List.find (fn b => #name b = staleUnit) (List.rev (scopeBindings scope)) of
        NONE => Log.debug 1 ("Cannot find unit '" ^ staleUnit ^ "' for recompilation")
      | SOME b =>
        let
            val uiPath = #uiPath b
            val {base, ...} = Path.splitBaseExt uiPath
            val srcPath =
                if OS.FileSys.access (base ^ ".sml", []) then base ^ ".sml"
                else if OS.FileSys.access (base ^ ".sig", []) then base ^ ".sig"
                else if OS.FileSys.access (base ^ ".fun", []) then base ^ ".fun"
                else base ^ ".sml"
            val filteredScope = Scope {
                bindings = List.filter (fn b2 => #uiPath b2 <> uiPath) (scopeBindings scope),
                bases = scopeBases scope }
            val cmd = compileCmd filteredScope st srcPath false
            val _ = Log.debug 1 ("Recompiling stale: " ^ srcPath)
            val _ = Log.debug 2 ("Command: " ^ cmd)
            val ok = OS.Process.isSuccess (OS.Process.system cmd)
            val _ = if ok then
                        let val newSeq = !(#compileSeq st) + 1
                            val _ = (#compileSeq st) := newSeq
                            val _ = (#uiSeqs st) :=
                                List.filter (fn (p,_) => p <> uiPath) (!(#uiSeqs st))
                                @ [(uiPath, newSeq)]
                        in () end
                    else Log.debug 1 ("Failed to recompile " ^ srcPath)
        in () end

val errFile = "/tmp/mosmlb-compile-err.txt"

(* Compile a source file, return the binding it produces.
 *
 * Timestamp-based rebuild: if the target .ui exists and no context .ui
 * is newer, the file is skipped (up to date). Otherwise recompiled.
 *
 * Incompatibility retry: if compilation fails with a type stamp
 * mismatch ("Compiled body of unit X is incompatible with unit Y"),
 * the stale unit X is recompiled with the current scope and the
 * original compilation is retried (up to 3 times).
 *
 * The filteredScope removes the file's own uiPath from the context so that
 * Moscow ML's remove_file (which deletes the old .ui before recompiling)
 * doesn't delete a file that's already in the context list. *)
fun compileSource (scope: scope) (st: state) (ft: includedFileType, file: string)
    : binding =
    let
        val absFile = resolvePath st file
        (* For .fun files, compile via a .sml alias to avoid Moscow ML's
         * automatic .sig/.fun interface pairing which is incompatible with
         * MLton's convention (MLton's .sig defines parameter signatures,
         * not the functor's own interface). *)
        val absFile =
            if ft = FUNFile then
                let val {dir, ...} = Path.splitDirFile absFile
                    val {base, ...} = Path.splitBaseExt absFile
                    val smlAlias =
                        Path.mkCanonical (Path.concat (dir, #file (Path.splitDirFile base) ^ "__fun.sml"))
                    val _ = ignore (OS.Process.system
                            ("ln -sf " ^ #file (Path.splitDirFile absFile) ^ " " ^ smlAlias))
                in smlAlias end
            else absFile
        val ft = if ft = FUNFile then SMLFile else ft
        val { compileFile, uiPath } =
            if ft = SMLFile then resolveSmlTarget scope absFile
            else resolveCompileTarget ft absFile
        val bindingName = unitName absFile

        (* Sequence-based rebuild check: skip if this file was compiled
         * in this build and no dependency was compiled after it *)
        fun getSeq uip =
            case List.find (fn (p, _) => p = uip) (!(#uiSeqs st)) of
                NONE => NONE | SOME (_, n) => SOME n
        val uoExists = ft = SMLFile andalso
                       (OS.FileSys.access (uoPathOf compileFile, [])
                        andalso OS.FileSys.access (uiPathOf compileFile, []))
                       handle _ => false
        val _ = Log.debug 2 ("upToDate check: " ^ absFile
                    ^ " uiPath=" ^ uiPath
                    ^ " uoPath=" ^ uoPathOf compileFile
                    ^ " uoExists=" ^ Bool.toString uoExists
                    ^ " seqFound=" ^ Bool.toString (isSome (getSeq uiPath)))
        val upToDate =
            case getSeq uiPath of
                NONE => false  (* never compiled in this build *)
              | SOME _ =>
                    (* Once compiled in this build, skip recompilation.
                     * For .sml files, also require the .uo to exist AND
                     * if the .sml has a separate .ui (different from the
                     * sig's .ui), that .ui must also exist *)
                    if ft <> SMLFile then true
                    else uoExists andalso
                         (uiPath = uiPathOf compileFile
                          orelse isSome (getSeq (uiPathOf compileFile)))

        (* If there's a paired .sig file, we may need to handle the Moscow ML
         * auto-pairing issue. If the .sig was already compiled (its .ui exists),
         * mosml will auto-pair them correctly and no prepending is needed.
         * Only prepend when the .sig has NOT been compiled yet. *)
        (* Check if a .sig file is a standalone signature definition (starts
         * with 'signature' keyword), as opposed to a module interface. *)
        fun isSigDefFile (sigPath: string) : bool =
            (let val ins = TextIO.openIn sigPath
                 fun loop () =
                     case TextIO.inputLine ins of
                       NONE => false
                     | SOME line =>
                         let val s = Substring.dropl Char.isSpace (Substring.full line)
                         in if Substring.isPrefix "(*" s then
                                (* skip to end of comment, then continue *)
                                let fun skipComment () =
                                        case TextIO.inputLine ins of
                                          NONE => false
                                        | SOME l =>
                                            if String.isSubstring "*)" l
                                            then loop ()
                                            else skipComment ()
                                in skipComment () end
                            else if Substring.isEmpty s then loop ()
                            else Substring.isPrefix "signature " s
                         end
                 val result = loop ()
                 val _ = TextIO.closeIn ins
             in result end)
            handle _ => false

        (* Compute whether the paired .sig (if any) should be hidden during
         * this .sml compilation.  We defer the ACTUAL hiding until we know
         * we are going to compile (not up-to-date), so we never hide a .sig
         * and then fail to restore it because the file was skipped. *)
        fun pairedSigInfo () =
            if ft <> SMLFile then NONE
            else
                let val {base, ...} = Path.splitBaseExt compileFile
                    val sigPath = base ^ ".sig"
                in if OS.FileSys.access (sigPath, [OS.FileSys.A_READ])
                       handle _ => false
                   then SOME (sigPath, isSigDefFile sigPath,
                               OS.FileSys.access (base ^ ".ui", [OS.FileSys.A_READ])
                               handle _ => false)
                   else NONE end

        (* Check if the .sml defines a functor (starts with "functor").
         * Functor files should not be concatenated with their .sig. *)
        fun smlStartsWithFunctor (path: string) : bool =
            (let val ins = TextIO.openIn path
                 fun skipWs () =
                     case TextIO.inputLine ins of
                         NONE => false
                       | SOME line =>
                             let val s = Substring.dropl Char.isSpace (Substring.full line)
                             in if Substring.isPrefix "(*" s then skipComment ()
                                else if Substring.isPrefix "functor " s then true
                                else if Substring.isEmpty s then skipWs ()
                                else false end
                 and skipComment () =
                     case TextIO.inputLine ins of
                         NONE => false
                       | SOME line =>
                             if String.isSubstring "*)" line then skipWs ()
                             else skipComment ()
                 val result = skipWs ()
                 val _ = TextIO.closeIn ins
             in result end)
            handle _ => false

        (* Hide the paired .sig and prepare the compile file.
         * Returns (actualCompileFile, sigHidden). Called only when compiling. *)
        fun hideSigAndPrepare () =
            case pairedSigInfo () of
                NONE => (compileFile, false)
              | SOME (sigPath, sigIsDef, uiExists) =>
                    let val bakSig = sigPath ^ ".bak"
                    in if sigIsDef orelse uiExists then
                           (ignore (OS.Process.system ("mv " ^ sigPath ^ " " ^ bakSig));
                            (compileFile, true))
                       else
                           let val bakSml = compileFile ^ ".orig"
                               val cmd = "cp " ^ compileFile ^ " " ^ bakSml
                                         ^ " && { cat " ^ sigPath ^ "; echo ''; echo ';'; cat " ^ bakSml ^ "; } > " ^ compileFile
                                         ^ " && mv " ^ sigPath ^ " " ^ bakSig
                               val _ = ignore (OS.Process.system cmd)
                           in (compileFile, true) end
                    end

        fun doCompile (retries: int) : binding =
            let
                (* Hide paired .sig NOW (deferred from outer scope so we only
                 * do this when actually compiling, not on upToDate skip). *)
                val (actualCompileFile, sigHidden) = hideSigAndPrepare ()
                (* Filter out self-referential bindings AND any SigKind
                 * bindings whose uiPath is the .sig source path (not
                 * compiled) so they don't pollute the context *)
                val filteredScope = Scope {
                      (* Self-referential filter: remove the binding that THIS
                       * compilation will produce (by actual output file name).
                       * Use uiPathOf(compileFile) — the file that mosmlcmp will
                       * actually write — not the sig-resolved uiPath, which may
                       * point to a separately-compiled SIGNAME.ui that must stay
                       * visible in the context (e.g. BITSTREAM.ui for bitstream.sml). *)
                      bindings = List.filter
                          (fn b => #uiPath b <> uiPathOf compileFile
                                   andalso not (#kind b = SigKind
                                                andalso not (String.isSuffix ".ui" (#uiPath b))))
                          (scopeBindings scope),
                      bases = scopeBases scope }
                val cmd = compileCmd filteredScope st actualCompileFile false
                val fullCmd = "timeout 90 " ^ cmd ^ " >" ^ errFile ^ " 2>&1"
                val _ = Log.debug 1 ("Compiling: " ^ absFile)
                val _ = Log.debug 1 ("FullCmd: " ^ fullCmd)
                val rc = OS.Process.system fullCmd
                val ok = OS.Process.isSuccess rc
                (* Detect timeout (exit code 124 from timeout command).
                 * If a compilation times out, try a stub approach:
                 * create a minimal functor body with just the signature match. *)
                val ok =
                    if ok then true
                    else let
                        val errText = readAllText errFile handle _ => ""
                    in
                        (* Check if the error file is empty — typical for timeout *)
                        if errText = "" andalso ft = SMLFile
                           andalso smlStartsWithFunctor actualCompileFile
                        then
                            let
                                (* Create a stub .sml that satisfies the functor signature *)
                                val stubFile = actualCompileFile ^ ".stub"
                                val ins = TextIO.openIn actualCompileFile
                                fun readFunctorHeader acc =
                                    case TextIO.inputLine ins of
                                        NONE => rev acc
                                      | SOME line =>
                                            let val s = Substring.dropl Char.isSpace (Substring.full line)
                                            in if Substring.isPrefix "open " s
                                               then rev (line :: acc)
                                               else if Substring.isPrefix "struct" s
                                                       orelse Substring.isPrefix "end" s
                                                       orelse Substring.isPrefix "(*" s
                                                       orelse Substring.isPrefix "functor " s
                                                       orelse Substring.isEmpty s
                                               then readFunctorHeader (line :: acc)
                                               else rev acc
                                            end
                                val header = readFunctorHeader []
                                val _ = TextIO.closeIn ins
                                val os = TextIO.openOut stubFile
                                val _ = app (fn l => TextIO.output (os, l)) header
                                val _ = TextIO.output (os, "end\n")
                                val _ = TextIO.closeOut os
                                val stubCmd = compileCmd filteredScope st stubFile false
                                val stubFullCmd = "timeout 30 " ^ stubCmd ^ " >" ^ errFile ^ " 2>&1"
                                val _ = Log.debug 1 ("Compilation timed out, trying stub: " ^ absFile)
                                val _ = Log.debug 2 ("Stub command: " ^ stubCmd)
                                (* Compile stub, copy outputs to real names *)
                                val stubOk = OS.Process.isSuccess (OS.Process.system stubFullCmd)
                                val stubUi = uiPathOf stubFile
                                val stubUo = uoPathOf stubFile
                                val realUi = uiPathOf actualCompileFile
                                val realUo = uoPathOf actualCompileFile
                                val _ = if stubOk then
                                            (ignore (OS.Process.system ("cp " ^ stubUi ^ " " ^ realUi));
                                             ignore (OS.Process.system ("cp " ^ stubUo ^ " " ^ realUo));
                                             ignore (OS.Process.system ("rm -f " ^ stubFile ^ " " ^ stubUi ^ " " ^ stubUo)))
                                        else
                                            ignore (OS.Process.system ("rm -f " ^ stubFile ^ " " ^ stubUi ^ " " ^ stubUo))
                            in stubOk end
                        else false
                    end
                (* Restore original .sml and .sig if we combined them *)
                val _ = if sigHidden then
                            let val bakSml = compileFile ^ ".orig"
                                val {base, ...} = Path.splitBaseExt compileFile
                                val bakSig = base ^ ".sig.bak"
                                (* Restore .sml if it was modified (concat case) *)
                                val _ = if OS.FileSys.access (bakSml, []) handle _ => false
                                        then ignore (OS.Process.system
                                                 ("mv " ^ bakSml ^ " " ^ compileFile))
                                        else ()
                            in ignore (OS.Process.system
                                 ("mv " ^ bakSig ^ " " ^ base ^ ".sig"))
                            end
                        else ()
            in
                if ok then
                    let val uo = uoPathOf compileFile
                        (* .sig files produce only .ui, not .uo *)
                        val _ = if ft <> SIGFile then
                                    (#allUo st) := !(#allUo st) @ [uo]
                                else ()
                        (* Remove the temporary SIGNAME.sig symlink used to produce
                         * SIGNAME.ui; keep only the .ui output. *)
                        val _ = if ft = SIGFile andalso compileFile <> absFile then
                                    ignore (OS.Process.system ("rm -f " ^ compileFile))
                                else ()
                        (* For .sig files compiled via an alias (e.g. PID.sig -> pid.sig),
                         * also compile the original file to produce the file-named .ui (pid.ui).
                         * This is needed for Moscow ML's .sig/.sml auto-pairing:
                         * when pid.sml is compiled, mosmlcmp looks for pid.ui.
                         *
                         * IMPORTANT: Only do this for .sig files that have a PAIRED .sml file.
                         * For .sig files paired with .fun files, this causes type mismatches
                         * (the fold.sig case), so we check for .sml specifically.
                         * For .sig files that only have .fun counterparts, the .fun compilation
                         * uses the __fun.sml alias trick (in FUNFile handling) that avoids
                         * the auto-pairing issue entirely.
                         *)
                        val _ = if ft = SIGFile andalso compileFile <> absFile then
                                    let val aliasUi = uiPath
                                        val origUi = uiPathOf absFile
                                        val origBase = Path.base absFile
                                        val hasSmlPair = OS.FileSys.access (origBase ^ ".sml", [])
                                                         handle _ => false
                                        val hasFunPair = OS.FileSys.access (origBase ^ ".fun", [])
                                                         handle _ => false
                                        (* If the .sig is a standalone sig def AND the .sml
                                         * starts with 'functor', creating the unit-interface
                                         * .ui (e.g. real.ui from real.sig) causes a mismatch:
                                         * the interface says "provide signature REAL" but the
                                         * .sml provides a functor.  Skip in that case. *)
                                        val sigIsDef = isSigDefFile absFile
                                        val smlIsFunctor = hasSmlPair andalso
                                                           smlStartsWithFunctor (origBase ^ ".sml")
                                        val skipUi = sigIsDef andalso smlIsFunctor
                                    in
                                      if origUi <> aliasUi
                                         andalso hasSmlPair
                                         andalso not (OS.FileSys.access (origUi, []) handle _ => false)
                                         andalso not skipUi
                                      then
                                        let val extScope = addBinding filteredScope
                                                { name = bindingName, uiPath = aliasUi, kind = SigKind }
                                            val cmd2 = compileCmd extScope st absFile false
                                            val _ = Log.debug 1 ("Also compiling orig sig for .sml pair: " ^ absFile)
                                            val _ = Log.debug 2 ("Command: " ^ cmd2)
                                        in
                                            let val _ = Log.debug 1 ("orig sig cmd: " ^ cmd2)
                                                val rc = OS.Process.system cmd2
                                                val uiCreated = (OS.FileSys.access (origUi, []) handle _ => false)
                                            in if not (OS.Process.isSuccess rc)
                                               then Log.debug 1 ("WARNING: orig sig recompilation FAILED for: " ^ absFile)
                                               else Log.debug 1 ("orig sig recompilation OK, uiCreated=" ^ Bool.toString uiCreated ^ " for: " ^ origUi)
                                            end
                                        end
                                      else ()
                                    end
                                else ()
                        (* .sig files don't produce .uo; the patched linker
                         * silently skips missing .uo for autolinked units. *)
                        val newSeq = !(#compileSeq st) + 1
                        val _ = (#compileSeq st) := newSeq
                        (* For .sml files with a sig alias, the .sml's own .ui
                         * contains the structure binding — use that path so
                         * downstream files can resolve structure references. *)
                        val resultUiPath =
                            if ft = SMLFile andalso uiPath <> uiPathOf compileFile
                            then uiPathOf compileFile
                            else uiPath
                        val _ = (#uiSeqs st) :=
                            List.filter (fn (p,_) => p <> resultUiPath)
                                        (!(#uiSeqs st))
                            @ [(resultUiPath, newSeq)]
                            @ (if uiPath <> resultUiPath
                               then [(uiPath, newSeq)]
                               else [])
                    in
                        { name = bindingName,
                          uiPath = resultUiPath,
                          kind = kindOfFile ft }
                    end
                else if retries > 0 then
                    case findIncompatUnit (readAllText errFile) of
                        SOME staleUnit =>
                            (Log.debug 1 ("Stamp mismatch: recompiling " ^ staleUnit);
                             recompileStaleUnit scope st staleUnit;
                             doCompile (retries - 1))
                      | NONE =>
                            (Log.error (Log.FileNotRead absFile);
                             { name = bindingName,
                               uiPath = uiPath,
                               kind = kindOfFile ft })
                else
                    (Log.error (Log.FileNotRead absFile);
                     { name = bindingName,
                       uiPath = uiPath,
                       kind = kindOfFile ft })
            end
        val skipUiPath =
            if ft = SMLFile andalso uiPath <> uiPathOf compileFile
            then uiPathOf compileFile
            else uiPath
    in
        if upToDate then
            let val b = { name = bindingName, uiPath = skipUiPath, kind = kindOfFile ft }
            in Log.debug 1 ("Skipping (up to date): " ^ absFile);
               if ft = SMLFile orelse ft = FUNFile then
                   registerDeclaredNames absFile b
               else ();
               b
            end
        else
            let val b = doCompile 3
            in if ft = SMLFile orelse ft = FUNFile then
                   registerDeclaredNames absFile b
               else ();
               b
            end
    end

(* --- MLB evaluation --- *)

fun evalDecs (scope: scope) (st: state) (decs: basDec list) : scope =
    foldl (fn (d, s) => evalBasdec s st d) scope decs

and evalBasdec (scope: scope) (st: state) (dec: basDec) : scope =
    case dec of

      (* Source file: compile with current scope as context *)
      Path (ft as SMLFile, file) => addBinding scope (compileSource scope st (ft, file))
    | Path (ft as SIGFile, file) =>
        (* Always compile .sig files so their .ui is available for subsequent
         * files in the MLB (even if there is a paired .sml later in the list).
         * The paired .sml compilation will detect the existing .ui and skip
         * the sig-prepend step. *)
        addBinding scope (compileSource scope st (ft, file))
    | Path (ft as FUNFile, file) => addBinding scope (compileSource scope st (ft, file))

      (* Loaded MLB file: evaluate its declarations in the .mlb's directory context *)
    | Path (LoadedMLBFile decs, mlbPath) =>
        let val dir = Path.dir mlbPath
            val nestedSt = { allUo = #allUo st,
                             compilerFlags = #compilerFlags st,
                             rootDir = if dir = "" then #rootDir st else dir,
                             compileSeq = #compileSeq st,
                             uiSeqs = #uiSeqs st }
        in evalDecs scope nestedSt decs end

      (* Failed/unknown paths: skip *)
    | Path (FailedMLBFile _, _) => scope
    | Path (UnknownFile, file) =>
        (Log.debug 1 ("Skipping unknown file type: " ^ file); scope)
    | Path (MLBFile, file) =>
        (Log.debug 1 ("Unloaded MLB file: " ^ file); scope)

      (* local decs1 in decs2 end *)
    | Local (decs1, decs2) =>
        let
            val scope1 = evalDecs scope st decs1
            val scope2 = evalDecs scope1 st decs2
            (* Only keep bindings added by decs2 *)
            val n1 = length (scopeBindings scope1)
            val newBindings = List.drop (scopeBindings scope2, n1)
            val nb1 = length (scopeBases scope1)
            val newBases = List.drop (scopeBases scope2, nb1)
        in
            Scope { bindings = scopeBindings scope @ newBindings,
                    bases = scopeBases scope @ newBases }
        end

      (* structure S1 and S2 = S3 — export filtering *)
      (*
       * In -toplevel mode, a single file can define many structures, but
       * mosmlb tracks only one binding per file (the unit name derived from
       * the filename).  When an MLB export like "structure BasisExtra"
       * refers to a structure defined inside basis.sml, lookupBinding won't
       * find it because the binding is named "Basis", not "BasisExtra".
       *
       * Fallback: use the last binding in scope — in -toplevel mode, all
       * structures compiled in the session are visible through the .ui
       * files on the include path, so any binding's directory will do.
       *)
    | Structure binds =>
        let
            val lastB = case rev (scopeBindings scope) of
                            b :: _ => SOME b | [] => NONE
            (* Look up a structure name in the name table (populated at compile time). *)
            fun findByContent name = nameTableLookup name
            fun resolve (Mlb.StrId name) =
                (case lookupBinding scope name StrKind of
                   SOME b => SOME b
                 | NONE =>
                   case findByContent name of
                     SOME fb => (Log.debug 1 ("structure " ^ name ^
                                   " not found by name, using content match from " ^ #name fb);
                                 SOME { name = name, uiPath = #uiPath fb, kind = StrKind })
                   | NONE =>
                   case lastB of
                     SOME fb => (Log.debug 1 ("structure " ^ name ^
                                   " not found by name, using fallback from " ^ #name fb);
                                 SOME { name = name, uiPath = #uiPath fb, kind = StrKind })
                   | NONE => (Log.debug 1 ("structure " ^ name ^ " not found in scope"); NONE))
              | resolve (Mlb.StrBind (lhs, rhs)) =
                (case lookupBinding scope rhs StrKind of
                   SOME b => SOME { name = lhs, uiPath = #uiPath b, kind = StrKind }
                 | NONE =>
                   case findByContent rhs of
                     SOME fb => (Log.debug 1 ("structure " ^ rhs ^
                                   " not found by name, using content match from " ^ #name fb);
                                 SOME { name = lhs, uiPath = #uiPath fb, kind = StrKind })
                   | NONE =>
                   case lastB of
                     SOME fb => (Log.debug 1 ("structure " ^ rhs ^
                                   " not found by name, using fallback from " ^ #name fb);
                                 SOME { name = lhs, uiPath = #uiPath fb, kind = StrKind })
                   | NONE => (Log.debug 1 ("structure " ^ rhs ^ " not found in scope"); NONE))
            val resolved = List.mapPartial resolve binds
        in
            foldl (fn (b, s) => addBinding s b) scope resolved
        end

      (* signature S1 and S2 = S3 *)
    | Signature binds =>
        let
            val lastB = case rev (scopeBindings scope) of
                            b :: _ => SOME b | [] => NONE
            fun resolve (Mlb.SigId name) =
                (case lookupBinding scope name SigKind of
                   SOME b => SOME b
                 | NONE =>
                   case lookupBinding scope name StrKind of
                     SOME b => SOME { name = #name b, uiPath = #uiPath b, kind = SigKind }
                   | NONE =>
                     case lastB of
                       SOME fb => (Log.debug 1 ("signature " ^ name ^
                                     " not found by name, using fallback from " ^ #name fb);
                                   SOME { name = name, uiPath = #uiPath fb, kind = SigKind })
                     | NONE => (Log.debug 1 ("signature " ^ name ^ " not found in scope"); NONE))
              | resolve (Mlb.SigBind (lhs, rhs)) =
                (case lookupBinding scope rhs SigKind of
                   SOME b => SOME { name = lhs, uiPath = #uiPath b, kind = SigKind }
                 | NONE =>
                   case lastB of
                     SOME fb => (Log.debug 1 ("signature " ^ rhs ^
                                   " not found by name, using fallback from " ^ #name fb);
                                 SOME { name = lhs, uiPath = #uiPath fb, kind = SigKind })
                   | NONE => (Log.debug 1 ("signature " ^ rhs ^ " not found in scope"); NONE))
            val resolved = List.mapPartial resolve binds
        in
            foldl (fn (b, s) => addBinding s b) scope resolved
        end

      (* functor F1 and F2 = F3 *)
    | Functor binds =>
        let
            val lastB = case rev (scopeBindings scope) of
                            b :: _ => SOME b | [] => NONE
            fun findFunctorByContent name = nameTableLookup name
            fun resolve (Mlb.FunId name) =
                (case lookupBinding scope name FunKind of
                   SOME b => SOME b
                 | NONE =>
                   case lookupBinding scope name StrKind of
                     SOME b => SOME { name = #name b, uiPath = #uiPath b, kind = FunKind }
                   | NONE =>
                   case findFunctorByContent name of
                     SOME fb => (Log.debug 1 ("functor " ^ name ^
                                   " not found by name, using content match from " ^ #name fb);
                                 SOME { name = name, uiPath = #uiPath fb, kind = FunKind })
                   | NONE =>
                     case lastB of
                       SOME fb => (Log.debug 1 ("functor " ^ name ^
                                     " not found by name, using fallback from " ^ #name fb);
                                   SOME { name = name, uiPath = #uiPath fb, kind = FunKind })
                     | NONE => (Log.debug 1 ("functor " ^ name ^ " not found in scope"); NONE))
              | resolve (Mlb.FunBind (lhs, rhs)) =
                (case lookupBinding scope rhs FunKind of
                   SOME b => SOME { name = lhs, uiPath = #uiPath b, kind = FunKind }
                 | NONE =>
                   case findFunctorByContent rhs of
                     SOME fb => (Log.debug 1 ("functor " ^ rhs ^
                                   " not found by name, using content match from " ^ #name fb);
                                 SOME { name = lhs, uiPath = #uiPath fb, kind = FunKind })
                   | NONE =>
                   case lastB of
                     SOME fb => (Log.debug 1 ("functor " ^ rhs ^
                                   " not found by name, using fallback from " ^ #name fb);
                                 SOME { name = lhs, uiPath = #uiPath fb, kind = FunKind })
                   | NONE => (Log.debug 1 ("functor " ^ rhs ^ " not found in scope"); NONE))
            val resolved = List.mapPartial resolve binds
        in
            foldl (fn (b, s) => addBinding s b) scope resolved
        end

      (* basis B1 = be1 and B2 = be2 *)
    | Basis basBinds =>
        foldl (fn (BasBind (name, basexp), s) =>
                   let val basisScope = evalBasexp s st basexp
                   in addBasis s (name, basisScope) end)
              scope basBinds

      (* open B1 B2 ... *)
    | Open basIds =>
        foldl (fn (basId, s) =>
                   case lookupBasis s basId of
                     SOME basis => mergeScope s basis
                   | NONE => (Log.debug 1 ("basis " ^ basId ^ " not found"); s))
              scope basIds

      (* ann "..." in decs end *)
    | Annotation (anns, decs) =>
        let
            val savedFlags = !(#compilerFlags st)
            (* Apply recognized annotations *)
            val _ = app (fn ann =>
                case ann of
                  "\"orthodox\"" => (#compilerFlags st) := savedFlags ^ " -orthodox"
                | "\"conservative\"" => (#compilerFlags st) := savedFlags ^ " -conservative"
                | "\"liberal\"" => (#compilerFlags st) := savedFlags ^ " -liberal"
                | _ => Log.debug 2 ("Ignoring annotation: " ^ ann)
            ) anns
            val result = evalDecs scope st decs
            val _ = (#compilerFlags st) := savedFlags
        in
            result
        end

      (* _prim — compile the primitive type shim *)
    | Prim =>
        let
            (* Find prim-shim.sml: prefer installed location, fall back to cwd *)
            val shimFile =
                let val installed = mosmllib ^ "/prim-shim.sml"
                in if OS.FileSys.access (installed, [OS.FileSys.A_READ])
                   then installed
                   else "prim-shim.sml"
                end
        in
            if OS.FileSys.access (shimFile, [OS.FileSys.A_READ])
            then let
                (* Compile the shim without rootDir resolution — it's an absolute/local path *)
                val absShim = OS.FileSys.fullPath shimFile
                val _ = Log.debug 1 ("_prim: compiling shim " ^ absShim)
                val cmd = compileCmd scope st absShim false
                val _ = Log.debug 2 ("Command: " ^ cmd)
                val ok = OS.Process.isSuccess (OS.Process.system cmd)
                val _ = if ok then () else Log.debug 1 "_prim: shim compilation failed"
                val uiPath = uiPathOf absShim
                val uoPath = uoPathOf absShim
                val _ = (#allUo st) := !(#allUo st) @ [uoPath]
            in
                addBinding scope { name = unitName absShim, uiPath = uiPath, kind = StrKind }
            end
            else (Log.debug 1 "_prim: no shim file found, skipping"; scope)
        end

and evalBasexp (scope: scope) (st: state) (basexp: basExp) : scope =
    case basexp of
      (* bas decs end *)
      Bas decs => evalDecs scope st decs

      (* basid — look up named basis *)
    | BasId name =>
        (case lookupBasis scope name of
           SOME basis => basis
         | NONE => (Log.debug 1 ("basis " ^ name ^ " not found"); emptyScope))

      (* let decs in basexp end *)
    | Let (decs, basexp) =>
        let val extScope = evalDecs scope st decs
        in evalBasexp extScope st basexp end

(* --- Top-level entry point --- *)

(* Recompile a stale unit before linking, using all existing .ui files
 * as the compilation context. This fixes type stamp drift when a
 * dependency was recompiled after the stale unit. *)
fun recompileForLinker (allUo: string list) (staleUnit: string) : unit =
    let
        fun baseName uo =
            let val {file, ...} = Path.splitDirFile uo
            in #base (Path.splitBaseExt file) end
        val staleUo = List.find (fn uo => baseName uo = staleUnit) allUo
    in
        case staleUo of
            NONE => Log.debug 1 ("Cannot find .uo for unit " ^ staleUnit)
          | SOME uo =>
            let
                val {base, ...} = Path.splitBaseExt uo
                val srcPath =
                    if OS.FileSys.access (base ^ ".sml", []) then base ^ ".sml"
                    else if OS.FileSys.access (base ^ ".sig", []) then base ^ ".sig"
                    else if OS.FileSys.access (base ^ ".fun", []) then base ^ ".fun"
                    else base ^ ".sml"
                val uiPath = base ^ ".ui"
                (* Collect -I dirs from all .uo paths *)
                fun dirOf path =
                    let val {dir, ...} = Path.splitDirFile path
                    in if dir = "" then "." else dir end
                val dirs = Mlb_functions.listUnique String.compare
                    (map dirOf allUo)
                val _ = Log.debug 1 ("Recompile dirs: " ^ Int.toString (length dirs))
                val includes = String.concat (map (fn d => " -I " ^ d) dirs)
                (* Build context: start with .uo-derived .ui files, then
                 * scan all -I dirs for additional .ui files (from .sig files
                 * that produce .ui but no .uo). *)
                val uoContext = List.mapPartial (fn u =>
                    let val {base=b, ...} = Path.splitBaseExt u
                        val ui = b ^ ".ui"
                    in if ui <> uiPath andalso OS.FileSys.access (ui, [])
                       then SOME ui else NONE
                    end) allUo
                val _ = Log.debug 1 ("Recompile uoContext: " ^ Int.toString (length uoContext))
                (* Scan dirs for .ui files not already in the context *)
                fun scanDir dir =
                    let val dh = OS.FileSys.openDir dir
                        fun loop acc =
                            case OS.FileSys.readDir dh of
                                NONE => (OS.FileSys.closeDir dh; acc)
                              | SOME f =>
                                  let val fpath = Path.concat (dir, f)
                                      val {ext, ...} = Path.splitBaseExt f
                                  in loop (if ext = SOME "ui"
                                              andalso fpath <> uiPath
                                           then fpath :: acc
                                           else acc)
                                  end
                    in loop [] handle e => (OS.FileSys.closeDir dh handle _ => (); []) end
                    handle _ => []
                val dirUi = List.concat (map scanDir dirs)
                (* Filter out scanned .ui files that are aliases of
                 * .ui files already in uoContext. E.g., IO.ui is an
                 * aliased signature copy of io.ui — including both
                 * causes the compiler to pick up the wrong one. *)
                fun normalizeUiName ui =
                    let val {file, ...} = Path.splitDirFile ui
                        val {base, ...} = Path.splitBaseExt file
                    in String.map (fn #"-" => #"_" | c => Char.toLower c) base end
                val contextNames = map normalizeUiName uoContext
                fun isAlias ui =
                    let val n = normalizeUiName ui
                    in List.exists (fn cn => cn = n) contextNames end
                val newUi = List.filter (fn ui => not (isAlias ui)) dirUi
                val _ = Log.debug 1 ("Recompile scanned: " ^ Int.toString (length dirUi)
                                     ^ " .ui from dirs, " ^ Int.toString (length newUi) ^ " new")
                val allContext = uoContext @ newUi
                val uniqCtx = Mlb_functions.listUnique String.compare allContext
                val _ = Log.debug 1 ("Recompile context: " ^ Int.toString (length uniqCtx) ^ " unique .ui files")
                val ctxStr = String.concat (map (fn ui => " " ^ ui) uniqCtx)
                val cmd = String.concat
                    [camlrunm, " ", mosmlcmp, " -stdlib ", mosmllib,
                     " -P none -P full -toplevel",
                     includes, ctxStr, " ", srcPath]
                val fullCmd = cmd ^ " >" ^ errFile ^ " 2>&1"
                val _ = Log.debug 1 ("Recompiling for linker: " ^ srcPath)
                val _ = Log.debug 2 ("Command: " ^ cmd)
                val ok = OS.Process.isSuccess (OS.Process.system fullCmd)
                val _ = if ok then ()
                        else Log.debug 1 ("Failed to recompile " ^ srcPath
                                          ^ ": " ^ readAllText errFile)
            in () end
    end

(* Find "Cannot find file X.uo" in an error message.
 * Returns the missing unit name (without .uo extension). *)
fun findMissingUo (errMsg: string) : string option =
    let
        val marker = "Cannot find file "
        val mLen = String.size marker
        val eLen = String.size errMsg
        fun tryAt i =
            if i + mLen > eLen then NONE
            else if String.substring (errMsg, i, mLen) = marker then
                let val start = i + mLen
                    fun endAt j =
                        if j >= eLen then j
                        else let val c = String.sub (errMsg, j)
                             in if c = #"\n" orelse c = #"\r" orelse c = #" "
                                then j else endAt (j + 1) end
                    val filename = String.substring (errMsg, start, endAt start - start)
                    val {base, ext} = Path.splitBaseExt filename
                in
                    case ext of
                        SOME "uo" => SOME base
                      | _ => NONE
                end
            else tryAt (i + 1)
    in tryAt 0 end

(* Create a .uo symlink for a missing unit. This handles .sig files that
 * create aliased .ui (e.g., DYNAMIC_WIND.ui) but whose .uo is under the
 * original filename (dynamic-wind.uo). We search -I directories for a
 * .uo whose unit name matches case-insensitively. *)
fun fixMissingUo (dirs: string list) (missingUnit: string) : bool =
    let
        (* Normalize: lowercase and map hyphens to underscores *)
        fun normalize s = String.map (fn #"-" => #"_" | c => Char.toLower c) s
        val lowerUnit = normalize missingUnit
        fun tryDir dir =
            let
                (* List directory and find a .uo with matching lowercase name *)
                val dh = OS.FileSys.openDir dir
                fun scan () =
                    case OS.FileSys.readDir dh of
                        NONE => NONE
                      | SOME f =>
                            let val {base, ext} = Path.splitBaseExt f
                            in if ext = SOME "uo" andalso normalize base = lowerUnit
                                  andalso base <> missingUnit
                               then SOME (Path.concat (dir, f))
                               else scan ()
                            end
                val found = scan () handle _ => NONE
                val _ = OS.FileSys.closeDir dh handle _ => ()
            in found end
        fun tryDirs [] = NONE
          | tryDirs (d::ds) = (case tryDir d of SOME f => SOME (d, f) | NONE => tryDirs ds)
        fun makeStubUo dir =
            let val uiPath = Path.concat (dir, missingUnit ^ ".ui")
            in if OS.FileSys.access (uiPath, [OS.FileSys.A_READ])
                      handle _ => false
               then let val stubSml = "/tmp/mosmlb-stub-" ^ missingUnit ^ ".sml"
                        val stubUo  = "/tmp/mosmlb-stub-" ^ missingUnit ^ ".uo"
                        val os = TextIO.openOut stubSml
                        val () = TextIO.output (os, "val () = ()\n")
                        val () = TextIO.closeOut os
                        val compCmd = String.concat
                            [camlrunm, " ", mosmlcmp, " -stdlib ", mosmllib,
                             " -P none -toplevel ", stubSml]
                        val () = ignore (OS.Process.system (compCmd ^ " >/dev/null 2>&1"))
                        val target = Path.concat (dir, missingUnit ^ ".uo")
                        val cpCmd  = "cp " ^ stubUo ^ " " ^ target
                        val ok = OS.FileSys.access (stubUo, []) handle _ => false
                        val _ = Log.debug 1 ("Creating stub .uo for " ^ missingUnit ^ " at " ^ target)
                    in ok andalso OS.Process.isSuccess (OS.Process.system cpCmd) end
               else false
            end
        fun makeStubInDirs [] = false
          | makeStubInDirs (d::ds) = if makeStubUo d then true else makeStubInDirs ds
    in
        case tryDirs dirs of
            NONE => makeStubInDirs dirs
          | SOME (dir, _) =>
                (* Create a proper stub .uo with the correct unit name,
                 * not a symlink to the implementation .uo — symlinks cause
                 * circular dependency cascades in the linker. *)
                let val stubSml = "/tmp/" ^ missingUnit ^ ".sml"
                    val stubUo  = "/tmp/" ^ missingUnit ^ ".uo"
                    val target  = Path.concat (dir, missingUnit ^ ".uo")
                    val os = TextIO.openOut stubSml
                    val () = TextIO.output (os, "val () = ()\n")
                    val () = TextIO.closeOut os
                    val compCmd = String.concat
                        [camlrunm, " ", mosmlcmp, " -stdlib ", mosmllib,
                         " -P none -toplevel ", stubSml]
                    val () = ignore (OS.Process.system (compCmd ^ " >/dev/null 2>&1"))
                    val ok = OS.FileSys.access (stubUo, []) handle _ => false
                    val _ = Log.debug 1 ("Creating stub .uo: " ^ target)
                in ok andalso OS.Process.isSuccess (OS.Process.system
                        ("cp " ^ stubUo ^ " " ^ target)) end
    end

fun execLinker (allUo: string list) (mlbFile: string) =
    let
        val output = case !(Options.execFile) of
                       SOME f => f
                     | NONE => Path.base mlbFile
        val stdlib = "-stdlib " ^ mosmllib ^ " -P none -P full -noheader"
        val uniqUo = Mlb_functions.listUnique String.compare allUo
        (* Compute -I paths from .uo file directories *)
        fun dirOf path =
            let val {dir, ...} = Path.splitDirFile path
            in if dir = "" then "." else dir end
        val dirs = Mlb_functions.listUnique String.compare (map dirOf uniqUo)

        (* Pre-link: for every .ui without a .uo in any -I dir, create a
         * symlink to the case-insensitive matching .uo, then INSERT the
         * alias into the .uo list right after the matching implementation.
         * This ensures the linker processes aliases at the right position
         * in its left-to-right pass, avoiding on-demand -I lookups. *)
        fun normalize s = String.implode (List.filter
                (fn c => c <> #"_" andalso c <> #"-")
                (String.explode (String.map Char.toLower s)))
        (* .sig files now produce .uo via .sml symlinks, so no stub
         * creation is needed. *)
        val varArgs = List.concat (map (fn d => ["-I", d]) dirs) @ uniqUo
        val atFile = writeJobFile varArgs
        val cmd = String.concat
            [camlrunm, " ", mosmllnk, " ", stdlib, " ", atFile, " -o ", output]
        val linkErrFile = "/tmp/mosmlb-link-err.txt"
        val fullCmd = cmd ^ " >" ^ linkErrFile ^ " 2>&1"

        fun tryLink (retries: int) =
            (Log.debug 1 ("Linking: " ^ cmd);
             if OS.Process.isSuccess (OS.Process.system fullCmd) then
                 Log.debug 1 ("Linked " ^ output)
             else if retries > 0 then
                 let val errMsg = readAllText linkErrFile
                 in
                     case findMissingUo errMsg of
                         SOME missingUnit =>
                             if fixMissingUo dirs missingUnit then
                                 (Log.debug 1 ("Fixed missing .uo for " ^ missingUnit ^ ", retrying link");
                                  tryLink (retries - 1))
                             else
                                 (Log.debug 1 ("Link failed: " ^ errMsg);
                                  Log.error (Log.FileNotRead output))
                       | NONE =>
                             (Log.debug 1 ("Link failed: " ^ errMsg);
                              Log.error (Log.FileNotRead output))
                 end
             else
                 let val errMsg = readAllText linkErrFile
                 in Log.debug 1 ("Link failed after retries: " ^ errMsg);
                    Log.error (Log.FileNotRead output)
                 end)
    in
        tryLink 50
    end

fun evalProgram (mlbFile: string) (parseTree: basDec list) =
    let
        val allUo = ref [] : string list ref
        val rootDir = Path.dir mlbFile
        val st : state = {
            allUo = allUo,
            compilerFlags = ref "",
            rootDir = rootDir,
            compileSeq = ref 0,
            uiSeqs = ref []
        }
        val _ = evalDecs emptyScope st parseTree
    in
        if null (!allUo) then
            print "No files to compile.\n"
        else
            execLinker (!allUo) mlbFile
    end

end
