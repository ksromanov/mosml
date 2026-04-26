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
  rootDir: string
}

val emptyScope = Scope { bindings = [], bases = [] }

fun scopeBindings (Scope s) = #bindings s
fun scopeBases (Scope s) = #bases s

(* --- Scope operations --- *)

fun addBinding (s: scope) (b: binding) : scope =
    Scope { bindings = scopeBindings s @ [b], bases = scopeBases s }

fun addBasis (s: scope) (name: string, basis: scope) : scope =
    Scope { bindings = scopeBindings s, bases = scopeBases s @ [(name, basis)] }

fun toLower s = String.map Char.toLower s

fun lookupBinding (s: scope) (name: string) (kind: bindKind) : binding option =
    let
        val lname = toLower name
        fun matches (b: binding) =
            (#name b = name orelse toLower (#name b) = lname)
            andalso #kind b = kind
        fun matchesAnyKind (b: binding) =
            #name b = name orelse toLower (#name b) = lname
    in
        case List.find matches (rev (scopeBindings s)) of
          SOME b => SOME b
        | NONE => List.find matchesAnyKind (rev (scopeBindings s))
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

(* Build the compiler command for a source file *)
fun compileCmd (scope: scope) (st: state) (file: string) : string =
    let
        val stdlib = "-stdlib ../mosmllib"
        val pervasive = "-P none -P full"
        val flags = !(#compilerFlags st)
        val dirs = includeDirs (scopeBindings scope)
        val includes = String.concat (map (fn d => " -I " ^ d) dirs)
        val context = String.concat
            (map (fn b => " " ^ #uiPath b) (scopeBindings scope))
    in
        String.concat
            ["../camlrunm ../mosmlcmp ", stdlib, " ", pervasive,
             " -toplevel",
             (if flags = "" then "" else " " ^ flags),
             includes, context, " ", file]
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

(* Compile a source file, return the binding it produces *)
fun compileSource (scope: scope) (st: state) (ft: includedFileType, file: string)
    : binding =
    let
        val absFile = resolvePath st file
        val cmd = compileCmd scope st absFile
        val _ = Log.debug 1 ("Compiling: " ^ absFile)
        val _ = Log.debug 2 ("Command: " ^ cmd)
        val ok = OS.Process.isSuccess (OS.Process.system cmd)
        val _ = if ok then ()
                else Log.error (Log.FileNotRead absFile)
        val uo = uoPathOf absFile
        val _ = (#allUo st) := !(#allUo st) @ [uo]
    in
        { name = unitName absFile,
          uiPath = uiPathOf absFile,
          kind = kindOfFile ft }
    end

(* --- MLB evaluation --- *)

fun evalDecs (scope: scope) (st: state) (decs: basDec list) : scope =
    foldl (fn (d, s) => evalBasdec s st d) scope decs

and evalBasdec (scope: scope) (st: state) (dec: basDec) : scope =
    case dec of

      (* Source file: compile with current scope as context *)
      Path (ft as SMLFile, file) => addBinding scope (compileSource scope st (ft, file))
    | Path (ft as SIGFile, file) => addBinding scope (compileSource scope st (ft, file))
    | Path (ft as FUNFile, file) => addBinding scope (compileSource scope st (ft, file))

      (* Loaded MLB file: evaluate its declarations in the .mlb's directory context *)
    | Path (LoadedMLBFile decs, mlbPath) =>
        let val dir = Path.dir mlbPath
            val nestedSt = { allUo = #allUo st,
                             compilerFlags = #compilerFlags st,
                             rootDir = if dir = "" then #rootDir st else dir }
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
    | Structure binds =>
        let
            fun resolve (Mlb.StrId name) =
                (case lookupBinding scope name StrKind of
                   SOME b => SOME b
                 | NONE => (Log.debug 1 ("structure " ^ name ^ " not found in scope"); NONE))
              | resolve (Mlb.StrBind (lhs, rhs)) =
                (case lookupBinding scope rhs StrKind of
                   SOME b => SOME { name = lhs, uiPath = #uiPath b, kind = StrKind }
                 | NONE => (Log.debug 1 ("structure " ^ rhs ^ " not found in scope"); NONE))
            val resolved = List.mapPartial resolve binds
        in
            foldl (fn (b, s) => addBinding s b) scope resolved
        end

      (* signature S1 and S2 = S3 *)
    | Signature binds =>
        let
            fun resolve (Mlb.SigId name) =
                (case lookupBinding scope name SigKind of
                   SOME b => SOME b
                 | NONE =>
                   (* Signatures often share .ui with their structure *)
                   case lookupBinding scope name StrKind of
                     SOME b => SOME { name = #name b, uiPath = #uiPath b, kind = SigKind }
                   | NONE => (Log.debug 1 ("signature " ^ name ^ " not found in scope"); NONE))
              | resolve (Mlb.SigBind (lhs, rhs)) =
                (case lookupBinding scope rhs SigKind of
                   SOME b => SOME { name = lhs, uiPath = #uiPath b, kind = SigKind }
                 | NONE => (Log.debug 1 ("signature " ^ rhs ^ " not found in scope"); NONE))
            val resolved = List.mapPartial resolve binds
        in
            foldl (fn (b, s) => addBinding s b) scope resolved
        end

      (* functor F1 and F2 = F3 *)
    | Functor binds =>
        let
            fun resolve (Mlb.FunId name) =
                (case lookupBinding scope name FunKind of
                   SOME b => SOME b
                 | NONE =>
                   case lookupBinding scope name StrKind of
                     SOME b => SOME { name = #name b, uiPath = #uiPath b, kind = FunKind }
                   | NONE => (Log.debug 1 ("functor " ^ name ^ " not found in scope"); NONE))
              | resolve (Mlb.FunBind (lhs, rhs)) =
                (case lookupBinding scope rhs FunKind of
                   SOME b => SOME { name = lhs, uiPath = #uiPath b, kind = FunKind }
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

      (* _prim — no-op for Moscow ML *)
    | Prim => scope

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

fun execLinker (allUo: string list) (mlbFile: string) =
    let
        val output = case !(Options.execFile) of
                       SOME f => f
                     | NONE => Path.base mlbFile
        val stdlib = "-stdlib ../mosmllib -P none -P full -noheader"
        val uniqUo = Mlb_functions.listUnique String.compare allUo
        (* Compute -I paths from .uo file directories *)
        fun dirOf path =
            let val {dir, ...} = Path.splitDirFile path
            in if dir = "" then "." else dir end
        val dirs = Mlb_functions.listUnique String.compare (map dirOf uniqUo)
        val includes = String.concat (map (fn d => " -I " ^ d) dirs)
        val objects = String.concat (map (fn u => " " ^ u) uniqUo)
        val cmd = String.concat
            ["../camlrunm ../mosmllnk ", stdlib, includes, objects, " -o ", output]
    in
        Log.debug 1 ("Linking: " ^ cmd);
        if OS.Process.isSuccess (OS.Process.system cmd) then
            Log.debug 1 ("Linked " ^ output)
        else
            Log.error (Log.FileNotRead output)
    end

fun evalProgram (mlbFile: string) (parseTree: basDec list) =
    let
        val allUo = ref [] : string list ref
        val rootDir = Path.dir mlbFile
        val st : state = {
            allUo = allUo,
            compilerFlags = ref "",
            rootDir = rootDir
        }
        val _ = evalDecs emptyScope st parseTree
    in
        if null (!allUo) then
            print "No files to compile.\n"
        else
            execLinker (!allUo) mlbFile
    end

end
