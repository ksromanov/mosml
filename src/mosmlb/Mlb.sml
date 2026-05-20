(*
 * Types for mlb file.
 *
 * In contrast to MLton parsing, each .mlb file is scanned and
 * parsed separately. The included files are loaded only after AST
 * is fully created.
 *)

(* Errors appearing while loading and parsing .mlb files. *)
(* Failure to read, parse file or cycle in included .mlb *)
datatype fileError = ReadFailure | ParseFailure | CyclicDependency

datatype funBind = FunId of string | FunBind of string*string

datatype strBind = StrId of string | StrBind of string*string

datatype sigBind = SigId of string | SigBind of string*string

type basId = string

type annotation = string

datatype basDec = Basis of basBind list | Local of (basDec list)*(basDec list)
    | Open of basId list | Structure of strBind list | Signature of sigBind list 
    | Functor of funBind list | Path of includedFileType*string
    | Annotation of (string list)*(basDec list)
    | Prim
    and basBind = BasBind of (basId)*(basExp)
    and basExp = Bas of basDec list | BasId of basId | Let of (basDec list)*basExp
    (* The type of referenced file - .mlb, .sig, .sml, .fun. For some
     * .mlb files from MLton distribution we also need Unknown. The type
     * of the file is determined by lexer by extension of the file.
     * MLBFile - not yet loaded .mlb, LoadedMLBFile - parse tree of successfully
     * loaded file, FailedMLBFile - why failed loading of the file.
     *)
    and includedFileType = UnknownFile | MLBFile | LoadedMLBFile of basDec list
      | FailedMLBFile of fileError | SIGFile | SMLFile | FUNFile

(* Path variables, populated by -mlb-path-map and -mlb-path-var options.
 * No hardcoded defaults: callers must supply all needed variables. *)
val pathVariables : (string * string) list ref = ref []

(* Set or replace a single path variable. *)
fun setPathVar (name : string) (value : string) : unit =
    pathVariables :=
        (name, value) ::
        List.filter (fn (n, _) => n <> name) (!pathVariables)

(* Expand $(VAR) references inside a value string using current pathVariables. *)
fun expandPathVarRefs (s : string) : string =
    let
        fun loop [] acc = String.implode (List.rev acc)
          | loop (#"$" :: #"(" :: rest) acc =
              let
                  fun readVar [] var = (List.rev var, [])
                    | readVar (#")" :: t) var = (List.rev var, t)
                    | readVar (c :: t) var = readVar t (c :: var)
                  val (varChars, rest') = readVar rest []
                  val varName = String.implode varChars
                  val expansion =
                      case List.find (fn (n, _) => n = varName) (!pathVariables) of
                          SOME (_, v) => String.explode v
                        | NONE => String.explode ("$(" ^ varName ^ ")")
              in
                  loop rest' (List.rev expansion @ acc)
              end
          | loop (c :: rest) acc = loop rest (c :: acc)
    in
        loop (String.explode s) []
    end

(* Parse and load an mlb-path-map file.
 * Format: one entry per line, "VARIABLE value", comments start with #.
 * $(VAR) references in values are expanded using already-defined variables. *)
fun loadPathMap (file : string) : unit =
    let
        val ins = TextIO.openIn file
        fun processLine line =
            let
                val s = Substring.full line
                val s = Substring.dropl Char.isSpace s
            in
                if Substring.size s = 0
                   orelse Substring.sub (s, 0) = #"#"
                then ()
                else
                    let
                        val (nameSub, rest) = Substring.splitl (fn c => not (Char.isSpace c)) s
                        val name = Substring.string nameSub
                        val value = Substring.dropr Char.isSpace
                                        (Substring.dropl Char.isSpace rest)
                        val value = expandPathVarRefs (Substring.string value)
                    in
                        if name = "" then ()
                        else setPathVar name value
                    end
            end
        fun readLines () =
            case TextIO.inputLine ins of
                NONE => ()
              | SOME line => (processLine line; readLines ())
    in
        readLines ();
        TextIO.closeIn ins
    end
    handle IO.Io {name, ...} =>
        raise Fail ("Cannot read mlb-path-map file: " ^ name)

(* Parse a "NAME VALUE" string as from -mlb-path-var on the command line.
 * Expands $(VAR) references in value. *)
fun parsePathVar (s : string) : unit =
    let
        val sub = Substring.full s
        val sub = Substring.dropl Char.isSpace sub
        val (nameSub, rest) = Substring.splitl (fn c => not (Char.isSpace c)) sub
        val name = Substring.string nameSub
        val value = Substring.string (Substring.dropl Char.isSpace rest)
        val value = expandPathVarRefs value
    in
        if name = "" then raise Fail ("Invalid -mlb-path-var argument: " ^ s)
        else setPathVar name value
    end

(* Returns the value of a path variable by name. *)
fun pathVariable variable =
    case List.find (fn (name, _) => name = variable) (!pathVariables) of
        SOME (_, value) => value
      | NONE => raise Fail ("Unknown path variable: " ^ variable)

