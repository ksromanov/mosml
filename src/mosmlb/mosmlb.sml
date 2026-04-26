(**
 * ML Basis System frontend for MosML compiler.
 *)

(**
 * Prints parse tree into intermediate file. After that it reads
 * intermediate file into second Parse tree. Finally it compares
 * both trees. The name of intermediate file is generated randomly.
 *
 * @param initialAST the initial parse tree.
 *)
fun parsePrintParseTest initialAST =
    let
        val intermediateFile = OS.FileSys.tmpName ()
        val outStream = BasicIO.open_out intermediateFile
        val _ = Mlb_functions.printAST
            (fn str => BasicIO.output (outStream, str)) initialAST;
        val _ = BasicIO.close_out outStream
        val secondAST = Mlb_functions.loadSingleMLBFile intermediateFile
    in
        if initialAST <> secondAST then
            Log.report "test FAILED, parse trees are different"
        else
            Log.report "test PASSED, parse trees are equal"
    end

fun main () =
    let
        val _ = Options.readCommandLine ()
        val file = Option.valOf (!Options.mlbFile)
        val parseTree = Mlb_functions.loadMlbFileTree file
    in
        case !Log.debugLevel of
          Log.PrintParseTree => Mlb_functions.printAST print parseTree
        | Log.ReadPrintReadTest => parsePrintParseTest parseTree
        | _ => Eval.evalProgram file parseTree
    end

val _ = (main() before OS.Process.exit OS.Process.success)
        handle e => (print ("Error occurred:\n"^exnMessage e^"\n");
		     OS.Process.exit OS.Process.failure)
