package Management;

import Utilities.KVParser;

import java.nio.file.Paths;
import java.util.HashMap;
import java.util.function.Supplier;

/** This is the class with the main method for the QUSat system. <br>
 * QUSat solves SAT-problems for quantified propositional logic. <br>
 * In this logic there are clauses with quantifiers over literals.
 * Examples: <br>
 *  >= 2 p,q,r,s means at least two of the literals must be true. <br>
 *  The allowed quantifiers are <br>
 *  %gt;= n  ... (at least n)   (%gt;= 1 ... are disjunctions in standard propositional logic) <br>
 *  %lt;= n  ... (at most n) <br>
 *  = n   ... (exactly n) <br>
 *  [n,m] ... (between n and m) <br>
 *
 *  In this system different solvers of different algorithm types can work in parallel and exchange
 *  intermediate results. Even different instances of the same solver type may work in parallel
 *  if they use different strategies or different seeds for random number generators.
 *  <br>
 *  The system can process several problems sequentially or in parallel and collect and print
 *  the corresponding statistics.
 *  <br>
 *  QUSat-problems can be generated, either randomly or with special algorithms (pigeonhole problems),
 *  or read from a cnf-file. The syntax of cnf-files is an extension of the syntax of cnf-files
 *  for standard propositional logic.
 */
public class QUSat {

    /** This is a file with default parameters for 'global' and 'solver'. */
    private static final String defaultFile = Paths.get(System.getProperties().get("user.dir").toString(),
            "src","main","resources","DefaultParameters.phs").toString();

    private static final String testFile = Paths.get(System.getProperties().get("user.dir").toString(),
            "src","main","resources","Test.cnf").toString();


    private static final String homeDirectory = System.getenv("USERPROFILE");



   private static final String parameters =
            "problem random\n"+
                    "predicates = 20\n" +
                    "cpRatio = 4\n"+
                    "length = 3\n"+
                    "seed = 1\n"+ // 20/4;  0 sat, 1 cont, 2 sat, 3 sat, 4 unsat, 5 sat, 6 sat, 7 sat, 8 wrong
            "global\n" +           //12/4: 8 wrong
                    "cnfFile = numbers\n"+
                    "logging = life\n"+
                    "showClauses = true\n"+
                    "monitor = life\n"+
                    "trackReasoning\n"+
           //"solver simplifier";
           "solver resolution\n";
                    //"mergeResolution = true\n";

    /*private static final String parameters =
            "problem cnfreader\n"+
                "files = " + testFile;*/


    public static GlobalParameters globalParameters  = null;

    ProblemDistributor problemDistributor = null;


    /** The main method can be called with or without arguments.
     * If there are no arguments then the control parameters are read from System.in<br>
     * In this case the first line must either be a help command or a jobname.<br>
     * If the first argument is 'help' then help-strings are printed
     * - help global:           prints the global help strings<br>
     * - help [generator name]: prints the help strings of the generator<br>
     * - help [solver name]:    prints the help strings of the solver<br>
     * - help:                  prints all help strings.
     * <br>
     * In the other cases the default control parameters are first read from the defaultFile. <br>
     * The other parameters may overwrite the default parameters.<br>
     * args[0] is the jobname (any string) and args[1]
     * must be a pathname relative to the homedirectory. The parameters are read from this file.<br>
     * If the pathname ends with .cnf then this is a clause file. <br>
     * The file is not read here, but in the corresponding generator.<br>
     * If only a .cnf file is given then all control parameters are read from a default file.
     * <br>
     * After all parameters are read, they are parsed and the generators and solvers are activated.
     *
     * @param args for the commands
     */
    public static void  main(String[] args)  {
        //args = new String[]{"help","walker"};
        args = new String[]{"string"};
        if(args.length == 0) {help(args); return;}
        KVParser kvParser = new KVParser("global", "problem", "solver");
        kvParser.parseFile(defaultFile);
        switch(args[0]) {
            case "help": help(args); return;
            case "main": for(int i = 1; i < args.length; ++i) kvParser.addLine(args[i]); break;
            case "file": kvParser.parseFile(Paths.get(homeDirectory,args[1]).toString()); break; // may produce an exception and stop;
            case "in":   kvParser.parseStream(System.in); break; // may produce an exception and stop;
            case "string": { // for test purposes
                if(parameters == null) {System.out.println("No parameters specified in QUSat class"); return;}
                kvParser.parseString(parameters);
                break;}
            default:
                System.out.println("Unknown keyword in main method: "+ args[0]+ "\nIt should be one of:");
                System.out.println("help, main, file, in, string");
                return;}
        QuSatJob quSatJob = new QuSatJob(kvParser);
        quSatJob.solveProblems();}

    /** collects the help functions */
    private static final HashMap<String, Supplier<String>> helpers = new HashMap<>();

    static {
        helpers.put("parameters", QUSat::help);
        helpers.put("global",     Management.GlobalParameters::help);
        helpers.put("generator",  ProblemGenerators.ProblemGenerator::help);
        helpers.put("cnfreader",  ProblemGenerators.CNFReader::help);
        helpers.put("random",     ProblemGenerators.RandomClauseSetGenerator::help);
        helpers.put("pigeonhole", ProblemGenerators.PigeonHoleGenerator::help);
        helpers.put("string",     ProblemGenerators.StringClauseSetGenerator::help);
        helpers.put("walker",     Solvers.Walker.Walker::help);
        helpers.put("backtracker", Solvers.Backtracker.Backtracker::help);
    }
    /** for displaying the helpers */
    private static final String helperKeys = "parameters, global, generator (cnfreader, random, pigeonhole, string) ";

    /** This method calls the help()-methods and prints the results.
     *
     * @param args either  [help] or [help,name] where name is the name of a helper.
     */
    private static void help(String[] args){
        if(args.length > 1) {
            Supplier<String> helper = helpers.get(args[1]);
            if(helper == null) {
                System.out.println("Unknown helper. The known helpers are:\n");
                System.out.println(helperKeys);}
            else System.out.println(helper.get());}
        else {
            System.out.println("The known helpers are:\n");
            System.out.println(helperKeys);}}

    /** returns a string with explanations about the parameters.
     *
     * @return a string with explanations about the parameters.
     */
    private static String help() {
        return "The parameters are specified as key-value pairs, grouped in blocks.\n"+
                "Each block has a name and possibly a parameter.\n"+
                "There are the following blocks:\n"+
                "   global                   (for global parameters)\n"+
                "   problem [generator-name] (for specifying the problem source)\n"+
                "   solver  [solver-name]    (for specifying the solvers)\n"+
                "All blocks may occur multiple times.\n"+
                "The parameters in global blocks are overwritten by later global blocks.\n"+
                "Several problem blocks specify several sources.\n"+
                "Several solver blocks specify that these solvers should work in parallel.\n"+
                "Even several solvers of the same type can work in parallel at the same problem.\n\n"+
                "There is a file DefaultParameters.phs which is always loaded for specifying default parameters.\n"+
                "Example:\n"+
                "global\n" +
                "   jobname     = Test\n" +
                "   parallel    = 1\n" +
                "   monitor     = life\n" +
                "   logging     = life\n" +
                "   simplifier  = true\n" +
                "   showClauses = true\n"+
                "\n"+
                "problem random\n" +
                "      predicates = 100\n" +
                "      cpRatio    = 4\n" +
                "      length     = 2-3\n" +
                "      seed       = 1\n"+
                "\n"+
                " solver walker\n" +
                "     flips = 50000\n" +
                "     monitor = 0\n" +
                "     seed = 1";
    }
}
