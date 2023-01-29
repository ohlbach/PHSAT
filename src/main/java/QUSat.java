import Datastructures.Clauses.AllClauses.InitializerSimplifier;
import Generators.Generator;
import Management.Controller;
import Management.GlobalParameters;
import Utilities.KVParser;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorLife;
import Solvers.Solver;

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

/** This is the class with the main method for the QUSat system. <br>
 * Created by Ohlbach on 02.09.2018.
 *
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
            "src","main","resources","DefaultParameters.qsat").toString();

    private static final String homeDirectory = System.getenv("USERPROFILE");

    private static final String parameters = null;

    public static String jobname;

    public static GlobalParameters globalParameters  = null;
    public HashMap<String,Object>            initializeParameters  = null;
    public ArrayList<HashMap<String,Object>> problemParameters = null;
    public ArrayList<HashMap<String,Object>> solverParameters  = null;
    Controller controller = null;


    /** The main method can be called with or without arguments. <br>
     * If there are no arguments then the control parameters are read from System.in<br>
     * In this case the first line must either be a help command or a jobname.<br>
     * If the first argument is 'help' then help-strings are printed
     * - help global:           prints the global help strings<br>
     * - help initialize:       prints the initializer help strings<br>
     * - help [generator name]: prints the help strings of the generator<br>
     * - help [solver name]:    prints the help strings of the solver<br>
     * - help:                  prints all help strings.
     * <br>
     * In the other cases the default control parameters are first read from the defaultFile. <br>
     * The other parameters may overwirte the default parameters.<br>
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
    public static void  main(String[] args) {
        //args = new String[]{"help","global"};
        if(args.length == 0) {help(args); return;}
        KVParser kvParser = new KVParser("global", "problem", "initialize", "solver");
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

        StringBuilder errors   = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        globalParameters= new GlobalParameters(kvParser.get("global"),errors,warnings);
        ArrayList<Generator> generators = Generator.parseParameters(kvParser.get("generator"),globalParameters,errors,warnings);
}

    /** This method calls the help()-methods and prints the results.
     * - help global:           prints the global help strings<br>
     * - help initialize:       prints the initializer help strings<br>
     * - help [generator name]: prints the help strings of the generator<br>
     * - help [solver name]:    prints the help strings of the solver<br>
     * - help:                  prints all help strings.
     *
     * @param args either empty, or [help,name] where name is the name of a generator or solver.
     */
    private static void help(String[] args) {
        if(args.length > 1) {
            String name = args[1];
            if(name.equals("global"))       {System.out.println(GlobalParameters.help()); return;}
            if(name.equals("initialize"))   {System.out.println(InitializerSimplifier.helpInitializer()); return;}
            if(Generator.isGenerator(name)) {System.out.println(Generator.help(name));    return;}
            if(Solver.isSolver(name))       {System.out.println(Solver.help(name));       return;}
            System.out.println("Unknown name '"+ name + "'. The available names are:");
            System.out.println("Generators: " + Arrays.toString(Generator.generators));
            System.out.println("Solvers:    " + Arrays.toString(Solver.solvers));
            return;}
        System.out.println(GlobalParameters.help());
        System.out.println(Generator.help());
        System.out.println(InitializerSimplifier.helpInitializer());
        System.out.println(Solver.help());}





    /** analyses the solverParameters and turns them into sequences of objectParameters.
     * Since the input parameters may specify ranges, each single input parameter may expand to a sequence of parsed parameters*/
    private void analyseSolverParameters(ArrayList<HashMap<String,String>> solverInputParameters) {
        solverParameters = new ArrayList<>();
        Monitor errors = new MonitorLife(); Monitor warnings = new MonitorLife();
        for(HashMap<String,String> parameters : solverInputParameters) {
            String type = parameters.get("type");
            if(type == null) {errors.print(jobname,"No solver type specified.\n"); return;}
            ArrayList<HashMap<String,Object>> pars = Solver.parseParameters(type,parameters,errors,warnings);
            if(pars != null) {
                for(HashMap<String,Object> map :pars) {map.put("type",type);}
                if(pars.size() > 1) {
                    for(int i = 0; i < pars.size(); ++i) {pars.get(i).put("solverId",type+"_"+i);}}
                else {pars.get(0).put("solverId",type);}
                solverParameters.addAll(pars);}}}






}
