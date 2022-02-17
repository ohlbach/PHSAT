import Datastructures.Clauses.AllClauses.InitializerSimplifier;
import Generators.Generator;
import Management.Controller;
import Management.GlobalParameters;
import Management.KVParser;
import Management.Monitor;
import Solvers.Solver;

import java.io.*;
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
 *  QUSat-problems can be generated, either randomly or with special algorithms (pigeon hole problems),
 *  or read from a cnf-file. The syntax of cnf-files is an extension of the syntax of cnf-files
 *  for standard propositional logic.
 */
public class QUSat {

    /** This is a file with default parameters for 'global' and 'solver'. */
    private static final File DefaultFile = Paths.get(System.getProperties().get("user.dir").toString(),
            "src","main","resources","DefaultParameters.phs").toFile();

    public String jobname;
    private Monitor errors;
    private Monitor warnings;

    public GlobalParameters                  globalParameters  = null;
    public HashMap<String,Object>            initializeParameters  = null;
    public ArrayList<HashMap<String,Object>> problemParameters = null;
    public ArrayList<HashMap<String,Object>> solverParameters  = null;
    Controller controller = null;


    /** The main method can be called with or without arguments. <br>
     * If the first argument is 'help' then help-strings are printed
     * - help global:           prints the global help strings<br>
     * - help initialize:       prints the initializer help strings<br>
     * - help [generator name]: prints the help strings of the generator<br>
     * - help [solver name]:    prints the help strings of the solver<br>
     * - help:                  prints all help strings.
     * <br>
     * In the other case args[0] is the jobname (any string) and args[1]
     * must be a filename. The parameters are read from this file.<br>
     * If the filename ends with .cnf then this is a clause file. <br>
     * The file is not read here, but in the corresponding generator.<br>
     * If only a .cnf file is given then all control parameters are read from a default file.
     * <br>
     * After all parameters are read, they are parsed and the generators and solvers are activated.
     *
     * @param args for the commands
     */
    public static void  main(String[] args) {
        //args = new String[]{"help","global"};
        //args = new String[]{Utilities.resourceFile("Purity.cnf")};
        if(args.length > 0 && args[0].trim().equals("help")) {help(args); return;}

        if(args.length < 2) {
            System.out.println("Not enough arguments. At least a jobname is necessary");
            return;}

        QUSat quSat = new QUSat(args);
        if(quSat.controller == null) return;
        long start = System.nanoTime();
        quSat.controller.solveProblems();
        long end = System.nanoTime();
        quSat.globalParameters.logstream.println("Solver Time: " + (float)(end-start)/1000.0 + " ms");
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


    /** Constructs a QUSat object, which reads and parses the arguments.
     * Finally, it creates a Controller, which can control the entire processing of the problem.
     * args[0] is the jobname (any string) and args[1]
     * must be a filename. The parameters are read from this file.<br>
     * If the filename ends with .cnf then this is a clause file. <br>
     * The file is not read here, but in the corresponding generator.<br>
     * If only a .cnf file is given then all control parameters are read from a default file.
     *
     * @param args for the commands
     */
    private QUSat(String[] args) {
        jobname = args[0];
        KVParser kvParser = new KVParser("global", "problem", "initialize", "solver");
        if(!readParameters(args, kvParser)) return;
        errors   = new Monitor("Parameter Errors",null,"mixed");
        warnings = new Monitor("Parameter Warnings",null,"mixed");

        ArrayList<HashMap<String,String>> globalParameterList = kvParser.get("global");
        if(globalParameterList != null && globalParameterList.size() > 1) {
            warnings.print("Global Parameters",
                    "There should be only one set. The superfluous sets are ignored.");}
        ArrayList<HashMap<String,String>> initializeParameterList = kvParser.get("initialize");
        if(initializeParameterList != null && initializeParameterList.size() > 1) {
            warnings.print("Initialize Parameters",
                    "There should be only one set. The superfluous sets are ignored.");}

        analyseParameters(((globalParameterList != null && !globalParameterList.isEmpty()) ?
                    globalParameterList.get(0) : null),
                ((initializeParameterList != null && !initializeParameterList.isEmpty()) ?
                    initializeParameterList.get(0) : null),
                kvParser.get("problem"),
                kvParser.get("solver"));
        if(errors.filled) {
            errors.flush();
            warnings.flush();
            return;}      // parameters have errors. Stop the process.
        controller = new Controller(jobname,globalParameters,initializeParameters,problemParameters, solverParameters);
    }



    /** reads the parameters for the current job and fills the kvParser
     * If args.length = 1 then the parameters are read from system.in <br>
     * Otherwise args[1] must be a filename. The parameters are read from this file.<br>
     * If the filename ends with .cnf then this is a clause file. <br>
     * The file is not read here, but in the corresponding generator.<br>
     * If only a .cnf file is given then all control parameters are read from a default file.
     *
     * @param args     the input strings
     * @param kvParser a key-value parser
     * @return true    if it was not a help command
     */
    private static boolean readParameters(String[] args, KVParser kvParser) {
        if(args.length == 2) {
            if(args[1].endsWith(".cnf")) {
                kvParser.addLine("problem");
                kvParser.addLine("type = file");
                kvParser.addLine("file = " + args[0]);}
            else{kvParser.parseFile(args[1]);} // may produce an exception and stop
            readDefaults(kvParser);            // may produce an exception and stop
            return true;}
        if(!parseInStream(kvParser)) return false;
        readDefaults(kvParser);
        return true;}

    /** analyses the input specifications and turns them into internal data structures.
     */
    public void analyseParameters(
            HashMap<String,String> globalInputParameters,
            HashMap<String,String> initializeInputParameters,
            ArrayList<HashMap<String,String>> problemInputParameters,
            ArrayList<HashMap<String,String>> solverInputParameters) {
        if(globalInputParameters == null) {globalParameters = new GlobalParameters();} // default parameters
        else{globalParameters = new GlobalParameters(globalInputParameters,errors,warnings);}
        initializeParameters = InitializerSimplifier.parseParametersInitializer(initializeInputParameters,errors,warnings);
        if(problemInputParameters == null) {errors.print(jobname,"No problems specified.\n");}
        else {analyseProblemParameters(problemInputParameters);}
        if(solverInputParameters == null) {errors.print(jobname,"No solvers specified.\n");}
        else {analyseSolverParameters(solverInputParameters);}}



    /** analyses the problemParameters and turns them into sequences of objectParameters.
     * Since the input parameters may specify ranges, each single input parameter may expand to a sequence of parsed parameters*/
    private void analyseProblemParameters(ArrayList<HashMap<String,String>> problemInputParameters) {
        problemParameters = new ArrayList<>();
        for(HashMap<String,String> parameters : problemInputParameters) {
            String type = parameters.get("type");
            if(type == null) {
                errors.print("Problem Parameter",
                        "No problem type specified\n"+ parameters);
                continue;}
            ArrayList<HashMap<String,Object>> pars = Generator.parseParameters(type,parameters,errors,warnings);
            if(pars != null) {
                for(HashMap<String,Object> map :pars) {map.put("type",type);}
                problemParameters.addAll(pars);}}}

    /** analyses the solverParameters and turns them into sequences of objectParameters.
     * Since the input parameters may specify ranges, each single input parameter may expand to a sequence of parsed parameters*/
    private void analyseSolverParameters(ArrayList<HashMap<String,String>> solverInputParameters) {
        solverParameters = new ArrayList<>();
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




    /** The method reads the specification from System.in and parses it with the kvParser.
     * An IOException causes the system to stop.
     *
     * @param kvParser for parsing the specification
     * @return true if it was no help command and the parsing succeeded
     */
    private static boolean parseInStream(KVParser kvParser) {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String line;
        try {  // check if it is a help-command
            line = reader.readLine();
            if (line.startsWith("help")) {
                help(line.trim().split("\\s*[=,: ]\\s*"));
                return false;}
            kvParser.addLine(line);          // it was no help command.
            kvParser.parseStream(System.in); // the rest must be parsed.
            return true;}
        catch(IOException ex) {
            ex.printStackTrace();
            System.exit(1);}
        return false;}


    /** This method reads missing parameter types, 'global' and 'solver' from a default file.
     * The missing parameters are added to the kvParser's data.
     *
     * @param kvParser the parser with possibly missing 'global' and 'solver' parameters.
     */
    private static void readDefaults(KVParser kvParser) {
        if(!kvParser.get("global").isEmpty() &&  !kvParser.get("solver").isEmpty()) {return;}
        KVParser defaultParser = new KVParser("global", "problem", "initialize", "solver");
        defaultParser.parseFile(DefaultFile.getAbsolutePath());
        if(kvParser.get("global").isEmpty())     {kvParser.set("global",defaultParser.get("global"));}
        if(kvParser.get("solver").isEmpty())     {kvParser.set("solver",defaultParser.get("solver"));}
        if(kvParser.get("initialize").isEmpty()) {kvParser.set("initialize",defaultParser.get("initialize"));}}

}
