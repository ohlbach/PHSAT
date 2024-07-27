package Management;

import Management.GIU.Frame;
import ProblemGenerators.ProblemGenerator;
import Solvers.Solver;

import java.io.*;
import java.nio.file.Paths;
import java.util.ArrayList;

/** This is the class with the main method for the QUSat system. <br>
 * QUSat solves SAT-problems for quantified propositional logic. <br>
 * In this logic there are clauses with quantifiers over predicates.
 * Examples: <br>
 *  >= 2 p,q,r,s means at least two of the predicates must be true. <br>
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

    /** the name of the Default-file */
    private static String defaultFileName = "DefaultParameters.txt";

    public static File defaultFile = Paths.get(System.getProperty("user.dir"), "src","main","resources",defaultFileName).toFile();

    public static Parameters globalParams = GlobalParameters.makeGlobalParameters();
    public static ArrayList<Parameters> generatorParams = ProblemGenerator.makeParameters();
    public static ArrayList<Parameters> solverParams = Solver.makeParameters();

    /** loads the parameters for the QSat-Solver from the given file.
     *
     * @param file a file with parameters for the QSat solver
     * @param errors for appending error messages
     * @return true if the parameters were loaded successfully
     */
    public static boolean loadParameters(File file, StringBuilder errors) {
        try{
            InputStream input = new FileInputStream(file);
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(input));
            String line = bufferedReader.readLine();
            if(!line.startsWith("Global Parameters")) {
                errors.append("'"+line + "' does not start with 'Global Parameters'\n");}
            globalParams.loadParameters(bufferedReader,errors);
            while(!(line = bufferedReader.readLine()).startsWith("END")) {
                if(line.isEmpty()) {while((line = bufferedReader.readLine()).isEmpty()) {}}
                if(line.startsWith("Generator")) {
                    line = bufferedReader.readLine();
                    boolean found = false;
                    for(Parameters p: generatorParams) {
                        if(line.startsWith(p.name)) {found = true; p.loadParameters(bufferedReader,errors);}}
                    if(!found) errors.append("unknown generator " + line);}
                if(line.startsWith("Solver")) {
                    line = bufferedReader.readLine();
                    boolean found = false;
                    for(Parameters p: solverParams) {
                        if(line.startsWith(p.name)) {found = true; p.loadParameters(bufferedReader,errors);}}
                    if(!found) errors.append("unknown solver " + line);}}
            input.close();}
        catch(Exception e) {errors.append(e.getMessage());}
        return errors.isEmpty();}

    /** loads the default values and the starts a JFrame which allows one to set all the parameters and
     * start the QSat solvers.
     *
     * @param args no args necessary.
     */
    public static void  main(String[] args)  {
        StringBuilder errors = new StringBuilder();
        boolean withFrame = false;
        if(withFrame) {
            Frame.loadParameters(defaultFile, true);
            if(!errors.isEmpty()) {
            System.err.println("Errors while parsing the default values:\n" + errors.toString());}
            else Frame.openFrame();}
        else {
            if(!loadParameters(defaultFile,errors)) {
                System.out.println(errors);}
            else {
                QuSatJob quSatJob = new QuSatJob(globalParams,generatorParams,solverParams);
                quSatJob.solveProblems();
            }
        }
    }







}
