package Management;

import Management.GIU.Frame;

import java.io.File;
import java.nio.file.Paths;

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

    /** loads the default values and the starts a JFrame which allows one to set all the parameters and
     * start the QSat solvers.
     *
     * @param args no args necessary.
     */
    public static void  main(String[] args)  {
        StringBuilder errors = new StringBuilder();
        Frame.loadParameters(defaultFile, true);
        if(!errors.isEmpty()) {
            System.err.println("Errors while parsing the default values:\n" + errors.toString());}
        else Frame.openFrame();
    }







}
