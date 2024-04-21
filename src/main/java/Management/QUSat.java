package Management;

import Management.GIU.Frame;

import java.io.*;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;

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

    /** the name of the Default-file */
    private static String defaultFile = "DefaultParameters.phs";

    public static void  main(String[] args)  {
        HashMap<String, ArrayList<String>> moduleValues = loadDefaults();
        QuSatJob.setDefaults(moduleValues);
        GlobalParameters.setDefaults(moduleValues);
        ProblemGenerators.ProblemGenerator.setDefaults(moduleValues);
        Solvers.Solver.setDefaults(moduleValues);
        Frame.openFrame();
    }

    /** This method is used to load the default parameters from the default file into a HashMap.
     *
     * The file may contain groups of lines with strings parameter = value<br>
     * The groups are indicated by module names like 'global', 'logFrame' etc.<br>
     * The module names may have upper- and lower-case letters and blanks.<br>
     * The module names are normalized to lower-case letters and blanks are removed.
     *
     * @return A HashMap containing the default module values.
     */
    private static HashMap<String, ArrayList<String>> loadDefaults() {
        HashMap<String, ArrayList<String>> moduleValues = new HashMap<>();
        File file = Paths.get(System.getProperty("user.dir"), "src","main","resources",defaultFile).toFile();
        try {
            InputStream input = new FileInputStream(file);
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(input));
            String line;
            ArrayList<String> values = null;
            while( (line = bufferedReader.readLine()) != null ) {
                if(line.contains("%"))line = line.substring(0,line.indexOf('%'));
                line = line.trim();
                if(line.isEmpty())  continue;
                if(!line.contains("=")) {
                    values = new ArrayList<>();
                    moduleValues.put(line.toLowerCase().replaceAll("\\s+", ""), values);}
                else values.add(line);}}
        catch(Exception e) {
            System.out.println(e.getMessage());
            System.exit(1);}
        return moduleValues;
    }






}
