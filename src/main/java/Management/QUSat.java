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


    public static void  main(String[] args)  {
        HashMap<String, ArrayList<String>> moduleValues = loadDefaults();
        GlobalParameters.setDefaults(moduleValues);
        ProblemGenerators.ProblemGenerator.setDefaults(moduleValues);
        Solvers.Solver.setDefaults(moduleValues);
        Frame.openFrame();
    }

    private static HashMap<String, ArrayList<String>> loadDefaults() {
        HashMap<String, ArrayList<String>> moduleValues = new HashMap<>();
        File file = Paths.get(System.getProperty("user.dir"), "src","main","resources","DefaultParameters.phs").toFile();
        try {
            InputStream input = new FileInputStream(file);
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(input));
            String line;
            ArrayList<String> values = null;
            while( (line = bufferedReader.readLine()) != null ) {
                line = line.trim();
                if(line.isEmpty() || line.startsWith("%"))  continue;
                if(!line.contains("=")) {
                    values = new ArrayList<>();
                    moduleValues.put(line.toLowerCase(), values);}
                else values.add(line);}}
        catch(Exception e) {
            System.out.println(e.getMessage());
            System.exit(1);}
        return moduleValues;
    }






}
