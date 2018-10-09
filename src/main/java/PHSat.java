import Management.KVAnalyser;
import Management.KVParser;
import Solvers.RandomWalker;
//import Management.SolverController;

import java.io.*;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by Ohlbach on 02.09.2018.
 */
public class PHSat {
    private static KVParser kvParser = new KVParser("global", "problem", "solver");

    private static StringBuffer errors = new StringBuffer();
    private static StringBuffer warnings = new StringBuffer();

    private static KVAnalyser kvAnalyser = new KVAnalyser(errors,warnings);

    //private static SolverController solverControl = new SolverController(errors,warnings);

    static HashMap<String,Class> classMap = new HashMap<>();
    static {
        classMap.put("random", Generators.RandomClauseSetGenerator.class);
        classMap.put("file", Generators.CNFReader.class);
        classMap.put("pidgeonhole", Generators.PidgeonHoleGenerator.class);
        classMap.put("walker", RandomWalker.class );
    }


    /** This method calls the help()-methods of the generators and solvers, and prints the results.
     *
     * @param args either empty, or [help,name] where name is the name of a generator or solver.
     */
    private static void help(String[] args) {
        try {
        if(args.length > 1) {
            Class clazz = classMap.get(args[1]);
            if (clazz == null) {
                System.out.println("Unknown class " + args[1]+". The available names are:\n");
                for(String key : classMap.keySet()) {
                    System.out.printf(key + ", ");}
                System.out.println("");
                return;}
            else {
                Method help = clazz.getMethod("help");
                System.out.println((String)help.invoke(null));}
        }
        else {
            for(Map.Entry<String,Class> entry : classMap.entrySet()) {
                Method help = entry.getValue().getMethod("help");
                System.out.println((String)help.invoke(null));
                System.out.println("");}}
        }
        catch(Exception ex) {ex.printStackTrace();}
    }

    private static void parseCnfFile(String filename) {

    }

    private static  void parseInStream() {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String line;
        try {
            line = reader.readLine();
            if (line.startsWith("help")) {
                help(line.trim().split("\\s*(=|:| )\\s*"));
                return;}
            kvParser.addLine(line);
            kvParser.parseStream(System.in);}
        catch(IOException ex) {
            ex.printStackTrace();
            return;}}


    public static void  main(String[] args) {
        if(args.length > 0) {
            if(args[0].equals("help")) {help(args); return;}
            if(args.length == 1 && args[0].endsWith(".cnf")) {parseCnfFile(args[0]); return;}
            kvParser.parseFile(args[0]);}
        else {parseInStream();}
        kvAnalyser.analyse(kvParser,classMap);
        if(errors.length() > 0) {
            System.out.println("Errors:\n");
            System.out.println(errors);}
        if(warnings.length() > 0) {
            System.out.println("Warnings:\n");
            System.out.println(warnings);}
        if(errors.length() >0 ) {return;}

        errors = new StringBuffer();
        warnings = new StringBuffer();
      //  solverControl.solve(kvAnalyser);
    }
}
