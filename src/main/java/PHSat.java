import Management.KVAnalyser;
import Management.KVParser;
import Management.SolverControl;

import java.io.*;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by Ohlbach on 02.09.2018.
 */
public class PHSat {
    private static KVParser kvParser = new KVParser("global", "problem", "series", "solver");

    private static StringBuffer errors = new StringBuffer();
    private static StringBuffer warnings = new StringBuffer();

    private static KVAnalyser kvAnalyser = new KVAnalyser(errors,warnings);

    private static SolverControl solverControl = new SolverControl(errors,warnings);

    static HashMap<String,Class> classMap = new HashMap<>();
    static {
        classMap.put("RandomGenerator", Generators.RandomClauseSetGenerator.class);
        classMap.put("RandomWalk", Solver.RandomWalk.RandomWalker.class );
    }




    private static void help(String[] args) {
        System.out.println("HELPhhh");

    }

    private static void parseCnfFile(String filename) {

    }


    private static  void parseInStream() {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String line;
        try {
            line = reader.readLine();
            if (line.startsWith("help")) {
                help(line.trim().split("(=,:, )+"));
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
        solverControl.solve(kvAnalyser);}
}
