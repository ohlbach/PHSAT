import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by Ohlbach on 02.09.2018.
 */
public class PHSat {
    private static StringBuffer infos = new StringBuffer();
    private static HashMap<String,String> globalParameters            = new HashMap<>();
    private static ArrayList<HashMap<String,String>> problemSingle    = new ArrayList<>();
    private static ArrayList<HashMap<String,String>> problemSeries    = new ArrayList<>();
    private static ArrayList<HashMap<String,String>> solverParameters = new ArrayList<>();

    private static HashMap<String,Object> globalControl               = new HashMap<>();
    private static ArrayList<HashMap<String,Object>> problemControl   = new ArrayList<>();
    private static ArrayList<HashMap<String,Object>> solverControl    = new ArrayList<>();

    private static StringBuffer errors = new StringBuffer();
    private static StringBuffer warnings = new StringBuffer();

    private static HashMap<String,String> parameterMap = null;

    static void parseParameters(String line, String[] parameters) {
        if(parameters.length == 1) {parameterMap.put(parameters[0],"true");}
        else {parameterMap.put(parameters[0],parameters[1]);}}

    static Integer parseInteger(String place,String value) {
        try{return Integer.parseInt(value);}
        catch(NumberFormatException ex) {errors.append(place+": " + value + " is no integer.\n");}
        return null;}

    static void prepareProblems()  {
        prepareGlobalParameters();
        try {prepareSingleParameters();
            prepareSeriesParameters();
            prepareSolverParameters();}
        catch (Exception e) {
            e.printStackTrace();
            System.exit(0);}}



    static void prepareGlobalParameters() {
        globalControl.put("parallel",(Integer)0);  // problems are solved sequentially.
        for(Map.Entry<String,String> entry : globalParameters.entrySet()) {
            String key = entry.getKey();
            switch(key) {
                case "parallel":
                    String value = entry.getValue();
                    if(value.equals("true")) {globalControl.put("parallel",Runtime.getRuntime().availableProcessors()); break;}
                    Integer par = parseInteger("parallel", value);
                    if(par != null) {globalControl.put("parallel",par);}
                    break;
                case "logfile":
                    break;
                default: warnings.append("Unknown global parameter: " + key);
            }}}

    static HashMap<String,Class> classMap = new HashMap<>();
    static {
        classMap.put("Random", Generators.RandomClauseSetGenerator.class);
        classMap.put("RandomWalk", Solver.RandomWalk.RandomWalker.class );
    }

    static void prepareSingleParameters() throws Exception {
        for(HashMap<String,String> parameters :  problemSingle) {
            String type = parameters.get("problem");
            Class generatorClass = classMap.get(type);
            if(generatorClass == null) {errors.append("Unknown generator type: " + type); continue;}
            Method parser = generatorClass.getMethod("parseSingleParameters", HashMap.class, StringBuffer.class,StringBuffer.class);
            problemControl.add ((HashMap<String,Object>)parser.invoke(parameters,errors,warnings));}}


    static void prepareSeriesParameters() throws Exception{
        for(HashMap<String,String> parameters :  problemSeries) {
            String type = parameters.get("series");
            Class generatorClass = classMap.get(type);
            if(generatorClass == null) {errors.append("Unknown generator type: " + type); continue;}
            Method parser = generatorClass.getMethod("parseSeriesParameters", HashMap.class, StringBuffer.class,StringBuffer.class);
            problemControl.addAll ((ArrayList<HashMap<String,Object>>)parser.invoke(parameters,errors,warnings));}}

    static void prepareSolverParameters() throws Exception{
        for(HashMap<String,String> parameters :  solverParameters) {
            String[] type = parameters.get("solver").split("\\s+");
            Class solverClass = classMap.get(type[0]);
            if(solverClass == null) {errors.append("Unknown solver type: " + type); continue;}
            Method parser = solverClass.getMethod("parseParameters", HashMap.class, StringBuffer.class,StringBuffer.class);
            HashMap<String,Object> control = (HashMap<String,Object>)parser.invoke(parameters,errors,warnings);
            control.put("parallel",(Integer)0);
            if(type.length > 1) {
                if(type[1].equals("max")) {control.put("parallel",Runtime.getRuntime().availableProcessors());}
                else {Integer parallel = parseInteger("solver " + type[0]+ " ", type[1]);
                    if(parallel != null) {control.put("parallel",parallel);}}}
            solverControl.add(control);}}

    static void solveProblems() {
        int parallel = (Integer)globalControl.get("parallel");
        if(parallel == 0) {
            for(HashMap<String,Object> control : problemControl) {solveProblem(control);}
            return;}
        int nproblems = problemControl.size();
        int nthreads = nproblems / parallel;
        if(nproblems % parallel != 0) {++nthreads;}
        Thread[] threads = new Thread[nthreads];
        int threadCounter = -1;
        for(int n = 0; n < nproblems; ++n) {
            if((n % parallel) == 0) {
                ++threadCounter;
                int m = n;
                threads[threadCounter] = new Thread(()->solveProblems(m,parallel));}}
        for(int n = 0; n < nthreads; ++n) {threads[n].start();}}


    static void solveProblems(int n, int parallel) {
        for(int i = n; i < n+parallel; ++i) {
            solveProblem(problemControl.get(i));}}

    static void solveProblem(HashMap<String,Object> problemControl) {
        int nthreads = solverControl.size();
        Thread[] threads = new Thread[nthreads];
        for(int n = 0; n < nthreads; ++n) {
                HashMap<String,Object> solverCtr = solverControl.get(n);
                threads[n] = new Thread(()->solve(solverCtr, problemControl));}
        for(int n = 0; n < nthreads; ++n) {
            threads[n].start();}}


    static void solve(HashMap<String,Object> solverControl,HashMap<String,Object> problemControl) {
        try{
        ((Method)solverControl.get("solver")).invoke("solve",solverControl,problemControl,errors,warnings);}
        catch(Exception ex) {
            ex.printStackTrace();
            System.exit(1);}}

    private static void help(String[] args) {
        System.out.println("HELPhhh");

    }

    private static void parseCnfFile(String filename) {

    }

    private static void parseSpecFile(String[] args) {

    }
    static boolean info = true;

    private static void addLine(String line) {
        System.out.println("Line " + line);
        if(info) {infos.append(line+"\n"); return;}
        if(line.startsWith("global") || line.startsWith("problem") || line.startsWith("series") || line.startsWith("solver")) {
            info = false;}
        line = line.trim();
        if(line.isEmpty()) {return;}
        String[] parts = line.split("(=,:, )+",2);
        String firstPart = parts[0];
        switch(firstPart) {
            case "global":
                parameterMap = globalParameters;
                break;
            case "problem":
                parameterMap = new HashMap<>();
                problemSingle.add(parameterMap);
                parseParameters(line,parts);
                break;
            case "series":
                parameterMap = new HashMap<>();
                problemSeries.add(parameterMap);
                parseParameters(line,parts);
                break;
            case "solver":
                parameterMap = new HashMap<>();
                solverParameters.add(parameterMap);
                parseParameters(line,parts);
                break;
            default: parseParameters(line,parts);}}

    private static  void parseInStream() {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String line;
        try {
            line = reader.readLine();
            if (line.startsWith("help")) {
                help(line.trim().split("(=,:, )+"));
                return;}
            addLine(line);
            while((line = reader.readLine()) != null) {
                addLine(line);}}
            catch (IOException e) {e.printStackTrace();}}

    public static void  main(String[] args) {
        if(args.length > 0) {
            if(args[0].equals("help")) {help(args); return;}
            if(args.length == 1 && args[0].endsWith(".cnf")) {parseCnfFile(args[0]); return;}
            parseSpecFile(args);}
            else {parseInStream();}
        prepareProblems();
        if(errors.length() > 0) {
            System.out.println("Errors:\n");
            System.out.println(errors);}
        if(warnings.length() > 0) {
            System.out.println("Warnings:\n");
            System.out.println(warnings);}
        if(errors.length() >0 ) {return;}

        errors = new StringBuffer();
        warnings = new StringBuffer();
        solveProblems();}
}
