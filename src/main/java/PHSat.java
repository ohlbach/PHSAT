import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

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

    private static StringBuffer errors = new StringBuffer();
    private static StringBuffer warnings = new StringBuffer();

    private static HashMap<String,String> parameterMap = null;

    static void parseParameters(String line, String[] parameters) {
        if(parameters.length == 1) {parameterMap.put(parameters[0],"true");}
        else {parameterMap.put(parameters[0],parameters[1]);}}


    static void prepareProblems() {
        prepareGlobalParameters();
        prepareSingleParameters();
        prepareSeriesParameters();}

    static void prepareGlobalParameters() {
        for(Map.Entry entry : globalParameters.entrySet()) {
            switch(entry.getKey()) {
                case "parallel"
            }
        }
    }

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
            parseSpecFile(args);
            prepareProblems();
            return;}
        parseInStream();
        prepareProblems();
        }
}
