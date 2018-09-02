import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Created by Ohlbach on 02.09.2018.
 */
public class PHSat {
    private static HashMap<String,String> globalParameters    = new HashMap<>();
    private static HashMap<String,String> generatorParameters = new HashMap<>();
    private static HashMap<String,String> solverParameters    = new HashMap<>();

    private static StringBuffer errors = new StringBuffer();

    private static BiConsumer<String,String[]> processor =
            ((line,parameters) -> {
        switch(parameters.length) {
            case 1: globalParameters.put(parameters[0],"true"); break;
            case 2: globalParameters.put(parameters[0],parameters[1]); break;
            default: errors.append("Unknown command ").append(line).append("\n");}});

    private static HashMap<String,BiConsumer<String,String>> processors = new HashMap<>();


    private static void help(String[] args) {
        System.out.println("HELPhhh");

    }

    private static void processCnfFile(String filename) {

    }

    private static void processSpecFile(String[] args) {

    }

    private static void addLine(String line) {
        System.out.println("Line " + line);
        line = line.trim();
        if(line.isEmpty()) {return;}
        String[] parts = line.split("(=,:, )+");
        processor.accept(line,parts);

    }

    private static  void processInStream() {
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
            if(args.length == 1 && args[0].endsWith(".cnf")) {processCnfFile(args[0]); return;}
            processSpecFile(args); return;}
        processInStream();
        }
}
