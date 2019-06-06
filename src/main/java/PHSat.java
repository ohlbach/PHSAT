import Generators.Generator;
import Management.Controller;
import Management.GlobalParameters;
import Management.KVParser;
import Solvers.Solver;
import Utilities.Utilities;

import java.io.*;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

/** This is the class with the main method for the PHSat system. <br>
 * Created by Ohlbach on 02.09.2018.
 */
public class PHSat {
    /** for controlling the execution of the problem processing */
    private static Controller controller;

    /** This is a file with default parameters for 'global' and 'solver'. */
    private static File DefaultFile = Paths.get(System.getProperties().get("user.dir").toString(),
            "src","main","resources","DefaultParameters.phs").toFile();


    /** The main method can be called with or without arguments. <br>
     * If it is called without arguments, the input is read from System.in.<br>
     * The arguments can be: <br>
     * - help (all help-texts are printed to System.out)<br>
     * - help name (the help-text for a problem generator or a problem solver with this name is printed) <br>
     * - the name of a .cnf file (it is processed with default values)<br>
     * - the name of a parameter specification file (it contains the problem- and solver specifications).
     *
     * @param args for the commands
     */
    public static void  main(String[] args) {
        long start = System.currentTimeMillis();
        //args = new String[]{"help","global"};
        //args = new String[]{Utilities.resourceFile("Purity.cnf")};
        KVParser kvParser = new KVParser("global", "problem", "solver");
        boolean goon = false;
        if(args.length > 0) {
            if(args[0].trim().equals("help")) {help(args); return;}
            if(args.length == 1 && args[0].endsWith(".cnf")) {
                goon = cnfFile(kvParser,args[0]);}
            else{goon = kvParser.parseFile(args[0]);}}
        else {goon = parseInStream(kvParser);}
        if(goon) {
            readDefaults(kvParser);
            ArrayList<HashMap<String,String>> globalParameters = kvParser.get("global");
            if(globalParameters != null && globalParameters.size()> 1) {
                System.out.println("There should be only one set of global parameters.");}
            HashMap<String,String> global = !globalParameters.isEmpty() ? globalParameters.get(0) : null;
            controller = new Controller(global,kvParser.get("problem"), kvParser.get("solver"));
            if(controller.analyseParameters()) {
                if(controller.solve()) {controller.printStatistics();}
                controller.close();}}
        long end = System.currentTimeMillis();
        System.out.println("Total Time: " + (end-start) + " ms");
    }

    /** This method calls the help()-methods of the generators and solvers, and prints the results.
     *
     * @param args either empty, or [help,name] where name is the name of a generator or solver.
     */
    private static void help(String[] args) {
        if(args.length > 1) {
            String name = args[1];
            if(name.equals("global"))       {System.out.println(GlobalParameters.help());     return;}
            if(Generator.isGenerator(name)) {System.out.println(Generator.help(name)); return;}
            if(Solver.isSolver(name))       {System.out.println(Solver.help(name)); return;}
            System.out.println("Unknown name '"+ name + "'. The available names are:");
            System.out.println("Generators: " + Arrays.toString(Generator.generators));
            System.out.println("Solvers:    " + Arrays.toString(Solver.solvers));
            return;}
        System.out.println(GlobalParameters.help());
        System.out.println(Generator.help());
        System.out.println(Solver.help());}


    /** The specification of a problem source as .cnf file is added to the kvParser
     *
     * @param kvParser a key-value parser
     * @param filename the name of a .cnf file
     * @return true
     */
    private static boolean cnfFile(KVParser kvParser, String filename) {
        String parameters = "problem\n file = " + filename;
        kvParser.addLine("problem");
        kvParser.addLine("type = file");
        kvParser.addLine("file = " + filename);
        return true;}

    /** The method reads the specification from System.in and parses it with the kvParser.
     * An IOException causes the system to stop.
     *
     * @param kvParser for parsing the specification
     * @return true if the parsing succeeded
     */
    private static boolean parseInStream(KVParser kvParser) {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String line;
        try {
            line = reader.readLine();
            if (line.startsWith("help")) {
                help(line.trim().split("\\s*(=|:| )\\s*"));
                return false;}
            kvParser.addLine(line);
            kvParser.parseStream(System.in);
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
        KVParser defaultParser = new KVParser("global", "problem", "solver");
        defaultParser.parseFile(DefaultFile.getAbsolutePath());
        if(kvParser.get("global").isEmpty()) {kvParser.set("global",defaultParser.get("global"));}
        if(kvParser.get("solver").isEmpty()) {kvParser.set("solver",defaultParser.get("solver"));}}



}
