package Generators;

import Datastructures.Clauses.InputClauses;
import Management.GlobalParameters;
import Management.Monitor.Monitor;
import Management.ProblemSupervisor;
import Utilities.KVParser;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;

/** This is the interface to the generator classes.
 * The generator classes generate SAT-Problems from different sources. <br>
 * Each generator class should provide the following static methods: <br>
 *  - public static help()  for producing a help text<br>
 *  - public static ArrayList&lt;HashMap&lt;String,Object&gt;&gt; parseParameters(HashMap&lt;String,String&gt; parameters, Monitor errors, Monitor warnings) <br>
 *  - public static HashMap&lt;String,Object&gt; generate(HashMap&lt;String,Object&gt; parameters,
 *               ProblemSupervisor problemSupervisor, Monitor errors, Monitor warnings) <br>
 * <br>
 * The parseParameters method turns parameters as strings into sequences of parameters as objects <br>
 * The generate method generates InputClausees and puts them as parameter "clauses" into the parameters map.
 * <br>
 * One can add a new generator class by extending the variable 'generators' and the method 'generatorClass'.
 * <br>
 * The class has only static methods" <br>
 * Created by ohlbach on 09.10.2018.
 */
public abstract class Generator {

    public static String[] generators = new String[]{"random","file","pidgeonhole","string"};

    InputClauses inputClauses = null;

    /** checks if the name is a generator name
     *
     * @param name  a string
     * @return true if the name is the name of a generator.
     */
    public static boolean isGenerator(String name) {
        for(String generator : generators) {if(name.equals(generator)) {return true;}}
        return false;}

    /** maps the generator names to the generator classes
     *
     * @param name a generator name
     * @return the generator class, or null
     */
    public static Class generatorClass(String name) {
        switch (name) {
            case "random":       return Generators.RandomClauseSetGenerator.class;
            case "file":         return Generators.CNFReader.class;
            case "pidgeonhole":  return PigeonHoleGenerator.class;
            case "string" :      return Generators.StringClauseSetGenerator.class;
            default: return null;}}

    /** collects all the help-strings for all generator classes
     *
     * @return the collected help string for all generator classes
     */
    public static String help() {
        StringBuilder st = new StringBuilder();
        st.append("The following generator types are available:\n");
        for(String generator : generators) {
            st.append(generator).append(":\n");
            st.append(help(generator)).append("\n");}
        return st.toString();}

    /** returns the help-string for the generator with the given name
     *
     * @param name a generator name
     * @return its hel-string
     */
    public static String help(String name) {
        Class clazz = generatorClass(name);
        if(clazz == null) {return "Unknown Generator Class: " +name;}
        try{
            Method helper = clazz.getMethod("help");
            return (String)helper.invoke(null);}
        catch(Exception ex) {ex.printStackTrace();System.exit(1);}
        return null;}

    /** parses the string-type parameters into sequences of objects
     *
     * @param pars       the generator parameters
     * @param errors     for collecting error messages
     * @param warnings   for collecting warning messages
     * @return           a list of generators
     */
    public static ArrayList<Generator> parseParameters(ArrayList<HashMap<String,String>> pars,
                                                       GlobalParameters globalParameters,
                                                       StringBuilder errors, StringBuilder warnings) {
        ArrayList<Generator> generators = new ArrayList<>();
        for(HashMap<String,String> parameters: pars) {
            String type = parameters.get("generator");
            if(type == null) continue;
            Class clazz = generatorClass(type);
            if(clazz == null) {errors.append("Problem Generator: Unknown generator class: " + type); continue;}
            try{
                Method parser = clazz.getMethod("parseParameters",HashMap.class, GlobalParameters.class,
                        ArrayList.class, StringBuilder.class, StringBuilder.class);
                parser.invoke(null,parameters,globalParameters,generators,errors,warnings);}
            catch(Exception ex) {ex.printStackTrace();System.exit(1);}}
        return generators;}

    /** generates the clauses as BasicClauseList and puts then with key "clauses" into the parametes map.
     *
     * @param name        the generator name
     * @param parameters  the parameters for the generator
     * @param problemSupervisor for generating next clause id
     * @param errors      for collecting error messages
     * @param warnings    for collecting warning messages
     * @return            the new BasicClauseList
     */
    public static InputClauses generate(String name, HashMap<String,Object> parameters,
                                        ProblemSupervisor problemSupervisor,
                                        Monitor errors, Monitor warnings) {
        Class clazz = generatorClass(name);
        if(clazz == null) {errors.print("Problem Generator","Unknown generator class: " + name); return null;}
        try{
            Method generator = clazz.getMethod("generate", Monitor.class, Monitor.class);
            return (InputClauses) generator.invoke(null,errors,warnings);}
        catch(Exception ex) {ex.printStackTrace();System.exit(1);}
        return null;}

    /** extracts the type-specific parameters from the kvParser
     *
     * @param kvParser  contains all control parameters
     * @param type      one of the generator types
     * @return the parameters for the given type
     */
    public static HashMap<String,String> getParameters(KVParser kvParser, String type) {
        for(HashMap<String,String> parameters: kvParser.get("generator")) {
            if(type.equals(parameters.get("type"))) return parameters;}
        return null;}

    /** generates a BasicClauseList
     *
     * @param errorMonitor    for error massages
     * @param warningMonitor  for warnings
     * @return true if there was no error.
     */
    public abstract boolean generate(Monitor errorMonitor, Monitor warningMonitor);


}
