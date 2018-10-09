package Generators;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;

/** This is the interface to the generator classes.
 * The generator classes generate SAT-Problems from different sources. <br/>
 * Each generator class should provide the following static methods: <br>
 *  - public static help()  for producing a help text<br/>
 *  - public static ArrayList&lt;HashMap&lt;String,Object&gt;&gt; parseParameters(HashMap&lt;String,String&gt; parameters, StringBuffer errors, StringBuffer warnings) <br/>
 *  - public static HashMap&lt;String,Object&gt generate(HashMap&lt;String,Object&gt; parameters, StringBuffer errors, StringBuffer warnings) <br/>
 * <br/>
 * The parseParameters method turns parameters as strings into sequences of parameters as objects <br/>
 * The generate method generates a BasicClauseList and puts it as parameter "clauses" into the parameters map.
 * <br/>
 * One can add a new generator class by extending the variable 'generators' and the method 'generatorClass'.
 * <br/>
 * The class has only static methods" <br/>
 * Created by ohlbach on 09.10.2018.
 */
public final class SourceType {

    public static String[] generators = new String[]{"random","file","pidgeonhole","string"};

    /** maps the generator names to the generator classes
     *
     * @param name a generator name
     * @return the generator class, or null
     */
    public static Class generatorClass(String name) {
        switch (name) {
            case "random":      return Generators.RandomClauseSetGenerator.class;
            case "file":        return Generators.CNFReader.class;
            case "pidgeonhole": return Generators.PidgeonHoleGenerator.class;
            case "string" :     return Generators.StringClauseSetGenerator.class;
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
     * @param name       the generator name
     * @param parameters a key-value map with parameters as strings
     * @param errors     for collecting error messages
     * @param warnings   for collecting warning messages
     * @return           a list of key-value maps where the values are objects.
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(String name, HashMap<String,String> parameters,
                                                                    StringBuffer errors, StringBuffer warnings) {
        Class clazz = generatorClass(name);
        if(clazz == null) {errors.append("Unknown generator class: " + name+"\n"); return null;}
        try{
            Method parser = clazz.getMethod("parseParameters",HashMap.class,StringBuffer.class, StringBuffer.class);
            return (ArrayList<HashMap<String,Object>>)parser.invoke(null,parameters,errors,warnings);}
        catch(Exception ex) {ex.printStackTrace();System.exit(1);}
        return null;}

    /** generates the clauses as BasicClauseList and puts then with key "clauses" into the parametes map.
     *
     * @param name        the generator name
     * @param parameters  the parameters for the generator
     * @param errors      for collecting error messages
     * @param warnings    for collecting warning messages
     * @return            the parameters map with a new key "clauses".
     */
    public static HashMap<String,Object> generate(String name, HashMap<String,Object> parameters,
                                                                    StringBuffer errors, StringBuffer warnings) {
        Class clazz = generatorClass(name);
        if(clazz == null) {errors.append("Unknown generator class: " + name+"\n"); return null;}
        try{
            Method parser = clazz.getMethod("generate",HashMap.class,StringBuffer.class, StringBuffer.class);
            return (HashMap<String,Object>)parser.invoke(null,parameters,errors,warnings);}
        catch(Exception ex) {ex.printStackTrace();System.exit(1);}
        return null;}


}
