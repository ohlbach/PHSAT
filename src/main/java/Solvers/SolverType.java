package Solvers;

import Coordinator.Preprocessor;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by ohlbach on 09.10.2018.
 */
public class SolverType {
    public static String[] solvers = new String[]{"walker","recursive","resolution","connectionGraph","PHresolution"};

    /** maps the generator names to the generator classes
     *
     * @param name a generator name
     * @return the generator class, or null
     */
    public static Class solverClass(String name) {
        switch (name) {
            case "walker":      return Solvers.RandomWalker.class;
            default: return null;}}

    /** collects all the help-strings for all generator classes
     *
     * @return the collected help string for all generator classes
     */
    public static String help() {
        StringBuilder st = new StringBuilder();
        st.append("The following solver types are available:\n");
        for(String solver : solvers) {
            st.append(solver).append(":\n");
            st.append(help(solver)).append("\n");}
        return st.toString();}

    /** returns the help-string for the generator with the given name
     *
     * @param name a generator name
     * @return its hel-string
     */
    public static String help(String name) {
        Class clazz = solverClass(name);
        if(clazz == null) {return "Unknown Solver Class: " +name;}
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
        Class clazz = solverClass(name);
        if(clazz == null) {errors.append("Unknown solver class: " + name+"\n"); return null;}
        try{
            Method parser = clazz.getMethod("parseParameters",HashMap.class,StringBuffer.class, StringBuffer.class);
            return (ArrayList<HashMap<String,Object>>)parser.invoke(null,parameters,errors,warnings);}
        catch(Exception ex) {ex.printStackTrace();System.exit(1);}
        return null;}

    /** parses the string-type parameters into sequences of objects
     *
     * @param name       the generator name
     * @param solverParameters a key-value map with parameters as strings
     * @return           a list of key-value maps where the values are objects.
     */
    public static Object construct(String name, Integer id, HashMap<String,Object> solverParameters,
                                   HashMap<String,Object> globalParameters, Preprocessor centralData) {
        Class clazz = solverClass(name);
        try{
            Constructor constructor = clazz.getConstructor(Integer.class,HashMap.class,HashMap.class, Preprocessor.class);
            return constructor.newInstance(id,solverParameters,globalParameters,centralData);}
        catch(Exception ex) {ex.printStackTrace();System.exit(1);}
        return null;}






}
