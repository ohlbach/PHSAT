package Solvers;

import Coordinator.CentralProcessor;
import Coordinator.PreProcessor;
import Datastructures.Results.Result;
import Datastructures.Statistics.Statistic;
import Datastructures.Theory.Model;
import Management.GlobalParameters;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by ohlbach on 09.10.2018.
 */
public abstract class Solver {
    public String id;
    public static String[] solvers = new String[]{"walker","resolution"};
    public Statistic statistics;

    protected HashMap<String,Object> solverControl;
    protected GlobalParameters globalParameters;
    protected CentralProcessor centralProcessor;
    protected Model globalModel;

    public Solver(String id, HashMap<String,Object> solverControl, GlobalParameters globalParameters, CentralProcessor centralProcessor) {
        this.id = id;
        this.solverControl = solverControl;
        this.globalParameters = globalParameters;
        this.centralProcessor = centralProcessor;
        this.globalModel = centralProcessor.model;
    }

    /** maps the generator names to the generator classes
     *
     * @param name a generator name
     * @return the generator class, or null
     */
    public static Class solverClass(String name) {
        switch (name) {
            case "walker":      return Solvers.RandomWalker.class;
            case "resolution":  return Solvers.Resolution.class;
            default: return null;}}

    /** checks if the name is a solver name
     *
     * @param name  a string
     * @return true if the name is the name of a solver.
     */
    public static boolean isSolver(String name) {
        for(String solver : solvers) {if(name.equals(solver)) {return true;}}
        return false;}

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
    public static Solver construct(String name, Integer id, GlobalParameters globalParameters,
                                   HashMap<String,Object> solverParameters, CentralProcessor centralProcessor) {
        Class clazz = solverClass(name);
        try{
            Constructor constructor = clazz.getConstructor(Integer.class,HashMap.class,HashMap.class, PreProcessor.class);
            return (Solver)constructor.newInstance(id,solverParameters,globalParameters,centralProcessor);}
        catch(Exception ex) {ex.printStackTrace();System.exit(1);}
        return null;}

    public abstract Result solve();








}