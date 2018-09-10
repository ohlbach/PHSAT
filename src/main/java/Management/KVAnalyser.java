package Management;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import Utilities.Utilities;

/**
 * Created by Ohlbach on 03.09.2018.
 * <br/>
 * After a KVParser has parsed specifications and turned them into key-value String maps,
 * the KVAnalyser tries to turn the value-Strings into suitable objects.
 *<br/>
 * The global parameters are treated in this class.
 * The parameters for problem and solver types are treated by calling corresponding methods in the generator and solver classes:
 *<br/>
 * HashMap&lt;String,Object&gt; parseSingleParameters(HashMap&lt;String,String&gt; parameters, StringBuffer errors, StringBuffer warnings) <br/>
 *
 * ArrayList&lt;HashMap&lt;String,Object&gt;&gt; parseSeriesParameters(HashMap&lt;String,String&gt; parameters, StringBuffer errors, StringBuffer warnings) <br/>
 *
 * HashMap&lt;String,Object&gt; parseSolverParameters(HashMap&lt;String,String&gt; parameters, StringBuffer errors, StringBuffer warnings)
 */
public class KVAnalyser {
    private KVParser kvParser;

    /** the analysed global parameters */
    public HashMap<String,Object> globalParameters             = new HashMap<>();
    /** the analysed problem parameters */
    public ArrayList<HashMap<String,Object>> problemParameters = new ArrayList<>();
    /** the analysed solver parameters */
    public ArrayList<HashMap<String,Object>> solverParameters  = new ArrayList<>();

    public StringBuffer errors;
    public StringBuffer warnings;

    /** created an Analyser
     *
     * @param errors  for error messages
     * @param warnings for warnings
     */
    public KVAnalyser(StringBuffer errors, StringBuffer warnings) {
        this.errors = errors;
        this.warnings = warnings;}

    /** analysed the results of the KVParser.
     *
     * @param kvParser the KVParser
     * @param classMap maps types to classes.
     */
    public void analyse(KVParser kvParser, HashMap<String,Class> classMap) {
        this.kvParser = kvParser;
        try{
            analyseGlobalParameters(kvParser.kvList.get("global"));
            analyseParameters(problemParameters,kvParser.kvList.get("problem"),classMap, "generator");
            analyseParameters(solverParameters,kvParser.kvList.get("solver"),classMap,"solver");}
        catch(Exception ex) {  // applies to programming errors.
            ex.printStackTrace();
            System.exit(1);}}

    /** analyses the global parameters: <br/>
     * parallel, logfile, debug.
     *
     * @param globalStrings the global parameter strings.
     */
    private void analyseGlobalParameters(ArrayList<HashMap<String,String>> globalStrings) {
        globalParameters.put("parallel",(Integer)0);  // problems are solved sequentially.
        globalParameters.put("logstream",System.out);
        globalParameters.put("resultstream",System.out);
        ArrayList<HashMap<String,String>> parameters = kvParser.kvList.get("global");
        if(parameters == null) {return;}
        for(HashMap<String,String> map : parameters) {
            for(Map.Entry<String,String> entry : map.entrySet()) {
                String key = entry.getKey();
                if(key.equals("global")) {return;}
                String value = entry.getValue();
                switch(key) {
                    case "parallel":
                        if(value.equals("true")) {globalParameters.put("parallel",Runtime.getRuntime().availableProcessors()); break;}
                        Integer par = Utilities.parseInteger("parallel", value,errors);
                        if(par != null) {globalParameters.put("parallel",par);}
                        break;
                    case "logfile":
                        try (PrintStream out = new PrintStream(new File(value))) {
                            globalParameters.put("logstream", out);}
                        catch(FileNotFoundException ex) {
                            System.err.println("Logfile "+ value + " cannot be opened");}
                        break;
                    case "debug":
                        globalParameters.put("debug", true);
                        break;
                    case "results":
                        try (PrintStream out = new PrintStream(new File(value))) {
                            globalParameters.put("resultstream", out);}
                        catch(FileNotFoundException ex) {
                            System.err.println("Resultfile "+ value + " cannot be opened");}
                        break;
                    default: warnings.append("Unknown global parameter: " + key);
                }}}}


    private void analyseParameters(ArrayList<HashMap<String,Object>> objectParameters, ArrayList<HashMap<String,String>> stringParameters, HashMap<String,Class> classMap, String type) throws Exception {
        System.out.println("ANA");
        for(HashMap<String,String> parameters :  stringParameters) {
            Class generatorClass = classMap.get(type);
            if(generatorClass == null) {errors.append("Unknown generator type: " + type+"\n"); continue;}
            Method parser = generatorClass.getMethod("parseParameters", HashMap.class, StringBuffer.class,StringBuffer.class);
            objectParameters.addAll((ArrayList<HashMap<String,Object>>)parser.invoke(null,parameters,errors,warnings));}}



    /** reports the status of the analyser as a string
     *
     * @return the status of the analyser as a string.
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        if(!globalParameters.isEmpty()) {
            st.append("Global Parameters:\n");
            st.append(globalParameters.toString()).append("\n\n");}

        st.append("Problem Parameters\n");
        for(HashMap<String,Object> map : problemParameters) {
            st.append(map.toString()).append("\n\n");}

        st.append("\n\nSolver Parameters\n");
        for(HashMap<String,Object> map : solverParameters) {
            st.append(map.toString()).append("\n\n");}
        return st.toString();
    }
}
