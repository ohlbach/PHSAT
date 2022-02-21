package Generators;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Symboltable;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorLife;
import Management.ProblemSupervisor;
import Utilities.Utilities;

import java.io.*;
import java.util.*;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * Created by ohlbach on 26.08.2018.
 * <p>
 * This class is for reading cnf-files. <br>
 * A standard cnf-file has the following structure:<br>
 * c comment<br>
 * c comment<br>
 * % ignored lines
 * p cnf predicates clauses [symbolic]<br>
 * clause1 0 <br>
 * clause2 0 <br>
 * ...<br>
 * A clause is a blank or comma-separated list of literals (positive or negative numbers /= 0, or names).
 * <br>
 * An extension of this format may contain clauses beginning with special characters:.<br>
 *
 * '&':  means and:          'a 3 4 5' stands for 3 and 4 and 5.<br>
 * 'e':  means equivalences: 'e 4 5 -6' means that these three literals are equivalent.<br>
 * '<=': means atleast:      '<= 2 p q r' means atleast two of p,q,r are true.<br>
 * '>=': means atleast:      '>= 2 p q r' means atmost two of p,q,r are true.<br>
 * '=':  means exactly:      '= 2 p q r' means exactly two of p,q,r are true.<br>
 * '[min,max]':  means interval: '[2,4] p q r s t' means between 2 and 4 of p,q,r,s,t are true.<br>
 * No special symbol means 'or' 3 4 5' stands for 3 or 4 or 5.
 */
public final class CNFReader {

    private static final HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        Collections.addAll(keys, "problem", "type", "file", "directory", "regExpr");}

    /** parses a HashMap with key-value pairs:<br>
     * file: a comma separated list of pathnames<br>
     * directory: a comma separated list of directories (all .cnf files in this directory are adressed) <br>
     * regExpr: a regular expression: All files in the directories matching the expression are addressed
     *
     * @param parameters  the parameters with the keys "file", "directory", "regExpr"
     * @param errors      for error messages
     * @param warnings    for warnings
     * @return            a list of HashMaps with key "file" and value the corresponding File object, and 'name' with the filename
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters,
                                                                    MonitorLife errors, MonitorLife warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {warnings.print("CNFReader", "Unknown key in parameters: " + key + "\n");}}

        String files       = parameters.get("file");
        String directories = parameters.get("directory");
        String regExprs    = parameters.get("regExpr");


        ArrayList<HashMap<String,Object>> control = new ArrayList<>();

        if(files != null) {
            for(String filename : files.split("\\s*[, ]\\s*")) {
                File file = new File(filename);
                if(!file.exists()) {errors.print("CNFReader", "Unknown file: " +filename+"\n");}
                else {HashMap<String,Object> map = new HashMap<>();
                      map.put("file",file);
                      map.put("name",file.getName());
                      control.add(map);}}}

        if(directories != null && regExprs == null) {
            for(String directoryname : directories.split("\\s*,\\s*")) {
                File directory = new File(directoryname);
                if(!(directory.exists() && directory.isDirectory())) {
                    errors.print("CNFReader", "Unknown directory: " +directoryname+"\n");}
                else {
                    for(File file : directory.listFiles()) {
                        if(file != null && file.isFile() && file.getName().endsWith(".cnf")) {
                            HashMap<String,Object> map = new HashMap<>();
                            map.put("file",file);
                            map.put("name",file.getName());
                            control.add(map);}}}} }

        if(regExprs != null) {
            if(directories == null) {errors.print("CNFReader", "A directory must be specified for " + regExprs +"\n");}
            for(String directoryname : directories.split("\\s*,\\s*")) {
                File directory = new File(directoryname);
                if(!(directory.exists() && directory.isDirectory())) {
                    errors.print("CNFReader", "Unknown directory: " +directoryname+"\n");}
                else{
                    Pattern pattern;
                    try {pattern = Pattern.compile(regExprs);}
                    catch(PatternSyntaxException ex) {
                        errors.print("CNFReader", ex.toString());
                        continue;}
                    for(File file: directory.listFiles(pathname -> pattern.matcher(pathname.getName()).matches())) {
                        HashMap<String,Object> map = new HashMap<>();
                        map.put("file",file);
                        map.put("name",file.getName());
                        control.add(map);}}}}

        return control;}

    /** generates a help-string
     *
     * @return a help-string
     */
    public static String help() {
        return "CNFReader for reading CNF-Files.\n" +
                "The parameters are:\n" +
                "  file:      A single filename or a comma-separated list of filenames.\n" +
                "  directory: A single directory name or a comma-separated list of directory names.\n" +
                "             All files in the directory ending with .cnf are loaded, unless regExpr is defined.\n" +
                "  regExpr:   A regular expression to select files in the directory.\n\n" +
                "A standard cnf-file has the following structure:\n" +
                " c comment\n" +
                " c comment\n" +
                " % ignored lines\n"+
                " p cnf predicates clauses\n" +
                " clause1 0\n" +
                " clause2 0\n" +
                " ...\n" +
                " A clause is a blank or comma-separated list of literals (positive or negative numbers /= 0, or symbols).\n" +
                " \n" +
                " An extension of this format may contain clauses beginning with special characters:.\n" +
                " '&':  means and:          '& 3 4 5'     stands for 3 and 4 and 5.\n" +
                " 'e':  means equivalence:  'e 4 5 -6'    means that these three literals are equivalent.\n" +
                " '>=': means atleast:      '>= 2 p q r'  means at least two of p,q,r are true.\n" +
                " '<=': means atmost:       '<= 2 p q r'  means at most two of p,q,r are true.\n" +
                " [min,max]: means interval '[2,3] p,q,r' means between 2 and 3 of p,q,r are true.\n"+
                " '=':  means exactly:      '= 2 p q r'   means exactly two of p,q,r are true.\n" +
                "No special symbol means 'or': 'p,q,r' means p or q or r";
    }

    /** reads the cnf-file
     *
     * @param parameters a HashMap with key "file"
     * @param errorMonitor    for error massages
     * @param warningMonitor  for warnings (not used here)
     * @return  null or the new clauses
     */
    public static BasicClauseList generate(HashMap<String,Object> parameters,
                                           ProblemSupervisor problemSupervisor,
                                           Monitor errorMonitor, Monitor warningMonitor) {
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String name = "CNFeader";
        StringBuilder info = new StringBuilder();
        File file = (File)parameters.get("file");
        String filename = file.getName();
        info.append("File ").append(filename).append("\n");
        String place =  "File " + filename;
        String prefix = "          ";
        BufferedReader reader;
        try {reader = new BufferedReader(new FileReader(file));}
        catch (FileNotFoundException e) {
            errors.append(place+" not found");
            return null;}
        String line;
        BasicClauseList bcl = new BasicClauseList();
        Integer predicates = 0;
        int lineNumber = 0;
        int id = 1;
        try{
        while((line = reader.readLine()) != null) {
            ++lineNumber;
            String errorPrefix = place + ", line " + lineNumber + ": '" + line + "': ";
            line = line.trim();
            if(line.isEmpty()) {continue;}
            if(line.startsWith("%")){continue;}
            if(line.startsWith("c")) {info.append(line.substring(1)).append("\n"); continue;}
            if(line.startsWith("p") && !line.endsWith("0")) { // p cnf predicates clauses symbolic
                String[] parts = line.split("\\s*( |,)\\s*");
                if(parts.length < 4) {
                    errors.append(errorPrefix+" illegal format of line").
                            append("It should be 'p cnf predicates clauses'");
                    return null;}
                if(!parts[1].equals("cnf")) {
                    errors.append(errorPrefix+" indicates no cnf file\n");
                    return null;}

                predicates = Utilities.parseInteger(errorPrefix, parts[2],errors);
                if(predicates == null) {return null;}
                if(predicates <= 0) {
                    errors.append(errorPrefix+" Negative number of predicates: '"+ parts[2]+"'");
                    return null;}
                bcl.predicates = predicates;
                bcl.symboltable = new Symboltable(predicates);
                continue;}
            if(predicates == 0) {
                errors.append(errorPrefix+" p-line missing: 'p cnf predicates clauses'");
                return null;}

            if(!line.endsWith(" 0")) {
                errors.append(errorPrefix+" does not end with '0'");
                continue;}
            int[] clause = StringClauseSetGenerator.parseLine(line.substring(0,line.length()-2).trim(),
                    id,bcl.symboltable,errorPrefix,errors);
            if(clause == null) continue;
            ++id;
            bcl.addClause(clause,errorPrefix,errors,warnings);
        }
        bcl.info = info.toString();}
        catch(IOException ex) {
            errors.append(place+" IOException\n" +ex);
            return null;}
        problemSupervisor.clauseCounter = id;
        return bcl;}


}
