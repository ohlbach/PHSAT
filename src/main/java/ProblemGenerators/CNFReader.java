package ProblemGenerators;

import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import Management.GlobalParameters;
import Management.Monitor.Monitor;
import Utilities.Utilities;

import java.io.*;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
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
 * '&amp;':  means and:          'a 3 4 5' stands for 3 and 4 and 5.<br>
 * 'e':  means equivalences: 'e 4 5 -6' means that these three literals are equivalent.<br>
 * '&lt;=': means atleast:      '&lt;= 2 p q r' means atleast two of p,q,r are true.<br>
 * '&gt;=': means atleast:      '&gt;= 2 p q r' means atmost two of p,q,r are true.<br>
 * '=':  means exactly:      '= 2 p q r' means exactly two of p,q,r are true.<br>
 * '[min,max]':  means interval: '[2,4] p q r s t' means between 2 and 4 of p,q,r,s,t are true.<br>
 * No special symbol means 'or' 3 4 5' stands for 3 or 4 or 5.
 */
public final class CNFReader extends ProblemGenerator {

    private static final HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        Collections.addAll(keys, "generator", "file", "directory", "regExpr");}


    /** parses a HashMap with key-value pairs of a CNF-Reader:<br>
     * file:      a comma separated list of pathnames<br>
     * directory: a comma separated list of directories (all .cnf files in this directory are addressed) <br>
     * regExpr:   a regular expression: All files in the directories matching the expression are addressed
     *
     * @param parameters   contains all control parameters
     * @param globalParameters contains the global parameters.
     * @param generators  for adding new CNFReaders
     * @param errors      for error messages
     * @param warnings    for warnings
     * */
    public static void parseParameters(HashMap<String,String> parameters, GlobalParameters globalParameters,
                                       ArrayList<ProblemGenerator> generators,
                                       StringBuilder errors, StringBuilder warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {warnings.append("CNFReader: Unknown key in parameters: " + key + "\n");}}

        String files       = parameters.get("file");
        String directories = parameters.get("directory");
        String regExprs    = parameters.get("regExpr");
        String basicDirectory = globalParameters.basicDirectory;
        if(files != null) {
            for(String filename : files.split("\\s*[, ]\\s*")) {
                if(!filename.endsWith(".cnf")) {
                    warnings.append("CNFReader: Filename " + filename + " does not end with .cnf. The file is ignored.\n");
                    continue;}
                if(basicDirectory != null) {filename = Paths.get(basicDirectory,filename).toString();}
                File file = new File(filename);
                if(!file.exists()) {
                    warnings.append("CNFReader: Unknown file: " + filename +". The file is ignored.\n");}
                else {generators.add(new CNFReader(file));}}}

        if(directories != null && regExprs == null) {
            for(String directoryname : directories.split("\\s*,\\s*")) {
                if(basicDirectory != null) {directoryname = Paths.get(basicDirectory,directoryname).toString();}
                File directory = new File(directoryname);
                if(!(directory.exists() && directory.isDirectory())) {
                    errors.append("CNFReader: Unknown directory: " +directoryname+". The directory is ignored.\n");}
                else {
                    File[] fileList = directory.listFiles();
                    if(fileList == null || fileList.length == 0) {
                        warnings.append("CNFReader: Directory " + directoryname + " is empty.\n");}
                    else {
                        for(File file : fileList) {
                            if(file != null && file.isFile() && file.getName().endsWith(".cnf"))
                                generators.add(new CNFReader(file));}}}}
            return;}

        if(regExprs != null) {
            if(directories == null) {
                errors.append("CNFReader: A directory must be specified for " + regExprs +"\n");
                return;}
            for(String directoryname : directories.split("\\s*,\\s*")) {
                if(basicDirectory != null) {directoryname = Paths.get(basicDirectory,directoryname).toString();}
                File directory = new File(directoryname);
                if(!(directory.exists() && directory.isDirectory())) {
                    errors.append("CNFReader: Unknown directory: " + directoryname + ". The directory is ignored.\n");}
                else{
                    Pattern pattern;
                    try {pattern = Pattern.compile(regExprs);}
                    catch(PatternSyntaxException ex) {
                        errors.append("CNFReader: " + ex);
                        continue;}
                    File[] fileList = directory.listFiles(pathname -> pattern.matcher(pathname.getName()).matches());
                    if(fileList == null || fileList.length == 0) {
                        warnings.append("CNFReader: Directory " + directoryname + " contains no matched files.\n");}
                    else {for(File file: fileList) generators.add(new CNFReader(file));}}}}}

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
                " p cnf predicates clauses [symbolic]\n" +
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

    private final File file;

    public CNFReader(File file) {
        this.file = file;}

    /** reads the cnf-file and generates a BasicClauseList
     *
     * @param errorMonitor    for error massages
     * @return true if there was no error.
     */
    public InputClauses generateProblem(Monitor errorMonitor) {
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        StringBuilder info = new StringBuilder();
        String filename = file.getName();
        info.append("File ").append(filename).append("\n");
        String place =  "CNFReader File " + filename;
        int id = 1;
        BufferedReader reader;
        try {
            reader = new BufferedReader(new FileReader(file));
            String line;
            inputClauses = new InputClauses();
            Integer predicates = 0;
            int lineNumber = 0;
            while((line = reader.readLine()) != null) {
                ++lineNumber;
                String errorPrefix = "    line " + lineNumber + ": '" + line + "': ";
                line = line.trim();
                if(line.isEmpty()) {continue;}
                if(line.startsWith("%")){continue;}
                if(line.startsWith("c")) {info.append(line.substring(1)).append("\n"); continue;}
                if(line.startsWith("p") && !line.endsWith("0")) { // p cnf predicates clauses [symbolic]
                    String[] parts = line.split("\\s*[ ,]\\s*");
                    if(parts.length < 4) {
                        errors.append(errorPrefix+" illegal format of line").
                                append("It should be 'p cnf predicates clauses [symbolic]'\n");
                        return null;}
                    if(!parts[1].equals("cnf")) {
                        errors.append(errorPrefix+" indicates no cnf file\n");
                        return null;}

                    predicates = Utilities.parseInteger(errorPrefix, parts[2],errors);
                    if(predicates == null) {return null;}
                    if(predicates <= 0) {
                        errors.append(errorPrefix+" Negative number of predicates: '"+ parts[2]+"'\n");
                        return null;}
                    inputClauses.predicates = predicates;
                    inputClauses.symboltable = new Symboltable(predicates);
                    if(parts.length > 4) inputClauses.symboltable.symbolic = true;
                    continue;}
                if(predicates == 0) {
                    errors.append(errorPrefix+" p-line missing: 'p cnf predicates clauses [symbolic]'\n");
                    return null;}

                if(!line.endsWith(" 0")) {
                    errors.append(errorPrefix+" does not end with '0'\n");
                    continue;}
                int[] clause = StringClauseSetGenerator.parseLine(line.substring(0,line.length()-2).trim(),
                        id, inputClauses.symboltable,errorPrefix,errors);
                if(clause == null) continue;
                clause = InputClauses.checkSyntax(clause,predicates,errorPrefix,errors,warnings);
                if(clause == null) continue;
                ++id;
                inputClauses.addClause(clause);}}
        catch (FileNotFoundException e) {
            errors.append(place+" not found");}
        catch(IOException ex) {
            errors.append(place+" IOException\n" +ex);}
        finally{
            if(warnings.length()> 0) {errorMonitor.print(place,warnings);}
            if(errors.length() > 0) {errorMonitor.print(place,errors); inputClauses = null; return null;}}

        inputClauses.info = info.toString();
        inputClauses.nextId = id;
        return inputClauses;}

    /** turns the file and, if already filled, the basicClauseList into a string.
     *
     * @return the filename and, if already filled, the basicClauseList
     */
    public String toString() {
        String st = "CNFReader for file " + file;
        if(inputClauses != null) st += "\n"+ inputClauses;
        return st;}


}
