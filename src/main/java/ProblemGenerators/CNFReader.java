package ProblemGenerators;

import Datastructures.Clauses.InputClauses;
import Management.Monitor.Monitor;
import Utilities.FileIterator;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import static Utilities.Utilities.pathWithHome;

/**
 * Created by ohlbach on 26.08.2018.
 * <p>
 * This class is for reading cnf-files. <br>
 * A standard cnf-file has the following structure:<br>
 * # comment<br>
 * # comment<br>
 * % ignored lines
 * p cnf predicates clauses [symbolic]<br>
 * clause1 0 <br>
 * clause2 0 <br>
 * ...<br>
 * A clause is a blank or comma-separated list of literals (positive or negative numbers /= 0, or names).
 * <br>
 * An extension of this format may contain clauses beginning with special characters:.<br>
 *
 * '&#38;':  means and:          '&#38; 3 4 5' stands for 3 and 4 and 5.<br>
 * 'e':  means equivalences: 'e 4 5 -6' means that these three literals are equivalent.<br>
 * '&lt;=': means atleast:      '&lt;= 2 p q r' means atleast two of p,q,r are true.<br>
 * '&gt;=': means atleast:      '&gt;= 2 p q r' means atmost two of p,q,r are true.<br>
 * '=':  means exactly:      '= 2 p q r' means exactly two of p,q,r are true.<br>
 * '[min,max]':  means interval: '[2,4] p q r s t' means between 2 and 4 of p,q,r,s,t are true.<br>
 * No special symbol means 'or' 3 4 5' stands for 3 or 4 or 5.
 */
public final class CNFReader extends ProblemGenerator {

    /** contains the allowed keys in the specification.*/
    private static final HashSet<String> keys = new HashSet<>();
    static { // these are the allowed keys in the specification.
        Collections.addAll(keys, "generator", "files", "directories", "regExprs");}

    /** the cnf file */
    private final File file;


    /** creates a CNFReader for the given file
     *
     * @param file a cnf file.
     */
    public CNFReader(File file) {
        this.file = file;}

    /** generates a help-string
     *
     * @return a help-string
     */
    public static String help() {
        return "CNFReader for reading CNF-Files.\n" +
                "The parameters are:\n" +
                "  files:       A single filename or a comma-separated list of filenames.\n" +
                "  directories: A single directory name or a comma-separated list of directory names.\n" +
                "               All files in the directory ending with .cnf are loaded, unless regExpr is defined.\n" +
                "  regExprs:    A regular expression to select files in the directory.\n\n" +
                "A standard cnf-file has the following structure:\n" +
                " # comment\n" +
                " # comment\n" +
                " % ignored lines\n"+
                " p cnf predicates ...\n" +
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

    /** parses a HashMap with key-value pairs of a CNF-Reader:<br>
     * file:      a comma separated list of pathnames<br>
     * *          a pathname may start with 'home'. It stands for the user's homedirectory.
     * directory: a comma separated list of directories (all .cnf files in this directory are addressed) <br>
     * regExpr:   a regular expression: All files in the directories matching the expression are addressed
     *
     * @param parameters   contains all control parameters.
     * @param generators  for adding new CNFReaders
     * @param errors      for error messages
     * @param warnings    for warnings
     */
    public static void makeProblemGenerator(HashMap<String,String> parameters,
                         ArrayList<ProblemGenerator> generators,
                         StringBuilder errors, StringBuilder warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {
                warnings.append("CNFReader: Unknown key in parameters: ").append(key).append("\n");}}

        String files       = parameters.get("files");
        String directories = parameters.get("directories");
        String regExprs    = parameters.get("regExprs");
        if(files != null) {
            for(String filename : files.split("\\s*[, ]\\s*")) {
                if(!filename.endsWith(".cnf")) {
                    warnings.append("CNFReader: Filename ").append(filename).append(" does not end with .cnf. The file is ignored.\n");
                    continue;}
                File file = pathWithHome(filename).toFile();
                if(!file.exists()) {
                    warnings.append("CNFReader: Unknown file: ").append(file.getAbsolutePath()).append(". The file is ignored.\n");}
                else {generators.add(new CNFReader(file));}}}

        if(directories != null && regExprs == null) {
            for(String directoryname : directories.split("\\s*[, ]\\s*")) {
                File directory = pathWithHome(directoryname).toFile();
                if(!(directory.exists() && directory.isDirectory())) {
                    errors.append("CNFReader: Unknown directory: ").append(directoryname).append(". The directory is ignored.\n");}
                else {
                    File[] fileList = directory.listFiles();
                    if(fileList == null || fileList.length == 0) {
                        warnings.append("CNFReader: Directory ").append(directoryname).append(" is empty.\n");}
                    else {
                        for(File file : fileList) {
                            if(file != null && file.isFile() && file.getName().endsWith(".cnf"))
                                generators.add(new CNFReader(file));}}}}
            return;}

        if(regExprs != null) {
            if(directories == null) {
                errors.append("CNFReader: A directory must be specified for regulare expression: ").append(regExprs).append("\n");
                return;}
            for(String directoryname : directories.split("\\s*[, ]\\s*")) {
                File directory = pathWithHome(directoryname).toFile();
                if(!(directory.exists() && directory.isDirectory())) {
                    errors.append("CNFReader: Unknown directory: ").append(directory.getAbsolutePath()).
                            append(". The directory is ignored.\n");}
                else{
                    Pattern pattern;
                    try {pattern = Pattern.compile(regExprs);}
                    catch(PatternSyntaxException ex) {
                        errors.append("CNFReader: ").append(ex);
                        continue;}
                    File[] fileList = directory.listFiles(pathname -> pattern.matcher(pathname.getName()).matches());
                    if(fileList == null || fileList.length == 0) {
                        warnings.append("CNFReader: Directory ").append(directoryname).append(" contains no matched files.\n");}
                    else {for(File file: fileList) generators.add(new CNFReader(file));}}}}}




    /** reads the cnf-file and generates a InputClauses
     *
     * @param errorMonitor    for error massages
     * @return null or the new InputClauses.
     */
    public InputClauses generateProblem(Monitor errorMonitor) {
        StringBuilder errors   = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        InputClauses inputClauses = null;
        String problemName = file.getName();
        try{
            inputClauses = parseClauses(problemName,new FileIterator(file.getAbsolutePath()),errors,warnings);}
        catch (FileNotFoundException ex){
            errors.append("CNFReader: File ").append(file).append(" was not found");}
        if(errorMonitor != null) {
            if(warnings.length() > 0) {
                errorMonitor.println("Warnings when parsing clauses "+problemName + "\n"+ errors);}
            if(errors.length() > 0) {
                errorMonitor.println("Errors when parsing clauses "+problemName + "\n"+ errors);}}
        if(inputClauses == null) return null;
        inputClauses.problemName = problemName;
        return inputClauses;}

    /** turns the file and, if already filled, the InputClauses into a string.
     *
     * @return the filename and, if already filled, the InputClauses
     */
    public String toString() {
        String st = "CNFReader for file " + file.getAbsolutePath();
        if(inputClauses != null) st += "\n"+ inputClauses;
        return st;}


}
