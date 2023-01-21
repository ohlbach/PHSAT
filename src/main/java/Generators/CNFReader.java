package Generators;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Symboltable;
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
 * '&':  means and:          'a 3 4 5' stands for 3 and 4 and 5.<br>
 * 'e':  means equivalences: 'e 4 5 -6' means that these three literals are equivalent.<br>
 * '<=': means atleast:      '<= 2 p q r' means atleast two of p,q,r are true.<br>
 * '>=': means atleast:      '>= 2 p q r' means atmost two of p,q,r are true.<br>
 * '=':  means exactly:      '= 2 p q r' means exactly two of p,q,r are true.<br>
 * '[min,max]':  means interval: '[2,4] p q r s t' means between 2 and 4 of p,q,r,s,t are true.<br>
 * No special symbol means 'or' 3 4 5' stands for 3 or 4 or 5.
 */
public final class CNFReader extends Generator {

    private static final HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        Collections.addAll(keys, "generator", "file", "directory", "regExpr");}


    /** parses a HashMap with key-value pairs of a CNF-Reader:<br>
     * file:      a comma separated list of pathnames<br>
     * directory: a comma separated list of directories (all .cnf files in this directory are addressed) <br>
     * regExpr:   a regular expression: All files in the directories matching the expression are addressed
     *
     * @param parameters  the parameters with the keys "file", "directory", "regExpr"
     * @param basicDirectory null or a directory such that all files and directories are relative to this directory.
     * @param generators  for adding new CNFReaders
     * @param errors      for error messages
     * @param warnings    for warnings
     * */
    public static void parseParameters(HashMap<String,String> parameters, String basicDirectory,
                                       ArrayList<Generator> generators,
                                       StringBuilder errors, StringBuilder warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {warnings.append("CNFReader: Unknown key in parameters: " + key + "\n");}}

        String files       = parameters.get("file");
        String directories = parameters.get("directory");
        String regExprs    = parameters.get("regExpr");

        if(files != null) {
            for(String filename : files.split("\\s*[, ]\\s*")) {
                if(!filename.endsWith(".cnf")) {
                    warnings.append("CNFReader: Filename " + filename + " does not end with .cnf. The file is ignored.\n");
                    continue;}
                if(basicDirectory != null) {filename = Paths.get(basicDirectory,filename).toString();}
                File file = new File(filename);
                if(!file.exists()) {
                    warnings.append("CNFReader: Unknown file: " + filename +". The file is ignored.\n");
                    continue;}
                else {generators.add(new CNFReader(file));}}}

        if(directories != null && regExprs == null) {
            for(String directoryname : directories.split("\\s*,\\s*")) {
                if(basicDirectory != null) {directoryname = Paths.get(basicDirectory,directoryname).toString();}
                File directory = new File(directoryname);
                if(!(directory.exists() && directory.isDirectory())) {
                    errors.append("CNFReader: Unknown directory: " +directoryname+". The directory is ignored.\n");}
                else {
                    for(File file : directory.listFiles()) {
                        if(file != null && file.isFile() && file.getName().endsWith(".cnf")) {
                            generators.add(new CNFReader(file));}}}}
            return;}

        if(regExprs != null) {
            if(directories == null) {errors.append("CNFReader: A directory must be specified for " + regExprs +"\n");}
            for(String directoryname : directories.split("\\s*,\\s*")) {
                if(basicDirectory != null) {directoryname = Paths.get(basicDirectory,directoryname).toString();}
                File directory = new File(directoryname);
                if(!(directory.exists() && directory.isDirectory())) {
                    errors.append("CNFReader: Unknown directory: " + directoryname + ". The directory is ignored.\n");}
                else{
                    Pattern pattern;
                    try {pattern = Pattern.compile(regExprs);}
                    catch(PatternSyntaxException ex) {
                        errors.append("CNFReader: " + ex.toString());
                        continue;}
                    for(File file: directory.listFiles(pathname -> pattern.matcher(pathname.getName()).matches())) {
                        generators.add(new CNFReader(file));}}}}}

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

    private File file;

    public CNFReader(File file) {
        this.file = file;}

    /** reads the cnf-file
     *
     * @param errorMonitor    for error massages
     * @param warningMonitor  for warnings (not used here)
     */
    public void generate(Monitor errorMonitor, Monitor warningMonitor) {
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        StringBuilder info = new StringBuilder();
        String filename = file.getName();
        info.append("File ").append(filename).append("\n");
        String place =  "File " + filename;
        BufferedReader reader;
        try {reader = new BufferedReader(new FileReader(file));}
        catch (FileNotFoundException e) {
            errors.append(place+" not found");
            return;}
        String line;
        basicClauseList = new BasicClauseList();
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
                    return;}
                if(!parts[1].equals("cnf")) {
                    errors.append(errorPrefix+" indicates no cnf file\n");
                    return;}

                predicates = Utilities.parseInteger(errorPrefix, parts[2],errors);
                if(predicates == null) {return;}
                if(predicates <= 0) {
                    errors.append(errorPrefix+" Negative number of predicates: '"+ parts[2]+"'");
                    return;}
                basicClauseList.predicates = predicates;
                basicClauseList.symboltable = new Symboltable(predicates);
                continue;}
            if(predicates == 0) {
                errors.append(errorPrefix+" p-line missing: 'p cnf predicates clauses'");
                return;}

            if(!line.endsWith(" 0")) {
                errors.append(errorPrefix+" does not end with '0'");
                continue;}
            int[] clause = StringClauseSetGenerator.parseLine(line.substring(0,line.length()-2).trim(),
                    id,basicClauseList.symboltable,errorPrefix,errors);
            if(clause == null) continue;
            ++id;
            basicClauseList.addClause(clause,errorPrefix,errors,warnings);
        }
        basicClauseList.info = info.toString();}
        catch(IOException ex) {
            errors.append(place+" IOException\n" +ex);
            return;}
        basicClauseList.nextId = id;}
    
    public String toString() {
        String st = "CNFReader for file " + file.toString();
        if(basicClauseList != null) st += "\n"+ basicClauseList.toString();
        return st;}


}
