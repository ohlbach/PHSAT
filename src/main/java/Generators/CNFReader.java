package Generators;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.ClauseType;
import Datastructures.Symboltable;
import Management.ProblemSupervisor;
import Utilities.Utilities;

import java.io.*;
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
 * p cnf predicates clauses [symbolic]<br>
 * clause1 0 <br>
 * clause2 0 <br>
 * ...<br>
 * A clause is a blank or comma-separated list of literals (positive or negative numbers /= 0, or names).
 * <br>
 * An extension of this format may contain clauses beginning with special characters:.<br>
 *
 * 'o':  means or:           'o 3 4 5' stands for 3 or 4 or 5.<br>
 * 'a':  means and:          'a 3 4 5' stands for 3 and 4 and 5.<br>
 * 'e':  means equivalences: 'e 4 5 -6' means that these three literals are equivalent.<br>
 * '<=': means atleast:      '<= 2 p q r' means atleast two of p,q,r are true.<br>
 * '>=': means atleast:      '>= 2 p q r' means atmost two of p,q,r are true.<br>
 * '=':  means exactly:      '= 2 p q r' means exactly two of p,q,r are true.<br>
 */
public final class CNFReader {

    private static final HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        Collections.addAll(keys, "problem", "type", "file", "directory", "regExpr");
    }

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
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {warnings.append("CNFReader: unknown key in parameters: " + key + "\n");}}

        String files       = parameters.get("file");
        String directories = parameters.get("directory");
        String regExprs    = parameters.get("regExpr");


        ArrayList<HashMap<String,Object>> control = new ArrayList<>();

        if(files != null) {
            for(String filename : files.split("\\s*[, ]\\s*")) {
                File file = new File(filename);
                if(!file.exists()) {errors.append("CNFReader: unknown file: " +filename+"\n");}
                else {HashMap<String,Object> map = new HashMap<>();
                      map.put("file",file);
                      map.put("name",file.getName());
                      control.add(map);}}}

        if(directories != null && regExprs == null) {
            for(String directoryname : directories.split("\\s*,\\s*")) {
                File directory = new File(directoryname);
                if(!(directory.exists() && directory.isDirectory())) {
                    errors.append("CNFReader: unknown directory: " +directoryname+"\n");}
                else {
                    for(File file : directory.listFiles()) {
                        if(file != null && file.isFile() && file.getName().endsWith(".cnf")) {
                            HashMap<String,Object> map = new HashMap<>();
                            map.put("file",file);
                            map.put("name",file.getName());
                            control.add(map);}}}} }

        if(regExprs != null) {
            if(directories == null) {errors.append("CNFReader: a directory must be specified for " + regExprs +"\n");}
            for(String directoryname : directories.split("\\s*,\\s*")) {
                File directory = new File(directoryname);
                if(!(directory.exists() && directory.isDirectory())) {
                    errors.append("CNFReader: unknown directory: " +directoryname+"\n");}
                else{
                    Pattern pattern;
                    try {pattern = Pattern.compile(regExprs);}
                    catch(PatternSyntaxException ex) {
                        errors.append("CNFReader: " + ex);
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
                " p cnf predicates clauses [symbolic]\n" +
                " clause1 0\n" +
                " clause2 0\n" +
                " ...\n" +
                " A clause is a blank or comma-separated list of literals (positive or negative numbers /= 0, or symbols).\n" +
                " \n" +
                " An extension of this format may contain clauses beginning with special characters:.\n" +
                " 'a':  means and:         'a 3 4 5' stands for 3 and 4 and 5.\n" +
                " 'o':  means or:          'o 3 4 5' stands for 3 or 4 or 5.\n" +
                " 'e':  means equivalence: 'e 4 5 -6' means that these three literals are equivalent.\n" +
                " '<=': means atleast:     '<= 2 p q r' means atleast two of p,q,r are true.\n" +
                " '>=': means atmost:      '>= 2 p q r' means atmost two of p,q,r are true.\n" +
                " '=':  means exactly:     '= 2 p q r' means exactly two of p,q,r are true.\n";
    }

    /** reads the cnf-file
     *
     * @param parameters a HashMap with key "file"
     * @param errors    for error massages
     * @param warnings  for warnings
     * @return  the new clauses
     */
    public static BasicClauseList generate(HashMap<String,Object> parameters,
                                           ProblemSupervisor problemSupervisor,
                                           StringBuffer errors, StringBuffer warnings) {
        StringBuilder info = new StringBuilder();
        File file = (File)parameters.get("file");
        String filename = file.getName();
        String place =  "CNFReader: File " + filename+": ";
        String prefix = "          ";
        BufferedReader reader;
        try {reader = new BufferedReader(new FileReader(file));}
        catch (FileNotFoundException e) {
            errors.append(place + "not found");
            return null;}
        String line;
        BasicClauseList bcl = new BasicClauseList();
        Integer predicates = 0;
        Symboltable symboltable = null;
        boolean symbolic = false;
        int lineNumber = 0;
        try{
        while((line = reader.readLine()) != null) {
            ++lineNumber;
            line = line.trim();
            if(line.isEmpty()) {continue;}
            if(line.startsWith("%")){continue;}
            if(line.startsWith("c")) {info.append(line.substring(1)).append("\n"); continue;}
            if(line.startsWith("p") && !line.endsWith("0")) { // p cnf predicates clauses symbolic
                String[] parts = line.split("\\s*( |,)\\s*");
                if(parts.length < 4) {
                    errors.append(place + "Illegal format of line '" + line+ "'\n").append(prefix).
                            append("It should be 'p cnf predicates clauses [symbolic]'\n");
                    return null;}
                if(!parts[1].equals("cnf")) {
                    errors.append(place + "'" + line + "' " + "indicates no cnf file");
                    return null;}

                predicates = Utilities.parseInteger(place + "'"+line+"' ", parts[2],errors);
                if(predicates == null) {return null;}
                if(predicates <= 0) {
                    errors.append(place + "Negative number of predicates: '" + parts[2] +"'");
                    return null;}
                bcl.predicates = predicates;

                if(parts.length == 5) {
                    if(parts[4].equals("symbolic")) {
                        symbolic = true;
                        symboltable = new Symboltable(predicates);
                        bcl.symboltable = symboltable;}
                    else {errors.append(place + "End of line '"+ line + "' should be 'symbolic'");
                        return null;}}
                continue;}
            if(predicates == 0) {
                errors.append(place + "p-line missing: 'p cnf predicates clauses [symbolic]'");
                return null;}

            if(!line.endsWith(" 0")) {
                errors.append(place + " line " + lineNumber + ": '" + line + "' does not end with '0'\n");
                continue;}
            String[] parts = line.split("\\s*( |,)\\s*");
            int startParts = 1;
            int typnumber;
            switch(line.charAt(0)) {
                case 'a': typnumber = ClauseType.AND.ordinal();          break;
                case 'o': typnumber = ClauseType.OR.ordinal();           break;
                case 'e': typnumber = ClauseType.EQUIV.ordinal();        break;
                case '<': typnumber = ClauseType.ATLEAST.ordinal();      break;
                case '>': typnumber = ClauseType.ATMOST.ordinal();       break;
                case '=': typnumber = ClauseType.EXACTLY.ordinal();      break;
                default: typnumber = ClauseType.OR.ordinal(); startParts = 0; break;}
            boolean isNumeric = ClauseType.isNumericType(typnumber);

            int length = parts.length + 1 + (isNumeric ? 1 : 0);
            int literalCounter = 1;
            int[] clause = new int[length];
            clause[0] = problemSupervisor.nextClauseId();
            clause[1] = typnumber;
            if(isNumeric) {
                Integer n = Utilities.parseInteger (place + "line " + lineNumber +": ",parts[1],errors);
                if(n == null) {errors.append("\n"); continue;}
                if(n < 1) {
                    errors.append(place + "line " + lineNumber  + ": '" + line + "' quantifier >= 1 required\n");
                    continue;}
                literalCounter = 2;
                clause[2] = n;}

            for(int i = startParts; i < parts.length-1; ++i) {
                String part = parts[i];
                if(symbolic) {
                    int sign = 1;
                    if(part.startsWith("-")) {sign = -1; part = part.substring(1);}
                    int literal = sign * symboltable.getPredicate(part);
                    if(literal == 0) {
                        errors.append(place + "line "+ lineNumber + ": predicate overflow: " + parts[i] +"\n");
                        literalCounter = 0; break;}
                    else {clause[++literalCounter] = literal;}}
                else {
                    Integer literal = Utilities.parseInteger (place+"line " + lineNumber +": ", parts[i],errors);
                    if(literal != null) {
                        if(literal == 0) {
                            errors.append(place + "line " + lineNumber + ": literal '" + literal + "' = 0\n");
                            literalCounter = 0; break;}
                        if(Math.abs(literal) > predicates) {
                            errors.append(place + "line " + lineNumber + ": |literal| '" + literal + "' > " +predicates +"\n");
                            literalCounter = 0; break;}
                        clause[++literalCounter] = literal;}
                    else {errors.append("\n"); literalCounter = 0; break;}}}
            if(literalCounter == 0) continue;
            bcl.addClause(clause);}
        errors.append(bcl.syntaxErrors.toString());
        bcl.info = info.toString();}
        catch(IOException ex) {
            errors.append(place + " IOException\n");
            return null;}
        return  bcl;}


}
