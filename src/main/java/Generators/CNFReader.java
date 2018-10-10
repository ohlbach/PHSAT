package Generators;

import Datastructures.Clauses.BasicClauseList;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import Datastructures.Clauses.ClauseType;
import Utilities.Utilities;

/**
 * Created by ohlbach on 26.08.2018.
 * <br/>
 * This class is for reading cnf-files. <br/>
 * A standard cnf-file has the following structure:<br/>
 * c comment<br/>
 * c comment<br/>
 * p cnf predicates clauses <br/>
 * clause1 0 <br/>
 * clause2 0 <br/>
 * ...<br/>
 * A clause is a blank or comma-separated list of literals (positive or negative numbers /= 0).
 * <br/>
 * An extension of this format may contain clauses beginning with special characters:.<br/>
 * 'd': means disjunction:  'd 1 3 5' means 1,3,5 are disjoint literals (at most one of them can be true).<br/>
 * 'e': means equivalences: 'e 4 5 -6' means that these three literals are equivalent.<br/>
 * 'x': means exclusive-or: 'x 3 4 5' means 3 xor 4 xor 5 (exactly one of them must be true).<br/>
 * 'a': means and:          'a 3 4 5' stands for 3 and 4 and 5.<br/>
 */
public final class CNFReader {

    private static HashSet<String> keys = new HashSet<>(); // contains the allowed keys in the specification.
    static { // these are the allowed keys in the specification.
        for(String key : new String[]{"type", "file", "directory", "regExpr"}) {
            keys.add(key);}}

    /** parses a HashMap with key-value pairs:<br/>
     * file: a comma separated list of pathnames<br/>
     * directory: a comma separated list of directories (all .cnf files in this directory are adressed) <br/>
     * regExpr: a regular expression: All files in the directories matching the expression are addressed
     *
     * @param parameters  the parameters with the keys "file", "directory", "regExpr"
     * @param errors      for error messages
     * @param warnings    for warnings
     * @return            a list of HashMaps with key "file" and value the corresponding File object.
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {warnings.append("RandomClauseSetGenerator: unknown key in parameters: " + key + "\n");}}

        String files       = parameters.get("file");
        String directories = parameters.get("directory");
        String regExprs    = parameters.get("regExpr");

        ArrayList<HashMap<String,Object>> control = new ArrayList<>();

        if(files != null) {
            for(String filename : files.split("\\s*,\\s*")) {
                File file = new File(filename);
                if(!file.exists()) {errors.append("CNFReader: unknown file: " +filename+"\n");}
                else {HashMap<String,Object> map = new HashMap<>();
                      map.put("file",file);
                      control.add(map);}}}

        if(directories != null && regExprs == null) {
            for(String directoryname : directories.split("\\s*,\\s*")) {
                File directory = new File(directoryname);
                if(!(directory.exists() && directory.isDirectory())) {
                    errors.append("CNFReader: unknown directory: " +directoryname+"\n");}
                else {
                    for(File file : directory.listFiles()) {
                        if(file.isFile() && file.getName().endsWith(".cnf")) {
                            HashMap<String,Object> map = new HashMap<>();
                            map.put("file",file);
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
                        errors.append("CNFReader: " + ex.toString());
                        continue;}
                    for(File file: directory.listFiles(pathname -> pattern.matcher(pathname.getName()).matches())) {
                        HashMap<String,Object> map = new HashMap<>();
                        map.put("file",file);
                        control.add(map);}}}}

        return control;}

    /** generates a help-string
     *
     * @return a help-string
     */
    public static String help() {
        StringBuilder st = new StringBuilder();
        st.append("CNFReader for reading CNF-Files.\n");
        st.append("The parameters are:\n");
        st.append("  file:      A single filename or a comma-sparated list of filenames.\n");
        st.append("  directory: A single directory name or a comma-separated list of directory names.\n");
        st.append("             All files in the directory ending with .cnf are loaded, unless regExpr is defined.\n");
        st.append("  regExpr:   A regular expression to select files in the directory.\n\n");
        st.append("A standard cnf-file has the following structure:\n" +
                " c comment\n" +
                " c comment\n" +
                " p cnf predicates clauses\n" +
                " clause1 0\n" +
                " clause2 0\n" +
                " ...\n" +
                " A clause is a blank or comma-separated list of literals (positive or negative numbers /= 0).\n" +
                " \n" +
                " An extension of this format may contain clauses beginning with special characters:.\n" +
                " 'd': means disjunction:  'd 1 3 5' means 1,3,5 are disjoint literals (at most one of them can be true).\n" +
                " 'e': means equivalences: 'e 4 5 -6' means that these three literals are equivalent.\n" +
                " 'x': means exclusive-or: 'x 3 4 5' means 3 xor 4 xor 5 (exactly one of them must be true).\n" +
                " 'a': means and:          'a 3 4 5' stands for 3 and 4 and 5.\n");
        return st.toString();
    }

    /** reads the cnf-file
     *
     * @param parameters a HashMap with key "file"
     * @param errors    for error massages
     * @param warnings  for warnings
     * @return  the new clauses
     */
    public static BasicClauseList generate(HashMap<String,Object> parameters, StringBuffer errors, StringBuffer warnings) {
        StringBuilder info = new StringBuilder();
        File file = (File)parameters.get("file");
        String filename = file.getName();
        String place = "CNFReader: file " + filename+": ";
        ArrayList<Integer> literals = new ArrayList();
        ArrayList<ArrayList<Integer>> clauseList = new ArrayList<>();
        BufferedReader reader = null;
        try {reader = new BufferedReader(new FileReader(file));}
        catch (FileNotFoundException e) {
            errors.append(place + " not found");
            return null;}
        String line;
        Integer predicates = null,clauses = null;
        try{
            int number = 0;
        while((line = reader.readLine()) != null) {
            line = line.trim();
            if(line.isEmpty()) {continue;}
            if(line.startsWith("c")) {info.append(line.substring(1)).append("\n"); continue;}
            if(line.startsWith("p")) {
                String[] parts = line.split("\\s+");
                if(parts.length < 4) {
                    errors.append(place + " illegal format of line " + line+"\n");
                    return null;}
                predicates = Utilities.parseInteger(place, parts[2],errors);
                continue;}
            if(line.startsWith("%")){break;}
            if(predicates == null) {
                errors.append(place + " unknown number of predicates.\n");
                return null;}
            if(!line.endsWith(" 0")) {
                errors.append(place + " line does not end with 0: " + line + "\n");
                return null;}
            literals = new ArrayList();
            literals.add(++number);
            int start = 1;
            switch(line.charAt(0)) {
                case 'd': literals.add(ClauseType.DISJOINT.ordinal());     break;
                case 'e': literals.add(ClauseType.EQUIV.ordinal());        break;
                case 'x': literals.add(ClauseType.XOR.ordinal());          break;
                case 'a': literals.add(ClauseType.AND.ordinal());          break;
                default: literals.add(ClauseType.OR.ordinal()); start = 0; break;}
            String[] parts = line.split("\\s*( |,)\\s*");
            for(int i = start; i < parts.length-1; ++i) {
                Integer lit = Utilities.parseInteger (place,parts[i],errors);
                if(lit != null) {literals.add(lit);};}
            clauseList.add(literals);}}
        catch(IOException ex) {
            errors.append(place + " IOException\n");
            return null;}
        BasicClauseList bcl = new BasicClauseList();
        bcl.predicates = predicates;
        int[] lits = null;
        for(ArrayList clause : clauseList) {
            lits = new int[clause.size()];
            for(int i = 0; i< clause.size(); ++i) {lits[i] = (int)clause.get(i);}
            bcl.addClause(lits);}
        bcl.info = info.toString();
        return  bcl;}

}
