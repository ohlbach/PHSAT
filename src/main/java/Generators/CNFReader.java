package Generators;

import Datastructures.Clauses.BasicClauseList;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

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
 * A clause is a blank-separated list of literals (positive or negative numbers /= 0).
 * <br/>
 * An extension of this format may contain clauses beginning with 0.<br/>
 * 0 1 3 5 <br/>
 * for example means that the predicates 1,3 and 5 are disjoint. At most one of them can be true in a model.
 *
 */
public class CNFReader {

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
    public static ArrayList<HashMap<String,Object>> parseProblemParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings){
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
        st.append("  regExpr:   A regular expression to select files in the directory.");
        return st.toString();
    }

    /** reads the cnf-file
     *
     * @param parameters a HashMap with key "file"
     * @param errors    for error massages
     * @param warnings  for warnings
     * @return  the modified HashMap parameters with the new key "clauses".
     */
    public static HashMap<String,Object> generate(HashMap<String,Object> parameters, StringBuffer errors, StringBuffer warnings) {
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
        boolean disjointness = false;
        try{
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
            int start = 0;
            if(line.startsWith("d")) {
                literals.add(0);
                disjointness = true;
                start = 1;}
            String[] parts = line.split("\\s+");
            for(int i = start; i < parts.length-1; ++i) {
                Integer lit = Utilities.parseInteger (place,parts[i],errors);
                if(lit != null) {
                    if(line.startsWith("d") && lit < 0) {errors.append(place+ "only positive predicates are allowed, not " + lit+"\n");}
                    literals.add(lit);};}
            clauseList.add(literals);}}
        catch(IOException ex) {
            errors.append(place + " IOException\n");
            return null;}
        BasicClauseList bcl = new BasicClauseList(disjointness);
        bcl.predicates = predicates;
        int[] lits = null;
        for(ArrayList clause : clauseList) {
            if(disjointness) {
                if((int)clause.get(0) == 0) { // disjointness clause
                    lits = new int[clause.size()];
                    lits[0] = 1;
                    for(int i = 1; i< clause.size(); ++i) {lits[i] = (int)clause.get(i);}}
                else{lits = new int[clause.size()+1];
                    lits[0] = 0;  // normal clause
                    for(int i = 1; i<= clause.size(); ++i) {lits[i] = (int)clause.get(i-1);}}}
            else{
                lits = new int[clause.size()];
                for(int i = 0; i< clause.size(); ++i) {lits[i] = (int)clause.get(i);}}

            bcl.clauses.add(lits);}
        bcl.info = info.toString();
        parameters.put("clauses",bcl);
        return  parameters;}

}
