package Generators;

import Datastructures.Clauses.BasicClauseList;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import Utilities.Utilities;

/**
 * Created by ohlbach on 26.08.2018.
 */
public class CNFReader {

    private static HashSet<String> keys = new HashSet<>();
    static { // these are the allowed keys in the specification.
        for(String key : new String[]{"type", "file", "directory", "rexExpr"}) {
            keys.add(key);}}

    public static ArrayList<HashMap<String,Object>> parseProblemParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings){
        for(String key : parameters.keySet()) {
            if(!keys.contains(key)) {warnings.append("RandomClauseSetGenerator: unknown key in parameters: " + key + "\n");}}

        String files       = parameters.get("file");
        String directories = parameters.get("directoy");
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
                        if(file.isFile()) {
                            HashMap<String,Object> map = new HashMap<>();
                            map.put("file",file);
                            control.add(map);}}}} }

        if(regExprs != null) {
            if(directories == null) {errors.append("CNFReader: a directory must be specified for " + regExprs +"\n");}
            for(String directoryname : directories.split("\\s*,\\s*")) {
                File directory = new File(directoryname);
                if(!(directory.exists() && directory.isDirectory())) {
                    errors.append("CNFReader: unknown directory: " +directoryname+"\n");}
                else{}}}     // weiter machen

        return control;}

    public String help() {
        StringBuilder st = new StringBuilder();
        return st.toString();
    }

    public Object[] generate(HashMap<String,Object> parameters, StringBuffer errors, StringBuffer warnings) {
        StringBuilder info = new StringBuilder();
        File file = (File)parameters.get("file");
        String filename = file.getName();
        String place = "CNFReader: file " + filename;
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
            if(line.startsWith("c")) {info.append(line).append("\n"); continue;}
            if(line.startsWith("p")) {
                String[] parts = line.split("\\s+");
                if(parts.length < 4) {
                    errors.append(place + " illegal format of line " + line+"\n");
                    return null;}
                predicates = Utilities.parseInteger(place, parts[2],errors);
                continue;}
            if(predicates == null) {
                errors.append(place + " unknown number of predicates.\n");
                return null;}
            literals.clear();
            int start = 0;
            if(line.startsWith("d")) {
                literals.add(0);
                disjointness = true;
                start = 1;}
            String[] parts = line.split("\\s+");
            for(int i = start; i < parts.length; ++i) {
                Integer lit = Utilities.parseInteger (place,parts[i],errors);
                if(lit != null) {literals.add(lit);};}
            clauseList.add(literals);}}
        catch(IOException ex) {
            errors.append(place + " IOException\n");
            return null;}

        BasicClauseList bcl = new BasicClauseList(disjointness);
        int[] lits = null;
        for(ArrayList clause : clauseList) {
            if(disjointness) {
                if((int)clause.get(0) == 0) {
                    lits = new int[clause.size()];
                    lits[0] = 1;}  // disjointness clause
                else{lits = new int[clause.size()+1];
                    lits[0] = 0;}  // normal clause
                for(int i = 1; i< clause.size(); ++i) {lits[i] = (int)clause.get(i);}}
            else{
                lits = new int[clause.size()];
                for(int i = 0; i< clause.size(); ++i) {lits[i] = (int)clause.get(i);}}

            bcl.clauses.add(lits);}

        parameters.put("info",info.toString());
        return  new Object[]{clauseList,parameters,null};}

}
