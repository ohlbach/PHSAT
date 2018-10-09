package Utilities;

import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;

import java.io.File;
import java.io.InputStream;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.*;
import java.util.function.BiConsumer;

/**
 * Created by Ohlbach on 03.09.2018.
 */
public class Utilities {

    /** parses a string-representation of an integer
     *
     * @param place for error reporting
     * @param value the string to be parsed
     * @param errors for appending error messages
     * @return the parsed Integer, or null
     */
    public static Integer parseInteger(String place, String value, StringBuffer errors) {
        if(value == null) {return null;}
        try{return Integer.parseInt(value);}
        catch(NumberFormatException ex) {errors.append(place+": " + value + " is no integer.\n");}
        return null;}

    /** parses a string-representation of a float value
     *
     * @param place for error reporting
     * @param value the string to be parsed
     * @param errors for appending error messages
     * @return the parsed Float, or null
     */
    public static Float parseFloat(String place, String value, StringBuffer errors) {
        if(value == null) {return null;}
        try{return Float.parseFloat(value);}
        catch(NumberFormatException ex) {errors.append(place+": " + value + " is no float number.\n");}
        return null;}



    public static ArrayList<Boolean> parseBoolean(String place, String value, StringBuffer errors) {
        ArrayList<Boolean> bools = new ArrayList<>();
        if(value == null || value.equals("true")) {bools.add(true); return bools;}
        if(value.equals("false")) {bools.add(false); return bools;}
        String parts[] = value.split("\\s*,\\s*");
        if(parts.length != 2) {errors.append(place + ": unknown boolean values: " + value+"\n"); return null;}
        for(String part : parts) {
            if(part.equals("true")) {bools.add(true); continue;}
            if(part.equals("false")) {bools.add(false); continue;}
            errors.append(place + ": unknown boolean value: " + part+"\n"); return null;}
        return bools;}



    /** expands an Integer range into a list of Integers<br/>
     * The formats are: <br/>
     * - just an integer<br/>
     * - a comma-separated list of integers. Ex. 3,5,-10<br/>
     * - a range from to to. Ex. 3 to 10<br/>
     * - a range from to to step n. Ex: 3 to 10 step 2  The step must not be negative.
     *
     * @param place for error reporting
     * @param value a string to be parsed
     * @param errors for appending error messages
     * @return the expanded integer list
     */
    public static ArrayList<Integer> parseIntRange(String place, String value, StringBuffer errors) {
        if(value == null) {return null;}
        ArrayList<Integer> range = new ArrayList();
        try{Integer n =  Integer.parseInt(value);
            range.add(n);
            return range;}
        catch(NumberFormatException ex) {
            String[] parts;
            if(value.contains("to")) {
                if(!value.contains("step")) {// 3-5
                    parts = value.split("\\s*to\\s*",2);
                    Integer from = parseInteger(place,parts[0],errors);
                    Integer to = parseInteger(place,parts[1],errors);
                    if(from != null && to != null ) {
                        if(to < from) {errors.append(place+ "to < from: " + value);}
                        for(int n = from; n <= to; ++n) {range.add(n);}}
                    else {return null;}
                    return range;}
                else {  // 3-10 step 2
                    parts = value.split("\\s*(to|step)\\s*",3);
                    Integer from = parseInteger(place,parts[0],errors);
                    Integer to = parseInteger(place,parts[1],errors);
                    Integer step = parseInteger(place,parts[2],errors);
                    if(from != null && to != null && step != null) {
                        if(step < 0) {errors.append(place+": negative step " + step); return null;}
                        if(to < from) {errors.append(place+ "to < from: " + value); return null;}
                        for(int n = from; n <= to; n += step) {range.add(n);}}
                    else {return null;}
                    return range;}}
            for(String part : value.split("\\s*,\\s*")) {
                Integer n = parseInteger(place,part,errors);
                if(n != null) {range.add(n);}
                else {return null;}}
            return range;}}


    /** expands a Float  range into a list of Integers<br/>
     * The formats are: <br/>
     * - just an integer<br/>
     * - a comma-separated list of integers. Ex. 3.5,5,-10.1<br/>
     * - a range from to to step n. Ex: 3.5 to 10.2 step 2.6  The step must not be negative.
     *
     * @param place for error reporting
     * @param value a string to be parsed
     * @param errors for appending error messages
     * @return the expanded integer list
     */
    public static ArrayList<Float> parseFloatRange(String place, String value, StringBuffer errors) {
        ArrayList<Float> range = new ArrayList();
        try{Float n =  Float.parseFloat(value);
            range.add(n);
            return range;}
        catch(NumberFormatException ex) {
            String[] parts;
            if(value.contains("to")) {
                parts = value.split("\\s*(to|step)\\s*",3);
                    Float from = parseFloat(place,parts[0],errors);
                    Float to = parseFloat(place,parts[1],errors);
                    Float step = parseFloat(place,parts[2],errors);
                    if(from != null && to != null && step != null) {
                        if(step < 0) {errors.append(place+": negative step " + step); return null;}
                        if(to < from) {errors.append(place+ "to < from: " + value); return null;}
                        for(float n = from; n <= to; n += step) {range.add(n);}}
                    else {return null;}
                    return range;}
            for(String part : value.split("\\s*,\\s*")) {
                Float n = parseFloat(place,part,errors);
                if(n != null) {range.add(n);}
                else {return null;}}
            return range;}}

    /** computes the cross product of the given list.<br/>
     * Example: [1,2] x [3,4] x {5,6] yields <br/>
     * [[1, 3, 5], [1, 4, 5], [2, 3, 5], [2, 4, 5], [1, 3, 6], [1, 4, 6], [2, 3, 6], [2, 4, 6]]
     *
     *
     * @param lists the list for which the cross product ist to be computed
     * @return the cross product
     */
    public static ArrayList<ArrayList<Object>> crossProduct(ArrayList<Object>... lists) {
        switch(lists.length) {
            case 0:
                return null;
            case 1:
                ArrayList<ArrayList<Object>> list = new ArrayList<>();
                list.add(lists[0]);
                return list;
            case 2:
                return crossProductTwo(lists[0], lists[1]);
            default:
                list = crossProductTwo(lists[0], lists[1]);
                for(int i = 2; i < lists.length; ++i) {
                    list = addCrossProduct(list,lists[i]);}
                return list;}}

    /** computes the cross product of two lists of integers
     *
     * @param list1 a list of integers
     * @param list2 a list of integers
     * @return the cross product of the lists
     */
    private static ArrayList<ArrayList<Object>> crossProductTwo(ArrayList<Object> list1, ArrayList<Object> list2 ) {
        ArrayList<ArrayList<Object>> list = new ArrayList<>();
        ArrayList<Object> product = null;
        if(list1 == null  || list1.isEmpty()) {
            if(list2 == null || list2.isEmpty()) {
                product = new ArrayList<>();
                product.add(null); product.add(null);
                list.add(product);
                return list;}
            for(Object o : list2) {
                product = new ArrayList<>();
                product.add(null); product.add(o);
                list.add(product);
                return list;}}
        if(list2 == null || list2.isEmpty()) {
            for(Object o : list1) {
                product = new ArrayList<>();
                product.add(o); product.add(null);
                list.add(product);}
            return list;}
        for(Object n1 : list1) {
            for(Object n2 : list2) {
                product = new ArrayList<>();
                list.add(product);
                product.add(n1);product.add(n2);}}
        return list;}

    /** adds a new list to an already computed cross product
     *
     * @param list1
     * @param list2
     * @return the new cross product
     */
    private static ArrayList<ArrayList<Object>> addCrossProduct(ArrayList<ArrayList<Object>> list1, ArrayList<Object> list2) {
        if(list2 == null || list2.isEmpty()) {
            for(ArrayList<Object> list : list1) {list.add(null);}
            return list1;}
        ArrayList<ArrayList<Object>> list = new ArrayList<>();
        for(Object n : list2) {
            ArrayList<ArrayList<Object>> newlist = addProductElement(list1,n);
            list.addAll(newlist);}
        return list;}

    /** adds a single element to an already computed cross product.
     *
     * @param list1
     * @param n
     * @return the new cross product.
     */
    private static ArrayList<ArrayList<Object>> addProductElement(ArrayList<ArrayList<Object>> list1, Object n) {
        ArrayList<ArrayList<Object>> list = new ArrayList<>();
        for(ArrayList<Object> elements : list1) {
            ArrayList<Object> clones = (ArrayList<Object>)elements.clone();
            clones.add(n);
            list.add(clones);}
        return list;}

    /** creates a temporary file in a temporary directory and writes the text into that file.
     *  An existing file with that name is deleted.
     *
     *
     * @param directory the name of the temporary directory
     * @param filename  the name of the file
     * @param text      the text to be written.
     * @return the File object for the new file.
     */
    public static File writeTmpFile(String directory, String filename, String text) {
        String tmp = System.getenv("TEMP");
        File dirfile = Paths.get(tmp,directory).toFile();
        try {
            if (!dirfile.exists()) {dirfile.mkdir();}
            File file = Paths.get(tmp,directory,filename).toFile();
            if(file.exists()) {file.delete();}
            PrintWriter writer = new PrintWriter(file);
            writer.append(text);
            writer.close();
            return file;}
        catch(Exception exp) {exp.printStackTrace();}
    return null;}

    public static void clearTmpDirectory(String directory) {
        String tmp = System.getenv("TEMP");
        File dirfile = Paths.get(tmp,directory).toFile();
        try {
            if (!dirfile.exists()) {return;}
            for(File file : dirfile.listFiles()) {file.delete();}}
        catch(Exception exp) {exp.printStackTrace();}}

    /** generates a BiConsumer which prints a message to System.out
     *
     * @return a BiConsumer which prints a message to System.out
     */
    public static BiConsumer<String,String> stdoutLogger() {
        return ((String id, String message) -> {System.out.printf(id);
            System.out.printf(": ");
            System.out.println(message);});}

    public static Clause makeClause(String id, String literals) {
        String[] lits = literals.split("\\s*(,| )\\s*");
        Clause clause = new Clause(id,lits.length);
        for(String lit : lits) {
            clause.addCLiteral(new CLiteral(Integer.parseInt(lit)));}
        return clause;
    }

    /** computes the intersection of to tree sets
     *
     * @param set1 a TreeSet
     * @param set2 a TreeSet
     * @return a list of intersecting objects, or null
     */
    public static <T> ArrayList<T> intersection(TreeSet<T> set1, TreeSet<T> set2) {
        ArrayList<T> intersects = null;
        for(T i1 : set1) {
            if(set2.contains(i1)) {
                if(intersects == null) {intersects = new ArrayList<>();}
                intersects.add(i1);}}
        return intersects;}

    public static void  main(String[] args) {
        for(Map.Entry  entry : System.getProperties().entrySet()) {
            System.out.println(entry.getKey() + " = "+ entry.getValue());}
    }
}
