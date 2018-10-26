package Utilities;

import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;

import java.io.*;
import java.lang.reflect.Array;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

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



    /** expands an Integer range into a list of Integers<br>
     * The formats are: <br>
     * - just an integer<br>
     * - a comma-separated list of integers. Ex. 3,5,-10<br>
     * - a range from to to. Ex. 3 to 10<br>
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


    /** expands a Float  range into a list of Integers<br>
     * The formats are: <br>
     * - just an integer<br>
     * - a comma-separated list of integers. Ex. 3.5,5,-10.1<br>
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

    /** computes the cross product of the given list.<br>
     * Example: [1,2] x [3,4] x {5,6] yields <br>
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
     * @param list1 a list
     * @param list2 a list
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
     * @param list1 a list
     * @param n     an object
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

    public static String tempFile(String directory, String filename) {
        String tmp = System.getenv("TEMP");
        File dirfile = Paths.get(tmp,directory).toFile();
        try {
            if (!dirfile.exists()) {dirfile.mkdir();}
            File file = Paths.get(tmp,directory,filename).toFile();
            if(file.exists()) {file.delete();}
            return file.getAbsolutePath();}
        catch(Exception exp) {exp.printStackTrace();}
        return null;}

    public static String readFile(String filename) {
        try {BufferedReader in = new BufferedReader(new FileReader(new File(filename)));
            StringBuilder st = new StringBuilder();
            String line;
            while((line = in.readLine()) != null) {st.append(line).append("\n");}
            in.close();
            return st.toString();}
    catch(Exception ex) {return null;}}


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
     * @param <T> a class
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


    /** yields the length  of the largest string
     *
     * @param strings some strings
     * @return the length of the largest string
     */
    public static int maxLength(String... strings) {
        int length = 0;
        for(String st : strings) {length = Math.max(length,st.length());}
        return length;}

    /** yields the length  of the largest decimal representation of the integers
     *
     * @param ints some integers
     * @return the length of the largest decimal representation of the integers.
     */
    public static int maxLength(int... ints) {
        int length = 0;
        for(int i : ints) {length = Math.max(length,Integer.toString(i).length());}
        return length;}

    /** yields the length  of the largest string representation of the objects
     *
     * @param toString a function mapping the objects to strings
     * @param objects some objects
     * @return the length of the largest string representation of the objects.
     */
    public static int maxLength(Function<Object,String> toString, Object[] objects) {
        int length = 0;
        for(Object o : objects) {length = Math.max(length,toString.apply(o).length());}
        return length;}

    /** yields the length  of the largest decimal representation of the objects
     *
     * @param toInteger a function mapping the objects to integers
     * @param objects some objects
     * @return the length of the largest decimal representation of the objects.
     */
    public static int maxSize(Function<Object,Integer> toInteger, Object[] objects) {
        int length = 0;
        for(Object o : objects) {length = Math.max(length,Integer.toString(toInteger.apply(o)).length());}
        return length;}

    /** yields the length  of the largest string representation of the objects
     *
     * @param toString a function mapping the objects to strings
     * @param objects some objects
     * @return the length of the largest string representation of the objects.
     */
    public static int maxLength( Collection objects,Function<Object,String> toString) {
        int length = 0;
        for(Object o : objects) {length = Math.max(length,toString.apply(o).length());}
        return length;}


    /** joins the strings generated by toString and separates them with the separator.
     *
     * @param collection  any collection of objects
     * @param separator   any string
     * @param toString    for mapping the objects in the collection to a string
     * @return the joined string separated by the separator.
     */
    public static String join(Collection collection, String separator, Function<Object,String> toString) {
        StringBuilder st = new StringBuilder();
        for(Object object : collection) {
            st.append(toString.apply(object)).append(separator);}
        String result = st.toString();
        return result.substring(0,result.length()-separator.length());}

    /** joins the strings generated by toString and separates them with the separator.
     *
     * @param collection  any collection of objects
     * @param separator   any string
     * @param toString    for mapping the objects in the collection to a string
     * @return the joined string separated by the separator.
     */
    public static String join(Object[] collection, String separator, Function<Object,String> toString) {
        StringBuilder st = new StringBuilder();
        for(Object object : collection) {
            st.append(toString.apply(object)).append(separator);}
        String result = st.toString();
        return result.substring(0,result.length()-separator.length());}

    /** calls the toString method. If the result ends with '.0', it is cut off.
     *
     * @param n any object
     * @return its String representation without '.0'
     */
    public static String numberString(Object n) {
        if(n == null) {return "";}
        String s = n.toString();
        if(s.endsWith(".0")) {s = s.substring(0,s.length()-2);}
        return s;}

    public static void printProperties() {
        System.out.println("System.getProperties()");
        for(Map.Entry<Object,Object> entry : System.getProperties().entrySet()) {
            System.out.println(entry.getKey().toString() + " = " + entry.getValue().toString());
        }
        System.out.println("\n\nSystem.getenv()");
        for(Map.Entry<String,String> entry : System.getenv().entrySet()) {
            System.out.println(entry.getKey() + " = " + entry.getValue());}
    }

    public static void printIndented(PrintStream stream, int indent, String string) {
        String blanks = "";
        if(indent > 0) {blanks = String.format("%"+indent+"s"," ");}
        for(String line : string.split("\\n")) {stream.printf(blanks); stream.println(line);}}

    /** This method extracts from a collection of arrays the n'th element
     *
     * @param collection a collection of arrays
     * @param n  an index
     * @param <T> the item type
     * @return the array of extracted items.
     */
    public static <T> T[] extract(Collection<T[]> collection, int n) {
        T[] list = null;
        int i = 0;
        for(T[] item : collection) {
            if(list == null){list =  (T[])Array.newInstance(item[0].getClass(),collection.size());}
            list[i++] = item[n];}
        return list;}

    /** checks if the array contains the item
     *
     * @param array an int-array
     * @param item an integer
     * @return the position of the item in the array, or -1
     */
    public static int contains(int[] array, int item) {
        for(int i = 0; i < array.length; ++i) {if(array[i] == item) {return i;}}
        return -1;}

    /** maps a filename to the entire path in IntelliJ's resources directory
     *
     * @param filename a filename
     * @return the entire path to the file.
     */
    public static String resourceFile(String filename) {
        return Paths.get(System.getProperty("user.dir"),"src", "main", "resources", filename).toString();}

    /** This method applies the predicate to all subsets of [0,...,n-1] (e.g. index sets for arrays).
     * The method goes from larger subsets to smaller subsets.
     * As soon as a predicate returns true for a particular index set, no further subsets of this index set are tested.
     * Therefore only the largest subsets for which the predicate returns true are tested.
     *
     * @param n         the size of the index set
     * @param predicate the predicate to be applied to the subsets.
     */
    public static void allSubsets(int n, Predicate<int[]> predicate) {
        HashSet<String> tuples = new HashSet<>();
        int[] indices = new int[n];
        for(int i = 0;  i < n; ++i) {indices[i] = i;}
        if(predicate.test(indices)) {return;}
        allSubsetsRec(indices,predicate, tuples);}

    /** This is the recursive part of the allSubsets method
     *
     * @param indices    the current index set to be tested (with all its subsets)
     * @param predicate  the predicate to be applied
     * @param tuples     stores blocked index sets
     */
    private static void allSubsetsRec(int[] indices, Predicate<int[]> predicate, HashSet<String> tuples) {
        int n = indices.length;
        if(n == 1) {return;}
        int[] newIndices = new int[n-1];
        for(int i = 1; i < n; ++i) {newIndices[i-1] = indices[i];}
        String st = Arrays.toString(newIndices);
        if(tuples.contains(st)){return;}
        tuples.add(st);
        if(predicate.test(newIndices)) {blockSubsets(newIndices,tuples); return;}
        for(int i = 0; i < n-1; ++i) {  // breadth first
            newIndices[i] = indices[i];
            st = Arrays.toString(newIndices);
            if(tuples.contains(st)){continue;}
            tuples.add(st);
            if(predicate.test(newIndices)) {blockSubsets(newIndices,tuples); continue;}}
        for(int i = 1; i < n; ++i) {newIndices[i-1] = indices[i];}
        for(int i = 0; i < n-1; ++i) {
            newIndices[i] = indices[i];
            allSubsetsRec(newIndices,predicate, tuples);}}

    /** blocks all subset of the given index set
     *
     * @param indices  the current index to be blocked, with all its subsets
     * @param tuples   all subsets are put into this set.
     */
    private static void blockSubsets(int[] indices, HashSet<String> tuples) {
        int n = indices.length;
        if(n == 1) {return;}
        String st = Arrays.toString(indices);
        tuples.add(st);
        int[] newIndices = new int[n-1];
        for(int i = 1; i < n; ++i) {newIndices[i-1] = indices[i];}
        tuples.add(Arrays.toString(newIndices));
        for(int i = 0; i < n-1; ++i) {
            newIndices[i] = indices[i];
            tuples.add(Arrays.toString(newIndices));
            blockSubsets(newIndices,tuples);}}



    public static void  main(String[] args) {
        allSubsets(6,(i -> {System.out.println(Arrays.toString(i));
            return Utilities.contains(i,2) < 0 ||  Utilities.contains(i,3) < 0;}));
        }
}

