package Utilities;

import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.io.*;
import java.lang.reflect.Array;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Created by Ohlbach on 03.09.2018.
 */
public class Utilities {


    /** for storing unused IntArrayLists.*/
    private static final Stack<IntArrayList> listReserve = new Stack<>();

    /** returns a free IntArrayList
     *
     * @return a free IntArrayList
     */
    public static IntArrayList popIntArrayList() {
        synchronized (listReserve) {
            if(listReserve.isEmpty()) return new IntArrayList();
            IntArrayList list = listReserve.pop();
            list.clear();
            return list;}}

    /** pushs a free IntArrayList to the stack */
    public static void pushIntArrayList(IntArrayList list) {
        synchronized (listReserve) {listReserve.push(list);}}

    /** parses a string-representation of an integer
     *
     * @param place for error reporting
     * @param value the string to be parsed
     * @param errors for appending error messages
     * @return the parsed Integer, or null
     */
    public static Integer parseInteger(String place, String value, StringBuilder errors) {
        if(value == null) {return null;}
        if(place == null) place = ""; else place += " ";
        try{return Integer.parseInt(value);}
        catch(NumberFormatException ex) {errors.append(place+"'" + value + "' is no integer\n");}
        return null;}

    /** trys to pars a string as integer.
     *
     * @param value the string to be parsed
     * @return null or the parsed integer.
     */
    public static Integer parseInteger(String value) {
        if(value == null) {return null;}
        try{return Integer.parseInt(value);}
        catch(NumberFormatException ex) {return null;}}

    /** trys to pars a string as integer.
     *
     * @param value the string to be parsed
     * @param minimum the number should not be smaller than this minimum
     * @param errors for appending error messages.
     * @return 0 or the parsed int-value.
     */
    public static int parseInteger(String value, int minimum, StringBuilder errors) {
        if(value == null) {
            errors.append("Number-String is empty");
            return 0;}
        int n = 0;
        try{ n = Integer.parseInt(value);}
        catch(NumberFormatException ex) {errors.append(ex.getMessage());}
        if(n < minimum) {errors.append(value + " < " + minimum+"\n"); return 0;}
        return n;}

    /** parses a string-representation of a float value
     *
     * @param place for error reporting
     * @param value the string to be parsed
     * @param errors for appending error messages
     * @return the parsed Float, or null
     */
    public static Float parseFloat(String place, String value, StringBuilder errors) {
        if(value == null) {return null;}
        try{return Float.parseFloat(value);}
        catch(NumberFormatException ex) {errors.append(place+": " + value + " is no float number.\n");}
        return null;}



    public static ArrayList<Boolean> parseBoolean(String place, String value, StringBuilder errors) {
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
    public static ArrayList<Integer> parseIntRange(String place, String value, StringBuilder errors) {
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
                        if(to < from) {errors.append(place+ " to < from: " + value);}
                        for(int n = from; n <= to; ++n) {range.add(n);}}
                    else {return null;}
                    return range;}
                else {  // 3-10 step 2
                    parts = value.split("\\s*(to|step)\\s*",3);
                    Integer from = parseInteger(place,parts[0],errors);
                    Integer to = parseInteger(place,parts[1],errors);
                    Integer step = parseInteger(place,parts[2],errors);
                    if(from != null && to != null && step != null) {
                        if(step < 0) {errors.append(place+"negative step " + step); return null;}
                        if(to < from) {errors.append(place+ "to < from: " + value); return null;}
                        for(int n = from; n <= to; n += step) {range.add(n);}}
                    else {return null;}
                    return range;}}}
            for(String part : value.split("(\\s+|\\s*,\\s*)")) {
                Integer n = parseInteger(place,part,errors);
                if(n != null) {range.add(n);}
                else {
                    errors.append(place +" the format should be: integer or comma separated integer or 'integer to integer'." );
                    return null;
                    }}
            return range;}

    /** expands an Integer range into a list of Integers<br>
     * The formats of a range are: <br>
     * - just an integer<br>
     * - a comma-separated list of ranges. Ex. 3,5,-10<br>
     * - a range start to end. start &le; end. Ex. 3 to 10. <br>
     * - a range start to end step n. Ex: 3 to 10 step 2  The step must not be negative.<br>
     * Double occurrences are removed.<br>
     * The final numbers are sorted in increasingly.
     *
     * @param value a string to be parsed
     * @param errors for appending error messages
     * @return the expanded integer list
     */
    public static IntArrayList parseIntRange(String value, StringBuilder errors) {
        if(value == null) {return null;}
        IntArrayList range = new IntArrayList();
        boolean erraneous = false;
        Integer step,from,to;
        for(String part : value.split("\\s*,\\s*")) {
            String[] parts = part.split("\\s*(to|step)\\s*",3);
            switch (parts.length) {
                case 1: from = parseInteger(null,parts[0],errors);
                    if(from == null) {erraneous = true; continue;}
                    if(!range.contains(from)) range.add(from);
                    break;
                case 2:
                    if(part.contains("step")) {
                            errors.append(part + " has not the form like 3 to 10 step 2\n" ); erraneous = true; continue;}
                    from = parseInteger(null,parts[0],errors);
                    to = parseInteger(null,parts[1],errors);
                    if(from == null || to == null) {erraneous = true; continue;}
                    if(to < from) {errors.append("to < from: " + part); erraneous = true; continue;}
                    for(int n = from; n <= to; ++n) {if(!range.contains(n))range.add(n);}
                    break;
                default:
                    from = parseInteger(null,parts[0],errors);
                    to = parseInteger(null,parts[1],errors);
                    step = parseInteger(null,parts[2],errors);
                    if(from == null || to == null || step == null) {erraneous = true; continue;}
                    if(step < 0) {errors.append("negative step: " + step+"\n"); erraneous = true; continue;}
                    if(to < from) {errors.append("to < from: " + part+"\n");    erraneous = true; continue;}
                    for(int n = from; n <= to; n += step) {if(!range.contains(n))range.add(n);}}}
        if(erraneous) return null;
        range.sort(Comparator.comparingInt(i->i));
        return range;}
    /** expands an Integer range into a list of Integers<br>
     * The formats of a range are: <br>
     * - just an integer<br>
     * - a comma-separated list of ranges. Ex. 3,5,-10<br>
     * - a range start to end. start &le; end. Ex. 3 to 10. <br>
     * - a range start to end step n. Ex: 3 to 10 step 2  The step must not be negative.<br>
     * Double occurrences are removed.<br>
     * The final numbers are sorted in increasingly.
     *
     * @param value a string to be parsed
     * @param minimum the smallest allowed value.
     * @param errors for appending error messages
     * @return the expanded integer list or null
     */
    public static IntArrayList parseIntRange(String value, int minimum, StringBuilder errors) {
        IntArrayList range = parseIntRange(value,errors);
        if(range == null) return null;
        if(range.getInt(0) < minimum) {
            errors.append("The values in " + range.toString() + " must be >= " + minimum+"\n");
            return null;}
        return range;}

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
    public static ArrayList<Float> parseFloatRange(String place, String value, StringBuilder errors) {
        ArrayList<Float> range = new ArrayList();
        float factor = 1000;
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
                        for(float n = from*factor; n <= to*factor; n += step*factor) {range.add(n/factor);}}
                    else {return null;}
                    return range;}
            for(String part : value.split("\\s*[, ]\\s*")) {
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

    /** computes the cross product of two lists of objects
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

    /** extends a pathname with the user's homedirectory.
     *
     * @param pathname a pathname, possibly starting with 'home'
     * @return a Path where 'home' is replaced by the user's homedirectory.
     */
    public static Path pathWithHome(String pathname) {
        return pathname.startsWith("home") ?
            Paths.get(System.getProperty("user.home"),pathname.substring(4)) :
            Paths.get(pathname); }

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
        try {BufferedReader in = new BufferedReader(new FileReader(filename));
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


    /** joins the strings generated by description and separates them with the separator.
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

    /** joins the strings generated by description and separates them with the separator.
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

    /** calls the description method. If the result ends with '.0', it is cut off.
     *
     * @param n any object
     * @return its String representation without '.0'
     */
    public static String numberString(Object n) {
        if(n == null) {return "";}
        String s = n.toString();
        if(s.endsWith(".0")) {s = s.substring(0,s.length()-2);}
        return s;}

    /** appends the two arrays into a new array
     *
     * @param a1 an int-array
     * @param a2 an int-array
     * @return the appended arrays
     */
    public static int[] appendArrays(int[] a1, int[] a2) {
        int length = a1.length;
        int[] a = new int[length+a2.length];
        System.arraycopy(a1,0,a,0,length);
        System.arraycopy(a2,0,a,length,a2.length);
        return a;}

    /** appends the arrays into a new array
     *
     * @param arrays a list of int-arrays
     * @return the appended arrays
     */
    public static int[] appendArrays(Collection<int[]> arrays) {
        int length = 0;
        for(int[] array : arrays) {length+= array.length;}
        int[] a = new int[length];
        length = 0;
        for(int[] array : arrays) {
            System.arraycopy(array,0,a,length,array.length);
            length += array.length;}
        return a;}

    /** turns the list into a separator-separated string. Duplicates are dropped.
     *
     * @param list      a list of ints
     * @param separator for separating the ints
     * @return the list as a string, without duplicates.
     */
    public static String toString(IntArrayList list, String separator) {
        if(list == null || list.isEmpty()) {return "";}
        StringBuilder st = new StringBuilder();
        int size = list.size()-1;
        for(int i = 0; i <= size; ++i) {
            int item = list.getInt(i);
            boolean found = false;
            for(int j = 0; j < i; ++j) {if(item == list.getInt(j)) {found = true; break;}}
            if(!found) {
                st.append(Integer.toString(item));
                if(i < size) st.append(separator);}}
        return st.toString();}

    /** removes all duplicates from an IntArray
     *
     * @param list an IntArray
     */
    public static void removeDuplicates(IntArrayList list) {
        for(int i = 0; i < list.size(); ++i) {
            int item = list.getInt(i);
            for(int j = 0; j < i; ++j) {
                if(item == list.getInt(j)) {list.removeInt(i--); break;}}}}

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
     * @return the clausePosition of the item in the array, or -1
     */
    public static int contains(int[] array, int item) {
        for(int i = 0; i < array.length; ++i) {if(array[i] == item) {return i;}}
        return -1;}

    /** checks if the array contains the item.
     *
     * @param array an int-array.
     * @param item an integer.
     * @return +1 if the item is contained in the array, -1 if -item is contained in the array, otherwise 0.
     */
    public static int contains(IntArrayList array, int item) {
        for(int i = 0; i < array.size(); ++i) {
            int item1 = array.getInt(i);
            if(item == item1) return +1;
            if(item == -item1) return -1;}
        return 0;}


    /** maps a filename to the entire path in IntelliJ's resources directory
     *
     * @param filename a filename
     * @return the entire path to the file.
     */
    public static String resourceFile(String filename) {
        return Paths.get(System.getProperty("user.dir"),"src", "main", "resources", filename).toString();}




    /** generates the largest subsets of 0...n-1 which satisfy the predicate.
     * The subsets are coded as int-values.
     *
     * @param n           the limit
     * @param predicate   to be applied to an int-integer
     * @return            a list of subsets of 0...n-1, coded as long, which satisfy the predicate.
     */
    public static ArrayList<Integer> largestSubsetsInt(int n, Predicate<Integer> predicate) {
        ArrayList<Integer> list = new ArrayList<>();
        TreeSet<Integer> candidates = new TreeSet<>();
        for(int i = 0; i < n; ++i) {
            for(int j = i+1; j < n; ++j) {
                int pair = (1 << i) | (1 << j) ;
                if(predicate.test(pair)) {candidates.add(pair);}}}
        if(candidates.isEmpty()) {return list;}
        TreeSet<Integer> nextCandidates = new TreeSet<>();
        for(int i = 2; i < n; ++i) {
            for(Integer j : candidates) {
                if(insertOneOfInt(n,j,predicate,nextCandidates) // && !isSubsetInt(j,nextCandidates)
                ) {list.add(j);}}
            candidates = nextCandidates;
            nextCandidates = new TreeSet<>();}
        list.addAll(candidates);
        return list;
    }

    /** inserts in the subset, coded be tuple, one new bit.
     *
     * @param n      the limit
     * @param tuple  the set, coded as int
     * @param predicate to be applied to a int-value
     * @param tuples  the already checked subsets
     * @return        true if tuple is the maximal subset satisfying the predicate.
     */
    private static boolean insertOneOfInt(int n, int tuple, Predicate<Integer> predicate, TreeSet<Integer> tuples) {
        boolean someSubsumed = false;
        boolean someNew = false;
        for(int i = 0; i < n; ++i) {
            int mask = 1 << i;
            if((tuple & mask) == 0) {
                int j = tuple | mask;
                if(tuples.contains(j)) {someSubsumed = true; continue;}

                if(predicate.test(j)) {someNew = true; tuples.add(j);}}}
        return !someSubsumed && !someNew;}

    /** generates the largest subsets of 0...n-1 which satisfy the predicate.
     * The subsets are coded as long-values.
     *
     * @param n           the limit
     * @param predicate   to be applied to a long-integer
     * @return            a list of subsets of 0...n-1, coded as long, which satisfy the predicate.
     */
    public static ArrayList<Long> largestSubsetsLong(int n, Predicate<Long> predicate) {
        ArrayList<Long> list = new ArrayList<>();
        TreeSet<Long> candidates = new TreeSet<>();
        for(int i = 0; i < n; ++i) {
            for(int j = i+1; j < n; ++j) {
                long pair = (1 << i) | (1 << j) ;
                if(predicate.test(pair)) {candidates.add(pair);}}}
        if(candidates.isEmpty()) {return list;}
        TreeSet<Long> nextCandidates = new TreeSet<>();
        for(int i = 2; i < n; ++i) {
            for(Long j : candidates) {
                if(insertOneOfLong(n,j,predicate,nextCandidates) // && !isSubsetInt(j,nextCandidates)
                        ) {list.add(j);}}
            candidates = nextCandidates;
            nextCandidates = new TreeSet<>();}
        list.addAll(candidates);
        return list;
    }

    /** inserts in the subset, coded be tuple, one new bit.
     *
     * @param n      the limit
     * @param tuple  the set, coded as long
     * @param predicate to be applied to a long-value
     * @param tuples  the already checked subsets
     * @return        true if tuple is the maximal subset satisfying the predicate.
     */
    private static boolean insertOneOfLong(int n, long tuple, Predicate<Long> predicate, TreeSet<Long> tuples) {
        boolean someSubsumed = false;
        boolean someNew = false;
        for(int i = 0; i < n; ++i) {
            int mask = 1 << i;
            if((tuple & mask) == 0) {
                long j = tuple | mask;
                if(tuples.contains(j)) {someSubsumed = true; continue;}
                if(predicate.test(j)) {someNew = true; tuples.add(j);}}}
        return !someSubsumed && !someNew;}


    /** This method turns an array of ints < Integer.MAX_VALUE into an int.
     * Each number in the array determines the 1 in the bitarray of the result.
     *
     * @param array an array of ints < INTEGER.MAX_VALUE
     * @return the corresponding int.
     */
    public static int toInt(int[] array) {
        int result = 0;
        int mask = 1;
        for(int i : array) {result |= mask << i;}
        return result;}



    /** This method turns a an int as bitarray into an array of ints.
     * Each 1 in the int causes its clausePosition to be inserted into the array.
     *
     * @param list an int as bitlist
     * @return the corresponding int-array
     */
    public static int[] toArray(int list) {
        int i = list;
        int mask = 1;
        int length = 0;
        while (i != 0) {
            if((i & mask) != 0) {++length; i &= ~mask;}
            mask <<= 1;}
        int[] result = new int[length];
        mask = 1;
        int k = -1;
        int j = 0;
        while (list != 0) {
            if((list & mask) != 0) {list &= ~mask; result[++k] = j;}
            ++j;
            mask <<= 1;}
        return result;}

    /** This method takes the integer i as bitmap and applies the predicate to the clausePosition of the 1's in the bitmap.
     * As soon as the predicate returns true, it stopps
     *
     * @param i           a bitmap
     * @param predicate   to be applied to the positions of the 1's in the bitmap
     * @return            true as soon as the first application of the predicate returns true.
     */
    public static boolean forSome(int i, Predicate<Integer> predicate) {
        int index = 0;
        int mask = 1;
        while(i != 0) {
            if((i & mask) != 0) {if(predicate.test(index)) {return true;}}
            i &= ~mask;
            ++index;
            mask <<= 1;}
        return false;}

    /** This method takes the long integer i as bitmap and applies the predicate to the clausePosition of the 1's in the bitmap.
     * As soon as the predicate returns true, it stopps
     *
     * @param i           a bitmap
     * @param predicate   to be applied to the positions of the 1's in the bitmap
     * @return            true as soon as the first application of the predicate returns true.
     */
    public static boolean forSome(long i, Predicate<Integer> predicate) {
        int index = 0;
        long mask = 1;
        while(i != 0) {
            if((i & mask) != 0) {if(predicate.test(index)) {return true;}}
            i &= ~mask;
            ++index;
            mask <<= 1;}
        return false;}

    private static void pl(String s,Collection<Integer> list) {
        System.out.println(s);
        for(int l : list) {System.out.println(Arrays.toString(toArray(l)));}}

    private static void pll(String s,Collection<Long> list) {
        System.out.println(s);
        for(long l : list) {System.out.println(Arrays.toString(toArray((int)l)));}}


    /** checks if the string codes an integer
     *
     * @param s a string
     * @return true if the string codes an integer
     */
    public static boolean isInteger(String s) {
        try{Integer.parseInt(s);}
        catch(NumberFormatException ex) {return false;}
        return true;}

    /** computes the union of the first IntArray with the second IntArray
     *
     * @param list1 an IntArray
     * @param list2 an IntArray or null
     * @return the union of both lists
     */
    public static IntArrayList joinIntArrays1(IntArrayList list1, IntArrayList list2) {
        if(list1 == null || list1.isEmpty()) {return list2;}
        if(list2 == null || list2.isEmpty()) {return list1;}
        IntArrayList newList = list1.clone();
        for(int item : list2) {if(!list1.contains(item)) newList.add(item);}
        return newList;}

    /** joins two IntArrays and and sorts the result
     *
     * @param list1 a IntArrayList
     * @param list2 a IntArrayList
     * @return the joined but unsorted list.
     */
    public static IntArrayList joinIntArrays(IntArrayList list1, IntArrayList list2) {
        if(list1 == null || list1.isEmpty()) {return list2;}
        if(list2 == null || list2.isEmpty()) {return list1;}
        IntArrayList newList = list1.clone();
        for(int item : list2) {if(!list1.contains(item)) newList.add(item);}
        return newList;}

    /** joins two IntArrays and and sorts the result
     *
     * @param list1 a IntArrayList
     * @param list2 a IntArrayList
     * @return the joined and sorted list.
     */
    public static IntArrayList joinIntArraysSorted(IntArrayList list1, IntArrayList list2) {
        if(list1 == null || list1.isEmpty()) {
            if(list2 == null || list2.isEmpty()) return null;
            else {list2.sort((i,j)-> Integer.compare(i,j)); return list2;}}
        if(list2 == null || list2.isEmpty()) {
            list1.sort((i,j)-> Integer.compare(i,j)); return list1;}
        IntArrayList newList = list1.clone();
        for(int item : list2) {if(!list1.contains(item)) newList.add(item);}
        newList.sort((i,j)-> Integer.compare(i,j));
        return newList;}

    /** sorts the IntArray.
     *
     * @param list an IntArray
     * @return the sorted IntArray.
     */
    public static IntArrayList sortIntArray(IntArrayList list) {
        if(list == null) return null;
        list.sort((i,j)-> Integer.compare(i,j));
        return list;}

    /** searchs the IntArrayList for the item and returns its index in the array.
     *
     * @param array an IntArrayList.
     * @param item an integer.
     * @return 0 or the index of the item in the array.
     */
    public static int indexOf(IntArrayList array, int item) {
        for(int i = 0; i < array.size(); ++i) {
            if(array.getInt(i) == item) return i;}
        return 0;}

    /** adds the elements of the second list to the first list
     *
     * @param list1 an IntArray
     * @param list2 an IntArray or null
     * @return the extended list1
     */
    public static IntArrayList addIntArray(IntArrayList list1, IntArrayList list2) {
        if(list1 == null || list1.isEmpty()) {return list2;}
        if(list2 == null || list2.isEmpty()) {return list1;}
        for(int item : list2) {if(!list1.contains(item)) list1.add(item);}
        return list1;}

    /** computes the union of the IntArrayLists.
     *
     * @param intArrayLists some IntArrayLists.
     * @return null or the union of all the lists.
     */
    public static IntArrayList unionIntArrayLists(IntArrayList... intArrayLists) {
        IntArrayList newList = new IntArrayList();
        for(IntArrayList list : intArrayLists) {
            if(list != null) {
                for(int item : list) {if(!newList.contains(item)) newList.add(item);}}}
        return newList.isEmpty() ? null : newList;}

    /** adds the item to the list, if it is not yet contained in the list
     *
     * @param list an IntArrayList
     * @param item an int
     * @return the list itself
     */
    public static IntArrayList addInt(IntArrayList list, int item) {
        if(list == null) {
            list = new IntArrayList();
            list.add(item);
            return list;}
        if(!list.contains(item)) {list.add(item);}
        return list;}

    /** replaces all occurrences of the old int by the new int
     *
     * @param list    an IntArrayList
     * @param oldItem an int
     * @param newItem an int
     * @return the list itself
     */
    public static IntArrayList replaceBy(IntArrayList list, int oldItem, int newItem) {
        if(list == null) return null;
        for(int i = 0; i < list.size(); ++i) {
            if(list.getInt(i) == oldItem) {list.set(i,newItem);}}
        return list;}


    /** checks if list1 is a subset of or equal to list2 (null means empty list)
     *  An empty list is a subset of everything
     *
     * @param list1 null or a IntArrayList
     * @param list2 null or an IntArrayList
     * @return true if list1 is a subset of list2
     */
    public static boolean isSubset(IntArrayList list1, IntArrayList list2) {
        if(list1 == null) {return true;}
        if(list1.size() > list2.size()) {return false;}
        for(int item : list1) {
            if(!list2.contains(item)) {return false;}}
        return true;}

    /** checks if the array1 is a subset of the array2 (using == on the objects)
     *
     * @param array1 an array of objects
     * @param array2 an array of objects
     * @return true if the first array is a subset of the second one.
     */
    public static boolean isSubset(Object[] array1, Object[] array2) {
        if(array1.length > array2.length) return false;
        for(Object object1 : array1) {
            boolean found = false;
            for(Object object2 : array2) {
                if(object1 == object2) {found = true; break;}}
            if(!found) return false;}
        return true;}

    /** creates a deep clone of the array list
     *
     * @param array a list of IntArrayLists
     * @return a deep clone of the list
     */
    public static ArrayList<IntArrayList> deepClone(ArrayList<IntArrayList> array) {
        ArrayList list = new ArrayList(array.size());
        for(IntArrayList item : array) {list.add(item == null ? null : item.clone());}
        return list;}

    /** searches the key in a sorted IntArrayList
     *
     * @param list a sorted IntArrayList
     * @param key  any int
     * @return the index of the key in the array, or -1 if the key is not in the array.
     */
    public static int binarySearch(IntArrayList list, int key) {
        if(list.isEmpty()) {return -1;}
        int start = 0; int end = list.size()-1;
        int first = list.getInt(0);
        int last = list.getInt(end);
        if(key < first || key > last) {return -1;}
        if(key == first) {return 0;}
        if(key == last) {return end;}
        while(start != end) {
            int middle = (start + end) / 2;
            int item = list.getInt(middle);
            if(key == item) {return middle;}
            if(middle == start) {return -1;}
            if(key < item) {end = middle;}
            else {start = middle;}}
        return -1;}


    /** inserts the item sorted into the list
     *
     * @param list a sorted list of integers
     * @param item an old or new integer
     * @return the index of the new (or old) integer in the list
     */
    public static int insertSorted(IntArrayList list, int item) {
        if(list.isEmpty()) {list.add(item); return 0;}
        int start = 0;
        int first = list.getInt(0);
        if(item == first) {return 0;}
        if(item < first) {list.add(0,item); return 0;}
        int end = list.size()-1;
        int last = list.getInt(end);
        if(item == last) {return end;}
        if(item > last) {list.add(end+1,item); return end+1;}
        while(start != end) {
            int middle = (start + end) / 2;
            int i = list.getInt(middle);
            if(i == item) {return middle;}
            if(middle == start) {list.add(middle+1,item); return middle+1;}
            if(item < i) {end = middle;}
            else {start = middle;}}
        return -1;
    }

    /** contatenates the string n times
     *
     * @param s a string
     * @param n an integer
     * @return the concatenated string.
     */
    public static String concatenateString(String s, int n) {
        StringBuilder st = new StringBuilder();
        for(int i = 0; i < n; ++i) st.append(s);
        return st.toString();}

    /** puts the string between blanks such that the string is centered.
     *
     * @param s a string
     * @param n the total length of the desired string
     * @return the string surrounded with blanks.
     */
    public static String centerString(String s, int n) {
        int length = s.length();
        if(n <= length) return s;
        String blanks = concatenateString (" ",((n-length) / 2));
        return blanks + s + blanks;}

    /** turns an array to a string
     *
     * @param array    the array
     * @param function for mapping array elements to a string
     * @return a string [item_1,...,item_n]
     */
    public static String arraysToString(Object[] array, Function<Object,Object> function) {
        StringBuilder st = new StringBuilder();
        st.append("[");
        for(int i = 0; i < array.length-1; ++i) {
            st.append(function.apply(array[i]).toString()).append(",");}
        st.append(function.apply(array[array.length-1]).toString()).append("]");
        return st.toString(); }

    /** checks to arrays if they are equal as multisets, testing the elements with ==
     *
     * @param as an array of objects
     * @param bs an array of objects
     * @return true if they are equal as multisets
     */
    public static boolean multisetEquals(Object[] as, Object [] bs) {
        if(as.length != bs.length) {return false;}
        for(Object a : as) {
            boolean contains = false;
            for(Object b : bs) {if(a == b) contains = true;}
            if(!contains) {return false;}}
        return true;}

    /** computes the greatest common divisor of the list of numbers.
     *
     * @param numbers a list of integers.
     * @return the greatest common divisor of the list.
     */
    public static int gcd(IntArrayList numbers) {
        assert(!numbers.isEmpty());
        int gcd = numbers.get(0);
        if(numbers.size() == 1) return gcd;
        for(int i = 1; i < numbers.size(); ++i) {
            int a = numbers.get(i);
            while(gcd != a) {
                if(gcd > a) gcd -= a;
                else a -= gcd;}}
        return gcd;}

    /** computes the greatest common divisor of two numbers
     *
     * @param n an integer > 0
     * @param m an integer > 0
     * @return the greatest common divisor of the two numbers.
     */
    public static int gcd(int n, int m) {
        assert(n > 0 && m > 0);
        while(n != m) {
            if(n > m) n -= m;
            else m -= n;}
        return n;}



    public static void  main1(String[] args) {
        IntArrayList n = IntArrayList.wrap(new int[]{456,678,888});
        System.out.println(gcd(n));
    }



    /** computes n!
     *
     * @param n an integer >= 0
     * @return n!
     */
    public static int factorial(int n) {
        assert n >= 0;
        int fact = 1;
        for(int i = 2; i <= n; ++i) fact *= i;
        return fact;}

    /** computes m over n = (m! / (n! * (m-n)!)
     *
     * @param m nominator
     * @param n denominator
     * @return (m! / (n! * (m-n)!)
     */
    public static int over(int m, int n) {
        assert m >= n && m > 0 && n > 0;
        int nominator = 1;
        for(int i =  Math.max(n,m-n) + 1; i <= m; ++i) nominator *= i;
        int k = Math.min(n,m-n);
        int denominator = 1;
        for(int i = 2; i <= k; ++i) denominator *= i;
        return nominator / denominator;
    }

    /** computes all combinations of n ones within m bits.
     * Example: combinations(3,2): 011,101,110 <br>
     * Notice that the number of such combinations is m over n. <br>
     * Example: 10 over 3  = 120
     *
     * @param m the number of bits
     * @param n the number of ones
     * @return the combinations as integers
     */
    public static IntArrayList combinations(int m, int n) {
        assert m > 0 && n > 0 && m >= n;
        IntArrayList com = new IntArrayList();
        int end = m-n;
        for(int i = 0; i <= end; ++i) {com.add((1 << i));}
        for(int i = 1; i < n; ++i) {
            ++end;
            IntArrayList com1 = new IntArrayList();
            for(int k : com) {
                int last = m;
                for(; last > 0; --last) {if(((1 << last) & k) != 0) break;}
                for(int position = last+1; position <= end; ++position) {
                    com1.add(k | (1<<position));}}
            com = com1;}
        return com;}

    /** computes all combinations of n numbers out of the list
     *
     * @param n                    the size of the combination
     * @param list                 a list of integers
     * @param avoidDoubles         if true then double items are avoided
     * @param avoidComplementaries if true then lists with complementary elements are avoided
     * @param checkSubsumption     if true then subsumed lists are avoided.
     * @return      an ArrayList of all combinations of n numbers out of the list.
     */
    public static ArrayList<IntArrayList> combinations(int n, IntArrayList list,
                                                       boolean avoidDoubles,
                                                       boolean avoidComplementaries,
                                                       boolean checkSubsumption) {
        assert n > 0 && n <= list.size();
        ArrayList<IntArrayList> combinations = new ArrayList<>();
        int size = list.size();
        for(int c : combinations(size,n)) {
            IntArrayList cmb = new IntArrayList();
            boolean addItem = true;
            for(int i = 0; i < size; ++i) {
                if(((1 << i) & c) != 0) {
                    int item = list.getInt(i);
                    if(avoidDoubles && cmb.contains(item)) continue;
                    if(avoidComplementaries && cmb.contains(-item)) {addItem = false; break;}
                    cmb.add(item);}}
            if(!addItem) continue;
            if(!cmb.isEmpty())
                if(checkSubsumption) addIfNotSubsumed(combinations,cmb);
                else combinations.add(cmb);}
        return combinations;
    }

    /** adds items to the list of items there is nos subset of it in the list
     * All list elements of which items is a subset of are removed
     *
     * @param lists a list of integer lists
     * @param items an integer list
     */
    private static void addIfNotSubsumed(ArrayList<IntArrayList> lists, IntArrayList items) {
        for(int i = 0; i < lists.size(); ++i) {
            IntArrayList list = lists.get(i);
            if(isSubset(list,items)) return;
            if(isSubset(items,list)) lists.remove(i--);}
        lists.add(items);}

    /** computes the cross product of the list of int-lists.
     * Double occurrences of items are avoided.<br>
     * Lists with complementary items are avoided.<br>
     * Superlists of other lists are avoided.
     *
     * @param lists a list of int-lists
     * @return the optimized cross product of the list
     */
    public static ArrayList<IntArrayList> crossProduct(ArrayList<IntArrayList> lists) {
        ArrayList<IntArrayList> product = new ArrayList<>();
        for(int item : lists.get(0)) product.add(IntArrayList.wrap(new int[]{item}));
        int size = lists.size();
        for(int i = 1; i < size; ++i) {
            ArrayList<IntArrayList> product1 = new ArrayList<>();
            IntArrayList nextList = lists.get(i);
            for(int item : nextList) {
                for(IntArrayList lastList : product) {
                    lastList = lastList.clone();
                    if(lastList.contains(-item)) continue;
                    if(!lastList.contains(item)) lastList.add(item);
                    addIfNotSubsumed(product1,lastList);}}
            product = product1;}
        return product;}


    /** multiplies the elements of the list
     *
     * @param list a list of integers
     * @return the product of the list elements
     */
    public static int product(IntArrayList list) {
        int product = 1;
        for(int q : list) product *= q;
        return product;}

    /** turns an IntArrayList to a comma separated string
     *
     * @param list the list
     * @param converter for converting integers to strings
     * @return the list as comma separated string.
     */
    public static String intArrayListToString(IntArrayList list,Function<Integer,String> converter) {
        StringBuilder st = new StringBuilder();
        int size = list.size();
        for(int i = 0; i < size; ++i) {
            int item = list.getInt(i);
            st.append(converter == null ? Integer.toString(item) : converter.apply(item));
            if(i < size-1) st.append(",");}
        return st.toString();}

    /** turns the IntArrayList into a string where consecutive integers are comprised into an interval notation.
     * <br>
     * Example: 3,6,7,8,10,11,12,13,15,16,19  -&gt; [3,6-8,10-13,15,16,19]
     *
     * @param list an IntArrayList
     * @return the list as condensed string.
     */

    public static String intArrayListToString(IntArrayList list) {
        if(list.isEmpty()) return "";
        int length = list.size();
        if(length < 3) return list.toString();
        StringBuilder st = new StringBuilder();
        int blocks = 1;
        int blocksize = 100;
        st.append("[");
        int index1 = 0;
        int index2 = 0;
        while(true) {
            for(;index2 < length-1; ++index2) {
                if(list.getInt(index2+1) != list.getInt(index2)+1) break;}
            if(index1 > 0) st.append(",");
            if(st.length() >= blocks*blocksize) {st.append("\n"); ++blocks;}
            if(index1 == index2) st.append(list.get(index1));
            else {if(index1+1 == index2) st.append(list.get(index1)).append(",").append(list.getInt(index2));
                  else {st.append(list.get(index1)).append("-").append(list.getInt(index2));}}
            if(index2 == length-1) break;
            index1 = index2+1;
            index2 = index1;}
        st.append("]");
        return st.toString() ;}

    /**
     * Converts an IntArrayList to an ArrayList<Integer>.
     *
     * @param list the IntArrayList to convert
     * @return an ArrayList<Integer> representing the same elements as the input IntArrayList
     */
    public static ArrayList toArrayList(IntArrayList list) {
        ArrayList intList = new ArrayList<>();
        for(int i : list) intList.add(i);
        return intList;}

    /**
     * Shuffles the elements of the given array using the Fisher-Yates algorithm.
     *
     * @param array the array to be shuffled
     * @param start the first array-index to be shuffled
     * @param seed the seed for the random number generator.
     */
    public static void shuffleArray(int[] array, int start, int end, int seed) {
        int index, temp;
        Random random = new Random(seed);
        for (int i = end; i >= start; i--)
        {
            index = random.nextInt(i + 1-start) + start;
            temp = array[index];
            array[index] = array[i];
            array[i] = temp;}}

    /** The bits in int integer i represent a model: position 0: first literal in predicates is true etc.
     *  The method checks if the literal is true in the given model.
     *
     * @param i          the bits represent a model of the predicates in the predicates array.
     * @param literal    a literal to be tested.
     * @param predicates   a list of predicates.
     * @return           true if the literal is true in the model.
     */
    public static boolean isTrue(int i, int literal, IntArrayList predicates) {
        int index = predicates.indexOf(Math.abs(literal));
        boolean truth = (i & (1 << index)) != 0;
        return literal > 0 ? truth : !truth;}

    /**
     * Generates a string representation of the given model.
     * <br>
     * The bits in the model must correspond to the given predicates.
     *
     * @param model The integer representation of the model.
     * @param predicates the corresponding predicates.
     * @param symboltable The given symbol table.
     * @return The string representation of the model.
     */
    public static String modelString(int model, IntArrayList predicates, Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        for(int i = 0; i < predicates.size(); ++i) {
            int sign = ((model & 1 << i) != 0) ? 1: -1;
            st.append(Symboltable.toString(sign*Math.abs(predicates.get(i)) ,symboltable));
            if(i < predicates.size()-1) st.append(",");}
        return st.toString();}

    /**
     * Pauses the execution of the current thread for the specified amount of time.
     *
     * @param time the amount of time, in milliseconds, to wait
     */
    public static void wait(int time) {
        try{Thread.sleep(time);}
        catch(Exception ignore) {}}

    public static void  main(String[] args) {
        IntArrayList a = new IntArrayList();
        a.add(3); a.add(6); a.add(7); a.add(8); a.add(10);a.add(11);a.add(12);a.add(13);a.add(15);a.add(16);a.add(19);
        System.out.println(intArrayListToString(a));
    }

    /** returns the time as sec, ms, μs or ns
     *
     * @param time nanoseconds
     * @return the time as string.
     */
    public static String duration(long time) {
        if(time > 1000000000) return (float)time/1000000000.0 + " sec";
        if(time > 1000000)    return (float)time/1000000.0 + " ms";
        if(time > 1000)    return (float)time/1000.0 + " μs";
        return time + " ns";}





}

