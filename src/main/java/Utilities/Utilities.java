package Utilities;

import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import sun.awt.image.ImageWatched;

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
                    return range;}}}
            for(String part : value.split("(\\s+|\\s*,\\s*)")) {
                Integer n = parseInteger(place,part,errors);
                if(n != null) {range.add(n);}
                else {
                    errors.append(place + " The format should be: integer or comma separated integer or 'intger to integer'." );
                    return null;
                    }}
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

    public static Clause makeClause(int id, String literals) {
        String[] lits = literals.split("\\s*(,| )\\s*");
        Clause clause = new Clause(id,lits.length);
        for(String lit : lits) {
            clause.add(new CLiteral(Integer.parseInt(lit)));}
        clause.setStructure();
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

    public static void  main(String[] args) {
        forSome(Integer.MIN_VALUE+1,(i-> {System.out.println(i); return false;}));
    }

    public static boolean isInteger(String s) {
        try{Integer.parseInt(s);}
        catch(NumberFormatException ex) {return false;}
        return true;}

    /** computes the union of the first IntArray with the second IntArray
     *
     * @param list1 an IntArray
     * @param list2 an IntArray
     */
    public static void joinIntArray(IntArrayList list1, IntArrayList list2) {
        for(int item : list2) {if(!list1.contains(item)) list1.add(item);}}


    public static void  mainA(String[] args) {
        int a = toInt(new int[]{0,4});
        int b = toInt(new int[]{1,3});
        int n = 15;
        long t = System.currentTimeMillis();
        ArrayList<Integer> list = largestSubsetsInt(n,(l-> ((a & l) == a) && ((b & l) != b)));
        System.out.println((System.currentTimeMillis()-t)+" ms");

        t = System.nanoTime();
        ArrayList<Integer> list2 = largestSubsetsInt(n,(l-> ((a & l) == a) && ((b & l) != b)));
        System.out.println((System.nanoTime()-t)+" ns");

        t = System.nanoTime();
        ArrayList<Long> list1 = largestSubsetsLong(n,(l-> ((a & l) == a) && ((b & l) != b)));
        System.out.println((System.nanoTime()-t)+" ns");

        pll("L",list1);
    }


}

