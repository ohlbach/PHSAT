package Utilities;

import java.io.InputStream;
import java.util.ArrayList;

/**
 * Created by Ohlbach on 03.09.2018.
 */
public class Utilities {

    /** parses a string-representation of an integer
     *
     * @param place for error reporting
     * @param value the string to be parsed
     * @param errors for appending error messages
     * @return the parse Integer, or null
     */
    public static Integer parseInteger(String place, String value, StringBuffer errors) {
        try{return Integer.parseInt(value);}
        catch(NumberFormatException ex) {errors.append(place+": " + value + " is no integer.\n");}
        return null;}

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
    public static ArrayList<Integer> parseRange(String place, String value, StringBuffer errors) {
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

    /** computes the cross product of the given list.<br/>
     * Example: [1,2] x [3,4] x {5,6] yields <br/>
     * [[1, 3, 5], [1, 4, 5], [2, 3, 5], [2, 4, 5], [1, 3, 6], [1, 4, 6], [2, 3, 6], [2, 4, 6]]
     *
     *
     * @param lists the list for which the cross product ist to be computed
     * @return the cross product
     */
    public static ArrayList<ArrayList<Integer>> crossProduct(ArrayList<Integer>... lists) {
        switch(lists.length) {
            case 0:
                return null;
            case 1:
                ArrayList<ArrayList<Integer>> list = new ArrayList<>();
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
    private static ArrayList<ArrayList<Integer>> crossProductTwo(ArrayList<Integer> list1, ArrayList<Integer> list2 ) {
        ArrayList<ArrayList<Integer>> list = new ArrayList<>();
        for(Integer n1 : list1) {
            for(Integer n2 : list2) {
                ArrayList<Integer> product = new ArrayList<>();
                list.add(product);
                product.add(n1);product.add(n2);}}
        return list;}

    /** adds a new list to an already computed cross product
     *
     * @param list1
     * @param list2
     * @return the new cross product
     */
    private static ArrayList<ArrayList<Integer>> addCrossProduct(ArrayList<ArrayList<Integer>> list1, ArrayList<Integer> list2) {
        ArrayList<ArrayList<Integer>> list = new ArrayList<>();
        for(Integer n : list2) {
            ArrayList<ArrayList<Integer>> newlist = addProductElement(list1,n);
            list.addAll(newlist);}
        return list;}

    /** adds a single element to an already computed cross product.
     *
     * @param list1
     * @param n
     * @return the new cross product.
     */
    private static ArrayList<ArrayList<Integer>> addProductElement(ArrayList<ArrayList<Integer>> list1, Integer n) {
        ArrayList<ArrayList<Integer>> list = new ArrayList<>();
        for(ArrayList<Integer> elements : list1) {
            ArrayList<Integer> clones = (ArrayList<Integer>)elements.clone();
            clones.add(n);
            list.add(clones);}
        return list;}


}
