package Management;

import java.util.ArrayList;
import java.util.function.BiFunction;
import java.util.function.Consumer;

/**
 * The Parameter class  together with the Parameters class is the interface between the QuSat solver system and the GUI.
 *
 * The GlobalParameters, the Generator classes and the Solver classes can specify their parameters
 * by generating Parameter and Parameters objects and filling the attributes with default values.<br>
 * The GUI uses the parameters to control the user interaction and specifying problem specific parameters.
 */
public class Parameter {
    /** The Type enum represents the different types of parameters.
     *
     *  - String: the parameter is just a String.<br>
     *  - OneOf: there are different choices which are specified by the parameters attribute.<br>
     *  - Label: one of the choices in oneOf parameters.<br>
     *  - File:  open a FileChooser to choose a file.<br>
     *  - Directory: open a FileChooser to choose a directory.<br>
     *  - Frame:  one of the choices in oneOf parameters: print output into a frame<br>
     *  - Boolean: a boolean choice.
     */
    public enum DisplayType {String,OneOf,Label,Button,File,Directory,Frame,Boolean}
    /** name of the parameter (displayed in the GUI)*/
    public String name;
    /** type of the parameter */
    public DisplayType displayType;
    /** the type of the parameter values */
    public ValueType valueType;
    /** the value as a String */
    public String valueString;
    /** the parsed valueString. It may be any object */
    public Object value;
    /** a description to be displayed in the GUI */
    public String description;
    /** specifies the allowed selections in oneOf parameters */
    public Parameters parameters;
    /** for turning the valueString into an Object */
    public BiFunction<String,StringBuilder,Object> parser;
    /** this is called when parameters are loaded and the GUI-elements are to be updated by the new values.
     * The updater is set in the corresponding GUI-elements and called by loadParameters in the Parameters class.*/
    public Consumer<String> updater;

    /**
     * Parameter represents a parameter with its name, type, value, valueString, and description.
     *
     * @param name         the name of the parameter
     * @param displayType  the type of the parameter (String, OneOf, Label, File, Directory, Frame, Boolean)
     * @param valueString  the string representation of the parameter value
     * @param errors       for adding error messages
     * @param description  the description of the parameter
     */
    public Parameter(String name, DisplayType displayType, ValueType valueType, String valueString,
                     StringBuilder errors,
                     String description) {
        this.name = name;
        this.displayType = displayType;
        this.valueType = valueType;
        this.valueString = valueString == null ? name : valueString;
        this.value = valueString == null ? null: valueType.parseValue(valueString,errors);
        this.description = description;
    }

    /** Sets the parser for this instance of the Parameter class.
     *
     * @param parser the parser to be set. It should be a BiFunction that takes a String and a StringBuilder as input parameters, and returns an Object.
     *               The parser is responsible for parsing the String representation of a value and converting it to the corresponding Object representation.
     *               The StringBuilder parameter can be used to append any error messages during parsing.
     */
    public void setParser(BiFunction<String,StringBuilder,Object> parser) {
        this.parser = parser;}


    /**
     * Creates a deep copy of the current Parameter object.
     *
     * This is used when one QuSat job is start in a new Thread and another job is to be specified in the GUI.
     *
     * @return A new Parameter object that is an exact copy of the current Parameter object.
     */
    public Parameter clone() {
        Parameter cloned = new Parameter(name, displayType, valueType, valueString, null, description);
        cloned.updater = updater;
        cloned.parser = parser;
        if(parameters != null) {cloned.parameters = parameters.clone();}
        return cloned;}

    /**
     * Returns a string representation of the parameter.<br>
     * The string includes the name, value string, and the result of the valueString() method.<br>
     * It is used to save a parameter into a file. Only the essential attributes need to be saved.
     *
     * @return a string representation of the parameter
     */
    public String toString() {
        return name + "; " + valueString + "; " +valueString(value);}

    /**
     * Converts the given value to a string representation.<br>
     * If the value is null, the method returns "null".<br>
     * If the value is an instance of ArrayList, it converts each element of the ArrayList to a string using recursion,
     * and returns a comma-separated string of all the converted elements.<br>
     * If the value is an array, it converts each element of the array to a string using recursion,
     * and returns a comma-separated string of all the converted elements enclosed in brackets.<br>
     * If none of the above conditions are met, it simply returns the value converted to a string.
     *
     * @param value the value to convert to a string representation.
     * @return a string representation of the given value.
     */
    public static String valueString(Object value) {
        if (value == null) {return "null";}
        if(value instanceof ArrayList) {
            StringBuilder sb = new StringBuilder();
            for (Object v : (ArrayList) value) {
                sb.append(valueString(v)).append(", ");}
            sb.delete(sb.length() - 2, sb.length());
            return sb.toString();}

        if(value instanceof int[]) {
            int[] arr = (int[]) value;
            if(arr.length == 2) {return(arr[0] == arr[1]) ? ""+ arr[0] : arr[0] + "-"+ arr[1]; }
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (Object v : (int[]) value) {
                sb.append(valueString(v)).append(", ");}
            sb.delete(sb.length() - 2, sb.length());
            sb.append("]");
            return sb.toString();}

        if(value.getClass().isArray()) {
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (Object v : (Object[]) value) {
                sb.append(valueString(v)).append(", ");}
            sb.delete(sb.length() - 2, sb.length());
            sb.append("]");
            return sb.toString();}
        return value.toString();
        }

        public static void main(String[] args) {
          int[] a = new int[]{1,2,3,4,5};
          ArrayList<int[]> b = new ArrayList<>();
          b.add(a);b.add(a);
            System.out.println(valueString(b));
        }




}
