package Management;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.function.BiFunction;

/**
 * The Parameters class  together with the Parameter class is the interface between the QuSat solver system and the GUI.
 *
 * The GlobalParameters, the Generator classes and the Solver classes can specify their parameters
 * by generating Parameter and Parameters objects and filling the attributes with default values.<br>
 * A Parameters object represents a group of Parameter objects. <br>
 * The GUI uses the parameters to control the user interaction and specifying problem specific parameters.
 */
public class Parameters {
    /** the name of the Paramters group */
    public String name;

    /** a description of the group, displayed in the GUI. */
    public String description;

    /** the actual list of parameters */
    public ArrayList<Parameter> parameters;

    /** allows to crosscheck the validity of the parameters in the group of parameters.<br>
     *
     * It is called with the Parameters object and a StringBuilder errors for appending error messages. <br>
     * The result is true if the check is positive.*/
    public BiFunction<Parameters, StringBuilder, Boolean> finalCheck;

    /** a function to be applied to a Parameters object and a StringBuilder - error.*/
    public BiFunction<Parameters, StringBuilder, Object> operation;

    /**Constructs a Parameters object with an empty list of parameters.
     * @param name the name of the Parameters object
     */
    public Parameters(String name) {
        this.name = name;
        parameters = new ArrayList<>();}

    /**Adds a Parameter to the list of parameters.
     *
     * @param parameter the Parameter object to be added
     */
    public void add(Parameter parameter) {
        parameters.add(parameter);}

    /**Sets the description of the Parameters object.
     *
     * @param description the description to set
     */
    public void setDescription(String description) {this.description = description;}

    /** set the operation
     *
     * @param operation a function to be applied to a Parameters object and a StringBuilder - error.
     */
    public void setOperation(BiFunction<Parameters,StringBuilder,Object> operation) {
        this.operation = operation;}

    /**
     * Sets the final check function for the Parameters object.
     *
     * @param finalCheck a BiFunction that takes a Parameters object and a StringBuilder object as parameters.
     *                   It returns a Boolean value indicating whether the final check passed or not.
     *                   The function should perform the necessary checks and modifications on the Parameters object
     *                   and append any error messages to the StringBuilder object.
     */
    public void setFinalCheck(BiFunction<Parameters, StringBuilder, Boolean> finalCheck) {this.finalCheck = finalCheck;}


    /**Creates a deep copy of the current Parameters object.
     *
     * @return A new Parameters object that is an exact copy of the current Parameters object.
     */
    public Parameters clone() {
        Parameters cloned = new Parameters(name);
        cloned.description = description;
        cloned.finalCheck = finalCheck;
        cloned.parameters = new ArrayList<>();
        for (Parameter parameter : parameters) cloned.parameters.add(parameter.clone());
        return cloned;}

    /**
     * Returns a string representation of the Parameters object.
     * The string includes the name and the string representation of each parameter.
     * Each parameter is represented as a separate line.
     * The string representation of the parameter includes the name, value string, and the result of the valueString() method.
     * It is used to save a parameter into a file. Only the essential attributes need to be saved.
     *
     * @return A string representation of the Parameters object.
     */
    public String toString() {
        StringBuilder result = new StringBuilder(name + "\n");
        for (Parameter parameter : parameters) {
            result.append(parameter.toString()).append("\n");}
        return result.toString();}

    /**
     * Loads parameters from the given BufferedReader and populates the values in the parameters list.
     * The values are parsed using the parser function if available.
     * If parsing fails, the error message is appended to the errors StringBuilder.
     *
     * @param reader The BufferedReader to read the parameter values from.
     * @param errors The StringBuilder to append any error messages to.
     * @throws IOException If an I/O error occurs.
     */
    public void loadParameters(BufferedReader reader, StringBuilder errors) throws IOException {
        String line = "";
        while((line = reader.readLine()).isEmpty()) {}
        while(!line.isEmpty()) {
            String[] parts = line.split("\\s*;\\s*");
            String name = parts[0];
            for(Parameter parameter : parameters) {
                if(parameter.name.equals(name)) {
                    parameter.valueString = parts[1];
                    parameter.value = (parameter.parser == null) ?
                            parts[1] : parameter.parser.apply(parts[1],errors);
                    if(parameter.updater != null) parameter.updater.accept(parts[1]);}}
            line = reader.readLine();}}

}
