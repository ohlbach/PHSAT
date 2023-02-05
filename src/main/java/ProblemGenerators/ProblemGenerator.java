package ProblemGenerators;

import Datastructures.Clauses.Connective;
import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import Management.Monitor.Monitor;
import Utilities.KVParser;
import Utilities.Utilities;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;

import static Utilities.Utilities.parseInteger;

/** This is the interface to the generator classes.
 * The generator classes generate SAT-Problems from different sources. <br>
 * Each generator class should provide the following static methods: <br>
 *  - public static help()  for producing a help text<br>
 *  - public static void makeProblemGenerator(HashMap&lt;String,String&gt; parameters,
 *                                             ArrayList&lt;ProblemGenerator&gt; generators,
 *                                             StringBuilder errors, StringBuilder warnings) <br>
 *  - public InputClauses generateProblem(Monitor errorMonitor); <br>
 * <br>
 * The makeProblemGenerator method turns parameters as strings into a ProblemGenerator object which then can generate the problem. <br>
  */
public abstract class ProblemGenerator {

    /** the constructor is never used */
    public ProblemGenerator() {}

    /** the list of problem generator names */
    public static String[] problemGeneratorNames = new String[]{"random","file","pigeonhole","string"};

    /** takes the parsed input clauses */
    public InputClauses inputClauses = null;

    /** checks if the name is a generator name
     *
     * @param name  a string
     * @return true if the name is the name of a generator.
     */
    public static boolean isProblemGenerator(String name) {
        for(String generatorName : problemGeneratorNames) {if(name.equals(generatorName)) {return true;}}
        return false;}

    /** maps the generator names to the generator classes
     *
     * @param name a generator name
     * @return the generator class, or null
     */
    public static Class generatorClass(String name) {
        switch (name) {
            case "random":       return ProblemGenerators.RandomClauseSetGenerator.class;
            case "file":         return ProblemGenerators.CNFReader.class;
            case "pigeonhole":   return ProblemGenerators.PigeonHoleGenerator.class;
            case "string" :      return ProblemGenerators.StringClauseSetGenerator.class;
            default: return null;}}

    /** collects all the help-strings for all generator classes
     *
     * @return the collected help string for all generator classes
     */
    public static String help() {
        StringBuilder st = new StringBuilder();
        st.append("The following problem generator types are available:\n\n");
        for(String generatorName : problemGeneratorNames) {
            st.append("Problem Generator ").append(generatorName).append(":\n");
            st.append(help(generatorName)).append("\n\n");}
        return st.toString();}

    /** returns the help-string for the generator with the given name
     *
     * @param name a generator name
     * @return its help-string
     */
    public static String help(String name) {
        Class clazz = generatorClass(name);
        if(clazz == null) {return "ProblemGenerator.help: Unknown Generator Class: " +name;}
        try{
            Method helper = clazz.getMethod("help");
            return (String)helper.invoke(null);}
        catch(Exception ex) {ex.printStackTrace();System.exit(1);}
        return null;}

    /** parses the string-type parameters into sequences of objects
     *
     * @param parameterList the parameters
     * @param errors        for collecting error messages
     * @param warnings      for collecting warning messages
     * @return              a list of generators
     */
    public static ArrayList<ProblemGenerator> makeProblemGenerator(ArrayList<HashMap<String,String>> parameterList,
                                                                   StringBuilder errors, StringBuilder warnings) {
        ArrayList<ProblemGenerator> generators = new ArrayList<>();
        for(HashMap<String,String> parameters: parameterList) {
            String type = parameters.get("generator");
            if(type == null) continue;
            Class clazz = generatorClass(type);
            if(clazz == null) {
                errors.append("Problem Generator: Unknown generator class: ").append(type); continue;}
            try{
                Method makeProblemGenerator = clazz.getMethod("makeProblemGenerator",HashMap.class,
                        ArrayList.class, StringBuilder.class, StringBuilder.class);
                makeProblemGenerator.invoke(null,parameters,generators,errors,warnings);}
            catch(Exception ex) {ex.printStackTrace();System.exit(1);}}
        return generators;}


    /** extracts the type-specific parameters from the kvParser
     *
     * @param kvParser  contains all control parameters
     * @param type      one of the generator types
     * @return the parameters for the given type
     */
    public static HashMap<String,String> getParameters(KVParser kvParser, String type) {
        for(HashMap<String,String> parameters: kvParser.get("generator")) {
            if(type.equals(parameters.get("type"))) return parameters;}
        return null;}

    /** generates the InputClauses
     *
     * @param errorMonitor    for error massages.
     * @return null or the new InputClauses.
     */
    public abstract InputClauses generateProblem(Monitor errorMonitor);

    /** parses the clause string and generates a InputClauses object.
     *
     * @param problemName the name of the example problem.
     * @param lineIterator for iterating over the lines.
     * @param errors   for error messages.
     * @param warnings for warning messages.
     * @return  null or an InputClauses object.
     */
    protected static InputClauses parseClauses(String problemName, Iterator<String> lineIterator,
                                               StringBuilder errors, StringBuilder warnings) {
        InputClauses inputClauses = new InputClauses();
        inputClauses.problemName = problemName;
        Integer predicates = 0;
        int lineNumber = 0;
        Symboltable symboltable = null;
        StringBuilder info = new StringBuilder();
        int id = 0;
        boolean inHeader = true;
        boolean firstClauseLineChecked = false;
        while(lineIterator.hasNext()) {
            String line = lineIterator.next();
            ++lineNumber;
            String errorPrefix = problemName + ": line " + lineNumber + ": '" + line + "':\n    ";
            line = line.trim();
            if(line.isEmpty()) {continue;}
            if(line.startsWith("%")){continue;}
            if(line.startsWith("#")) {info.append(line.substring(1)).append("\n"); continue;}
            if(line.startsWith("p") && inHeader) { // p cnf predicates ...
                inHeader = false;
                String[] parts = line.split("\\s*[ ,]\\s*");
                if(parts.length < 3) {
                    errors.append(errorPrefix).append("Illegal format of line. It should be 'p cnf predicates ...'\n");
                    return null;}
                if(!parts[1].equals("cnf")) {
                    errors.append(errorPrefix).append("This indicates no cnf file\n");
                    return null;}

                predicates = Utilities.parseInteger(errorPrefix, parts[2],errors);
                if(predicates == null) {return null;}
                if(predicates <= 0) {
                    errors.append(errorPrefix).append("Negative number of predicates: '").append(parts[2]).append("'\n");
                    return null;}
                inputClauses.predicates = predicates;
                inputClauses.symboltable = new Symboltable(predicates);
                continue;}
            if(inHeader) {info.append(line).append("\n"); continue;}

            if(predicates == null) {
                errors.append(errorPrefix).append(" p-line missing: 'p cnf predicates ...'\n");
                return null;}

            if(!firstClauseLineChecked) {
                String firstLiteral = getFirstLiteral(line);
                if(firstLiteral == null) {errors.append(errorPrefix).append("malformed first literal.\n"); return null;}
                if(parseInteger(firstLiteral) == null) {
                    symboltable = new Symboltable(predicates);
                    inputClauses.symboltable = symboltable;}
                firstClauseLineChecked = true;}

            int[] clause = parseLine(line.trim(),++id, symboltable,errorPrefix,errors);
            if(clause == null) continue;
            clause = InputClauses.checkSyntax(clause,predicates,errorPrefix,errors,warnings);
            if(clause == null) continue;
            inputClauses.addClause(clause);}
        if(info.length() > 0) inputClauses.info = info.toString();
        inputClauses.nextId = id+1;
        return inputClauses;}

    /** extracts the first literal in the line.
     *
     * @param line a clause line
     * @return the first literal, or null if it is a malformed interval clause.
     */
    protected static String getFirstLiteral(String line) {
        char firstChar = line.charAt(0);
        switch(firstChar) {
            case '&':
            case 'e': return line.split("\\s*[, ]\\s*",3)[1];
            case '=':
            case '<':
            case '>': return line.split("\\s*[, ]\\s*",4)[2];
            case '[':
                int firstIndex = line.indexOf(']');
                return firstIndex < 0 ? null :
                        line.substring(firstIndex+1).trim().split("\\s*[, ]\\s*",2)[0];}
        return line.split("\\s*[, ]\\s*")[0];}


    /** parses a single clause-line
     *
     * @param line        the line to be parsed.
     * @param id          the identifier for the new clause.
     * @param symboltable null or a symboltable
     * @param errorPrefix a prefix for the error messages
     * @param errors      for appending error messages.
     * @return            the new inputClause or null if errors have been detected.
     */
    protected static int[] parseLine(String line, int id, Symboltable symboltable, String errorPrefix, StringBuilder errors) {
        switch(line.charAt(0)) {
            case '&': return parseAndEquiv(line.trim(), id, Connective.AND, symboltable, errorPrefix,errors);
            case 'e': return parseAndEquiv(line.trim(), id, Connective.EQUIV, symboltable, errorPrefix,errors);
            case '<': return parseWithQuantification(line.trim(), id, Connective.ATMOST, symboltable, errorPrefix,errors);
            case '>': return parseWithQuantification(line.trim(), id, Connective.ATLEAST, symboltable, errorPrefix,errors);
            case '=': return parseWithQuantification(line.trim(), id, Connective.EXACTLY, symboltable, errorPrefix, errors);
            case '[': return parseInterval(line, id, symboltable, errorPrefix,errors);
            default:  return parseOr(line.trim(), id, symboltable, errorPrefix,errors);}}

    /** parses an or-clause, generates an InputClause and fills up the symboltable, if necessary.
     * If the symboltable is null, then the clause must consist of non-null integers.<br>
     * If the symboltable is not null, then the literals can be arbitrary strings. <br>
     * Negative literals are indicated by a preceding -.
     *
     * @param line        the line to be parsed.
     * @param id          the identifier for the clause.
     * @param symboltable null or a symboltable.
     * @param errorPrefix a prefix for the error messages
     * @param errors      for appending error messages
     * @return            the new inputClause or null if errors have been detected.
     */
    protected static int[] parseOr(String line, int id, Symboltable symboltable, String errorPrefix, StringBuilder errors) {
        String[] clause = line.split("\\s*[, ]\\s*");
        int length = clause.length;
        length = clause[length-1].equals("0")? length+1: length+2;
        int[] inputClause = new int[length];
        inputClause[0] = id;
        inputClause[1] = Connective.OR.ordinal();
        boolean okay = parseLiterals(clause,0,inputClause,2,symboltable,errorPrefix,errors);
        return okay ? inputClause : null;}



    /** parses an and- and an equiv-clause, generates an InputClause and fills up the symboltable, if necessary.
     * If the symboltable is null, then the clause must consist of non-null integers.<br>
     * If the symboltable is not null, then the literals can be arbitrary strings. <br>
     * Negative literals are indicated by a preceding -.
     *
     * @param line        the line to be parsed.
     * @param id          the identifier for the new clause.
     * @param connective  the connective for the new clause.
     * @param symboltable null or a symboltable.
     * @param errorPrefix a prefix for the error messages
     * @param errors      for appending error messages.
     * @return            the new inputClause or null if errors have been detected.
     */
    protected static int[] parseAndEquiv(String line, int id, Connective connective, Symboltable symboltable,
                                         String errorPrefix, StringBuilder errors) {
        String[] parts = line.split("\\s*[, ]\\s*");
        int length = parts.length;
        length = parts[length-1].equals("0")? length: length+1;
        int[] inputClause = new int[length];
        inputClause[0] = id;
        inputClause[1] = connective.ordinal();
        boolean okay = parseLiterals(parts,1,inputClause,2,symboltable,errorPrefix,errors);
        return okay ? inputClause : null;}


    /** parses a clause with a quantification (atleast, atmost, exactly), generates an InputClause and fills up the symboltable, if necessary.
     * If the symboltable is null, then the clause must consist of non-null integers.<br>
     * If the symboltable is not null, then the literals can be arbitrary strings. <br>
     * Negative literals are indicated by a preceding -.
     *
     * @param line        the line to be parsed.
     * @param id          the identifier for the new clause.
     * @param connective  the connective for the new clause.
     * @param symboltable null or a symboltable.
     * @param errorPrefix a prefix for the error messages
     * @param errors      for appending error messages.
     * @return            the new inputClause or null if errors have been detected.
     */
    protected static int[] parseWithQuantification(String line, int id, Connective connective,
                                                   Symboltable symboltable, String errorPrefix, StringBuilder errors) {
        String[] clause = line.split("\\s*[, ]\\s*");
        int length = clause.length;
        length = clause[length-1].equals("0")? length: length+1;
        int[] inputClause = new int[length];
        inputClause[0] = id;
        inputClause[1] = connective.ordinal();
        Integer amount = parseInteger(clause[1]);
        boolean okay = true;
        if(amount == null) {
            errors.append(errorPrefix).append("Quantification amount ").append(clause[1]).append(" is no number\n");
            okay = false;}
        else inputClause[2] = amount;
        okay &= parseLiterals(clause,2,inputClause,3,symboltable,errorPrefix, errors);
        return okay ? inputClause : null;}



    /** parses a line with an interval connective, generates an InputClause and fills up the symboltable, if necessary.
     * If the symboltable is null, then the clause must consist of non-null integers.<br>
     * If the symboltable is not null, then the literals can be arbitrary strings. <br>
     * Negative literals are indicated by a preceding -.
     *
     * @param line        the line to be parsed
     * @param id          the identifier for the new clause
     * @param symboltable a symboltable
     * @param errorPrefix a prefix for the error messages
     * @param errors      for appending error messages
     * @return            the parse clause
     */
    protected static int[] parseInterval(String line, int id, Symboltable symboltable, String errorPrefix, StringBuilder errors) {
        int position = line.indexOf("]");
        if(position < 0) {
            errors.append(errorPrefix).append("']' is missing");
            return null;}
        String[] parts = line.substring(position+1).trim().split("\\s*[, ]\\s*");
        int length = parts.length;
        length = parts[length-1].equals("0")? length+3: length+4;
        int[] inputClause = new int[length];
        boolean okay = parseLiterals(parts,0,inputClause,4,symboltable,errorPrefix,errors);
        inputClause[0] = id;
        inputClause[1] = Connective.INTERVAL.ordinal();
        String[] interval = line.substring(1,position).trim().split("\\s*[, ]\\s*");
        if(interval.length != 2) {
            errors.append(errorPrefix).append("Line has no proper interval: '").append(Arrays.toString(interval)).append("'\n");
            return null;}

        Integer min = parseInteger(interval[0]);
        Integer max = parseInteger(interval[1]);

        if(min == null) {
            errors.append(errorPrefix).append("Line has no proper interval: '").append(Arrays.toString(interval)).append("'\n");
            okay = false;}
        else{inputClause[2] = min;}

        if(max == null) {
            errors.append(errorPrefix).append("Line has no proper interval: '").append(Arrays.toString(interval)).append("'\n");
            okay = false;}
        else{inputClause[3] = max;}
        return okay ? inputClause : null;}



    /** parses the literal-part of a clause line, fills up inputClause with literals, and extends the symboltable.
     *
     * @param clause              the clause-line, split by , or blank.
     * @param startIndex          where the literals start.
     * @param inputClause         where to put the parsed literals into.
     * @param startIndexClause    start index of the inputClause.
     * @param symboltable         a symboltable or null.
     * @param errorPrefix a prefix for the error messages
     * @param errors              for appending error messages.
     * @return                    true if the parsing was successful.
     */
    protected static boolean parseLiterals(String[] clause, int startIndex, int[] inputClause,
                                           int startIndexClause, Symboltable symboltable,
                                           String errorPrefix, StringBuilder errors) {
        boolean okay = true;
        int length = clause.length;
        int endIndexParts = clause[length-1].equals("0") ? length-1: length;
        for(int i = startIndex; i < endIndexParts; ++i) {
            String part = clause[i];
            if(symboltable == null) {
               Integer literal = parseInteger(part);
                if(literal != null) {
                    if(literal == 0) {
                        errors.append(errorPrefix).append("Predicate 0 is not allowed.\n");
                        okay = false; continue;}
                    else {inputClause[startIndexClause++] = literal; continue;}}
                else {errors.append(errorPrefix).append("Mixing symbolic an alphanumeric literals is not allowed.\n");
                    okay = false; startIndexClause++;continue;}}

            int sign = 1;
            if(part.startsWith("-")) {sign = -1; part = part.substring(1);}
            int predicate = symboltable.getPredicate(part);
            if(predicate == 0) {
                errors.append(errorPrefix).append("Number of predicates: ").append(symboltable.predicates).
                        append(" is too small for literal '").append(part).append("'\n"); okay = false; startIndexClause++; continue;}
            inputClause[startIndexClause++] = sign*predicate;}
        return okay;}

}
