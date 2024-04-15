package ProblemGenerators;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import Management.Parameters;
import Utilities.Utilities;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;

import static Utilities.Utilities.parseInteger;

/** This is the interface to the generator classes.
 * The generator classes generate SAT-Problems from different sources. <br>
 * Each generator class should provide the following static methods: <br>
 *  - public static Parameters makeParameter() for generating the parameters for the GUI.
 *  - public static void makeProblemGenerator(Parameters parameters,ArrayList&lt;ProblemGenerator%gt; generators) <br>
 *    for actually generating the generators based on the parameters.
 *  - public InputClauses generateProblem(StringBuilder errors); for generating the clauses. <br>
  */
public abstract class ProblemGenerator {

    /** the constructor is never used */
    public ProblemGenerator() {}

    /** takes the parsed input clauses */
    public InputClauses inputClauses = null;

    /** the list of generator classes */
    public static ArrayList<Class> generatorClasses = new ArrayList<>();
    static{
        generatorClasses.add(ProblemGenerators.CNFReader.class);
        generatorClasses.add(ProblemGenerators.RandomClauseSetGenerator.class);
        generatorClasses.add(ProblemGenerators.PigeonHoleGenerator.class);
        generatorClasses.add(ProblemGenerators.PythagoraenTriples.class);
        generatorClasses.add(ProblemGenerators.StringClauseSetGenerator.class);
    }

    /**
     * Constructs a list of Parameters objects by invoking the "makeParameter" method in each generator class.
     *
     * @return an ArrayList of Parameters objects
     */
    public static ArrayList<Parameters> makeParameters() {
        ArrayList<Parameters> parameters = new ArrayList<>();
        for(Class clazz : generatorClasses) {
            try{
                Method method = clazz.getMethod("makeParameter");
                parameters.add((Parameters)method.invoke(null));}
            catch(Exception ignored){}}
            return parameters;}


    /** generates the InputClauses
     *
     * @param errors    for error massages.
     * @return null or the new InputClauses.
     */
    public abstract InputClauses generateProblem(StringBuilder errors);

    /** parses the clause string and generates a InputClauses object.
     *
     * @param problemName the name of the example problem.
     * @param lineIterator for iterating over the lines.
     * @param errors   for error messages.
     * @return  null or an InputClauses object.
     */
    protected static InputClauses parseClauses(String problemName, Iterator<String> lineIterator,StringBuilder errors) {
        InputClauses inputClauses = new InputClauses();
        inputClauses.problemId = problemName;
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
            clause = InputClauses.checkSyntax(clause,predicates,errorPrefix,errors);
            if(clause == null) continue;
            inputClauses.addClause(clause);}
        if(!info.isEmpty()) inputClauses.info = info.toString();
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
            case '&': return parseAndEquiv(line.trim(), id, Quantifier.AND, symboltable, errorPrefix,errors);
            case 'e': return parseAndEquiv(line.trim(), id, Quantifier.EQUIV, symboltable, errorPrefix,errors);
            case '<': return parseWithQuantification(line.trim(), id, Quantifier.ATMOST, symboltable, errorPrefix,errors);
            case '>': return parseWithQuantification(line.trim(), id, Quantifier.ATLEAST, symboltable, errorPrefix,errors);
            case '=': return parseWithQuantification(line.trim(), id, Quantifier.EXACTLY, symboltable, errorPrefix, errors);
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
        inputClause[1] = Quantifier.OR.ordinal();
        boolean okay = parseLiterals(clause,0,inputClause,2,symboltable,errorPrefix,errors);
        return okay ? inputClause : null;}



    /** parses an and- and an equiv-clause, generates an InputClause and fills up the symboltable, if necessary.
     * If the symboltable is null, then the clause must consist of non-null integers.<br>
     * If the symboltable is not null, then the literals can be arbitrary strings. <br>
     * Negative literals are indicated by a preceding -.
     *
     * @param line        the line to be parsed.
     * @param id          the identifier for the new clause.
     * @param quantifier  the connective for the new clause.
     * @param symboltable null or a symboltable.
     * @param errorPrefix a prefix for the error messages
     * @param errors      for appending error messages.
     * @return            the new inputClause or null if errors have been detected.
     */
    protected static int[] parseAndEquiv(String line, int id, Quantifier quantifier, Symboltable symboltable,
                                         String errorPrefix, StringBuilder errors) {
        String separators = (quantifier == Quantifier.EQUIV) ? "\\s*[,= ]\\s*" : "\\s*[, ]\\s*";
        String[] parts = line.split(separators);
        int length = parts.length;
        length = parts[length-1].equals("0")? length: length+1;
        int[] inputClause = new int[length];
        inputClause[0] = id;
        inputClause[1] = quantifier.ordinal();
        boolean okay = parseLiterals(parts,1,inputClause,2,symboltable,errorPrefix,errors);
        return okay ? inputClause : null;}


    /** parses a clause with a quantification (atleast, atmost, exactly), generates an InputClause and fills up the symboltable, if necessary.
     * If the symboltable is null, then the clause must consist of non-null integers.<br>
     * If the symboltable is not null, then the literals can be arbitrary strings. <br>
     * Negative literals are indicated by a preceding -.
     *
     * @param line        the line to be parsed.
     * @param id          the identifier for the new clause.
     * @param quantifier  the connective for the new clause.
     * @param symboltable null or a symboltable.
     * @param errorPrefix a prefix for the error messages
     * @param errors      for appending error messages.
     * @return            the new inputClause or null if errors have been detected.
     */
    protected static int[] parseWithQuantification(String line, int id, Quantifier quantifier,
                                                   Symboltable symboltable, String errorPrefix, StringBuilder errors) {
        String[] clause = line.split("\\s*[, ]\\s*");
        int length = clause.length;
        length = clause[length-1].equals("0")? length: length+1;
        int[] inputClause = new int[length];
        inputClause[0] = id;
        inputClause[1] = quantifier.ordinal();
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
        inputClause[1] = Quantifier.INTERVAL.ordinal();
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
