package Generators;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Connective;
import Datastructures.Symboltable;
import Management.Monitor;
import Management.ProblemSupervisor;
import Utilities.Utilities;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import static Utilities.Utilities.parseInteger;

/**
 * Created by ohlbach on 27.08.2018.
 *
 * This generator creates a clause set from literal names.
 * This is basically for test purposes.
 */
public final class StringClauseSetGenerator  {

    /** transfers the disjunctions (String) unchanged to the result
     *
     * @param parameters a HashMap with key "clauses"
     * @param errors    no effect
     * @param warnings  no effect
     * @return a HashMap with "clauseString"
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters,
                                                                           Monitor errors, Monitor warnings){
        HashMap<String,Object> map = new HashMap<>();
        map.put("clauses", parameters.get("clauses").trim());
        ArrayList<HashMap<String,Object>> list = new ArrayList<>();
        list.add(map);
        return list;}

    /** generates a help string
     *
     * @return a help string
     */
    public static String help() {
        return "StringClauseSetGenerator just parses a string of clauses.\n" +
                "It has to start with\n" +
                "p predicates\n" +
                "A clause is just a single line of literals.\n" +
                "The literals in the clause may by any strings, possibly preceded by -.\n" +
                "A clause starting with a special symbol indicates a special meaning of the clause:\n" +
                "& ...     conjunctions (all literals must be true).\n" +
                "e ...     equivalence  (all literals are equivalent).\n" +
                "<= n ...  at most n literals are true\n"+
                ">= n ...  at least n literals are true\n"+
                "= n ...   exaclty n literals are true\n"+
                "[n,m] ... interval (between n and m literal are true).\n"+
                "l1,...    or-clause\n"+
                "The literals may be names or integers != 0";}


    /** parses the clause string and generates a BasicClauseList object.
     *
     * @param parameters the HashMap with key "clauseString"
     * @param errors   for error messages
     * @param warnings no effect
     * @return  the new BasicClauseList object.
     */
    public static  BasicClauseList generate(HashMap<String,Object> parameters,
                                            ProblemSupervisor problemSupervisor, Monitor errors, Monitor warnings) {
        String clauses = (String)parameters.get("clauses");
        String[] lines = clauses.split("\\s*\\n\\s*");
        String line = lines[0];
        if(!line.startsWith("p")) {
            errors.print("First line '" + line + "' does not start with p\n");
            return null;}
        String[] parts = line.split("\\s+");
        if(parts.length != 2) {
            errors.print("First line '" + line + "' does not contain predicates\n");
            return null;}
        Integer predicates = parseInteger(parts[1]);
        if(predicates == null) {
            errors.print("First line '" + line + "' does not contain predicates\n");
            return null;}
        Symboltable symboltable = new Symboltable(predicates);
        BasicClauseList bcl = new BasicClauseList(predicates,symboltable,"String Generator");
        int id = 1;
        for(int i = 1; i < lines.length; ++i) {
            String errorPrefix = "Line " + i + ": '"+line + "'";
            line = lines[i].trim();
            if(line.isEmpty() || line.startsWith("%")) continue;
            int[] clause = parseLine(line,id,symboltable,errorPrefix,errors);
            if(clause == null) continue;
            ++id;
            bcl.addClause(clause,errorPrefix,errors,warnings);}
        problemSupervisor.clauseCounter = id;
        return bcl;
    }

    /** parses a single clause-line
     *
     * @param line        the line to be parsed
     * @param id          the identifier for the new clause
     * @param symboltable a symboltable
     * @param errors      for appending error messages
     * @return            the parsed clause
     */
    public static int[] parseLine(String line, int id, Symboltable symboltable,  String errorPrefix, Monitor errors) {
        switch(line.charAt(0)) {
            case '&': return parseSingle(line.substring(1).trim(), id, Connective.AND, symboltable, errorPrefix, errors);
            case 'e': return parseSingle(line.substring(1).trim(), id, Connective.EQUIV, symboltable, errorPrefix, errors);
            case '<': return parseWithQuantification(line.substring(2).trim(), id, Connective.ATMOST, symboltable, errorPrefix, errors);
            case '>': return parseWithQuantification(line.substring(2).trim(), id, Connective.ATLEAST, symboltable, errorPrefix, errors);
            case '=': return parseWithQuantification(line.substring(2).trim(), id, Connective.EXACTLY, symboltable, errorPrefix, errors);
            case '[': return parseInterval(line, id, symboltable, errorPrefix, errors);
            default:  return parseOr(line.trim(), id, symboltable, errorPrefix, errors);}}


    /** parses an or-clause
     *
     * @param line        the line to be parsed
     * @param id          the identifier for the clause
     * @param symboltable a symboltable
     * @param errors      for appending error messaes
     * @return            the parsed clause
     */
    private static int[] parseOr(String line, int id, Symboltable symboltable, String errorPrefix, StringBuilder errors) {
        String[] parts = line.split("\\s*[, ]\\s*");
        int[] basicClause = new int[parts.length+2];
        basicClause[0] = id;
        basicClause[1] = Connective.OR.ordinal();
        boolean okay = parseLiterals(parts,0,basicClause,2,symboltable,errorPrefix,errors);
        return okay ? basicClause : null;}

    /** parses a clause without a quantification amount (& or =)
     *
     * @param line        the line to be parsed
     * @param id          the identifier for the new clause
     * @param connective  the connective for the new clause
     * @param symboltable a symboltable
     * @param errors      for appending error messages
     * @return            the parse clause
     */
    private static int[] parseSingle(String line, int id, Connective connective, Symboltable symboltable, String errorPrefix,StringBuilder errors) {
        String[] parts = line.split("\\s*[, ]\\s*");
        int[] basicClause = new int[parts.length+2];
        basicClause[0] = id;
        basicClause[1] = connective.ordinal();
        boolean okay = parseLiterals(parts,0,basicClause,2,symboltable,errorPrefix,errors);
        return okay ? basicClause : null;}


    /** parses a clause with a quantification (at least, at most)
     *
     * @param line        the line to be parsed
     * @param id          the identifier for the new clause
     * @param connective  the connective for the new clause
     * @param symboltable a symboltable
     * @param errors      for appending error messages
     * @return            the parse clause
     */
    private static int[] parseWithQuantification(String line, int id, Connective connective, Symboltable symboltable, String errorPrefix, StringBuilder errors) {
        String[] parts = line.split("\\s*[, ]\\s*");
        int[] basicClause = new int[parts.length+2];
        basicClause[0] = id;
        basicClause[1] = connective.ordinal();
        Integer amount = Utilities.parseInteger(errorPrefix,parts[0],errors);
        boolean okay = true;
        if(amount == null) {okay = false;}
        else basicClause[2] = amount;
        okay &= parseLiterals(parts,1,basicClause,3,symboltable,errorPrefix,errors);
        return okay ? basicClause : null;}

    /** parses a line with an interval connective.
     * Only syntactic checks are done. Semantic checks are done in BasicClauseList
     *
     * @param line        the line to be parsed
     * @param id          the identifier for the new clause
     * @param symboltable a symboltable
     * @param errors      for appending error messages
     * @return            the parse clause
     */
    private static int[] parseInterval(String line, int id, Symboltable symboltable, String errorPrefix,Monitor errors) {
        int position = line.indexOf("]");
        boolean okay = true;
        if(position < 0) {
            errors.print(errorPrefix).append("'] not found\n");
            return null;};
        String[] parts = line.substring(position+1).trim().split("\\s*[, ]\\s*");
        int[] basicClause = new int[parts.length + 4];
        basicClause[0] = id;
        basicClause[1] = Connective.INTERVAL.ordinal();
        String[] interval = line.substring(1,position).trim().split("\\s*[, ]\\s*");
        if(interval.length != 2) {
            errors.append(errorPrefix).append("has no proper interval: '"+interval+"'\n");
            okay = false;}
        Integer min = Utilities.parseInteger(errorPrefix,interval[0],errors);
        Integer max = Utilities.parseInteger(errorPrefix,interval[1],errors);

        if(min == null) {okay = false;}
        else{basicClause[2] = min;}

        if(max == null) {okay = false;}
        else{basicClause[3] = max;}
        okay &= parseLiterals(parts,0,basicClause,4,symboltable,
                errorPrefix,errors);
        return okay ? basicClause : null;}





        /** parses the literal-part of a clause line
         *
         * @param parts         the clause-line, split by , or blank
         * @param startPart     where the literal start
         * @param basicClause   where to put the parsed literals into
         * @param positionBC    start index of the basicClause
         * @param symboltable   a symboltable
         * @param errorPrefix   for errors
         * @param errors        for appending errors
         * @return              true if the parsing was successful
         */
    private static boolean parseLiterals(String[] parts, int startPart, int[] basicClause,
                                int positionBC, Symboltable symboltable,
                                String errorPrefix, StringBuilder errors) {
        boolean okay = true;
        for(int i = startPart; i < parts.length; ++i) {
            String part = parts[i];
            Integer literal = parseInteger(part);
            if(literal != null) {
                if(literal == 0) {
                    errors.append(errorPrefix).append("Predicate 0 is not allowed\n");
                    okay = false; continue;}
                else {
                    symboltable.symbolic = false;
                    basicClause[positionBC++] = literal; continue;}}

            if(!symboltable.symbolic) {
                errors.append(errorPrefix).append("Mixing symbolic an alphanumeric literals is not allowed\n");
                return false;}

            int sign = 1;
            if(part.equals("-")) {sign = -1; part = parts[++i];}
            else{if(part.startsWith("-")){sign = -1; part = part.substring(1);}}
            if(!part.matches("[A-Za-z0-9]+")) {
                errors.append(errorPrefix).append("Literal '" + part + "' is not alphanumeric\n");
                okay = false;}
            int predicate = symboltable.getPredicate(part);
            if(predicate == 0) {errors.append(errorPrefix).append("Number of predicates: " + symboltable.predicates +
                    " is too small for literal '" + part + "'\n"); okay = false; continue;}
            basicClause[positionBC++] = sign*predicate;}
        return okay;}


}
