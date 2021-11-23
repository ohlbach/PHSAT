package Generators;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.ClauseOld;
import Datastructures.Clauses.Connective;
import Datastructures.Symboltable;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by ohlbach on 27.08.2018.
 *
 * This generator creates a clause set from literal names.
 * This is basically for test purposes.
 */
public final class StringClauseSetGenerator  {

    /** transfers the disjunctions (String) unchanged to the result
     *
     * @param parameters a HashMap with key "disjunctions"
     * @param errors    no effect
     * @param warnings  no effect
     * @return a HashMap with "clauseString"
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters,
                                                                           StringBuilder errors, StringBuilder warnings){
        HashMap<String,Object> map = new HashMap<>();
        map.put("clauseString", parameters.get("disjunctions").trim());
        ArrayList<HashMap<String,Object>> list = new ArrayList<>();
        list.add(map);
        return list;}

    /** generates a help string
     *
     * @return a help string
     */
    public static String help() {
        return "StringClauseSetGenerator just parses a string of clauses.\n" +
                "A clause is just a single line of literals.\n" +
                "The literals in the clause may by any strings, possibly preceded by -.\n" +
                "A clause starting with '$' indicates a special meaning of the clause:\n" +
                "$a ... means conjunctions (all literals must be true).\n" +
                "$x ... means xors          (excactly one literal must be true).\n" +
                "$d ... means disjointness (at most one literal can be true).\n" +
                "$e ... means equivalence  (either all literals are true, or all literals are false).";}


    /** parses the clause string and generates a BasicClauseList object.
     *
     * @param parameters the HashMap with key "clauseString"
     * @param errors   for error messages
     * @param warnings no effect
     * @return  the new BasicClauseList object.
     */
    public static  BasicClauseList generate(HashMap<String,Object> parameters, StringBuilder errors, StringBuilder warnings) {
        String clausesString = (String)parameters.get("clauseString");
        HashMap<String,Integer> name2Int = new HashMap<>();
        String[] clausesStrings = clausesString.split("\\s*\\n\\s*");
        int predicates = 0;
        int clauseCounter = 0;

        boolean disjointness = false;
        for(String clauseString : clausesStrings) {
            if(clauseString.trim().startsWith("disjoint")) {disjointness = true; break;}}

        BasicClauseList bcl = new BasicClauseList();
        bcl.info = "String-generated clauses:\n" + clausesString;

        ArrayList<ClauseOld> clauseList = new ArrayList<>();
        int clauseNumber = 0;
        for(String clauseString : clausesStrings) {
            if(clauseString.isEmpty()) {continue;}
            String[] literals = clauseString.split("\\s*(,| )\\s*");
            int start = 0;
            int length = literals.length+2;
            Connective type = Connective.OR;
            if(literals[0].startsWith("$")) {
                start = 1;
                length = literals.length+1;
                type = Connective.getType(literals[0].charAt(1));
                if(type == null) {
                    errors.append("Illegal ClauseType: '" + literals[0] + "' in " + clausesString + ".\n"+
                    "Should be one of a (and), x (xors), d (disjoint), e (equivalence)\n");
                    continue;}}
            int[] lits = new int[length];
            lits[0] = ++clauseNumber;
            lits[1] = type.ordinal();
            int n = 1;
            for(int i = start; i < literals.length; ++i) {
                String literal = literals[i];
                int sign = 1;
                if(literal.startsWith("-")) {sign = -1; literal = literal.substring(1,literal.length());}
                Integer number = name2Int.get(literal);
                if(number == null) {number = ++predicates; name2Int.put(literal,number);}
                lits[++n] = number*sign;}
            bcl.addClause(lits,"",errors,warnings);}
        Symboltable symboltable = new Symboltable(predicates);
        name2Int.forEach((name,predicate) -> symboltable.setName(predicate,name));
        bcl.symboltable = symboltable;
        bcl.predicates = predicates;
        //errors.append(bcl.syntaxErrors.toString());
        return bcl;
    }
}
