package Generators;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Symboltable;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by ohlbach on 27.08.2018.
 *
 * This generator creates a clause set from literal names.
 * This is basically for test purposes.
 */
public class StringClauseSetGenerator  {

    /** transfers the clauses (String) unchanged to the result
     *
     * @param parameters a HashMap with key "clauses"
     * @param errors    no effect
     * @param warnings  no effect
     * @return a HashMap with "clauseString"
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters,
                                                                           StringBuffer errors, StringBuffer warnings){
        HashMap<String,Object> map = new HashMap<>();
        map.put("clauseString", parameters.get("clauses").trim());
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
                "The literals in the clauses may by any strings, possibly preceded by -.\n" +
                "A clause starting with 'disjoint' indicates a set of disjoint predicates.\n" +
                "At most one of these predicates can be true in a model.";}


    /** parses the clause string and generates a BasicClauseList object.
     *
     * @param parameters the HashMap with key "clauseString"
     * @param errors   no effect
     * @param warnings no effect
     * @return a HashMap with key "clauses" and as value a BasicClauseList object.
     */
    public static  HashMap<String,Object> generate(HashMap<String,Object> parameters, StringBuffer errors, StringBuffer warnings) {
        String clausesString = (String)parameters.get("clauseString");
        HashMap<String,Integer> name2Int = new HashMap<>();
        String[] clausesStrings = clausesString.split("\\s*\\n\\s*");
        int predicates = 0;
        int clauseCounter = 0;

        boolean disjointness = false;
        for(String clauseString : clausesStrings) {
            if(clauseString.trim().startsWith("disjoint")) {disjointness = true; break;}}

        BasicClauseList bcl = new BasicClauseList(disjointness);
        bcl.info = "String-generated clauses:\n" + clausesStrings;

        ArrayList<Clause> clauseList = new ArrayList<>();
        for(String clauseString : clausesStrings) {
            if(clauseString.isEmpty()) {continue;}
            String[] literals = clauseString.split("\\s*(,| )\\s*");
            boolean disjoint = literals[0].equals("disjoint");
            int start = disjoint ? 1 : 0;
            int shift = (disjointness && start == 0) ? 1 : 0;
            int length = literals.length;
            if(disjointness && !disjoint) ++length;
            int[] lits = new int[length];
            if(disjointness) {lits[0] = disjoint ? 1: 0;}
            for(int i = start; i < literals.length; ++i) {
                String literal = literals[i];
                int sign = 1;
                if(literal.startsWith("-")) {sign = -1; literal = literal.substring(1,literal.length());}
                Integer number = name2Int.get(literal);
                if(number == null) {number = ++predicates; name2Int.put(literal,number);}
                lits[i+shift] = number*sign;}
            bcl.clauses.add(lits);}
        Symboltable symboltable = new Symboltable(predicates);
        name2Int.forEach((name,predicate) -> symboltable.setName(predicate,name));
        bcl.symboltable = symboltable;
        bcl.predicates = predicates;
        parameters.put("clauses",bcl);
        return parameters;
    }
}
