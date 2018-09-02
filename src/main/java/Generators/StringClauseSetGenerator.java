package Generators;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Model;
import Datastructures.Status;
import Datastructures.Symboltable;
import Generators.ClauseSetGenerator;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by ohlbach on 27.08.2018.
 *
 * This generator creates a clause set from literal names.
 */
public class StringClauseSetGenerator extends ClauseSetGenerator {
    private String clausesString;

    /** constructs the generator
     *
     * @param clausesString the clauses (\\n-separated) as a string of literal names (,-separated)
     */
    public StringClauseSetGenerator(String clausesString) {
        this.clausesString = clausesString;}

    /** generates the clause list
     *
     * @return the Status object with all the information about the clause set.
     */
    public Status generate() {
        HashMap<String,Integer> name2Int = new HashMap<>();
        String[] clausesStrings = clausesString.split("\\s*\\n\\s*");
        int predicates = 0;
        int clauseCounter = 0;
        ArrayList<Clause> clauseList = new ArrayList<>();
        for(String clauseString : clausesStrings) {
            String[] literals = clauseString.split("\\s*,\\s*");
            Clause clause = new Clause(++clauseCounter,literals.length);
            for(String literal : literals) {
                int sign = 1;
                if(literal.startsWith("-")) {sign = -1; literal = literal.substring(1,literal.length());}
                Integer number = name2Int.get(literal);
                if(number == null) {number = ++predicates; name2Int.put(literal,number);}
                clause.addLiteral( new CLiteral(number*sign));}
            clauseList.add(clause);}
        Symboltable symboltable = new Symboltable(predicates);
        name2Int.forEach((name,predicate) -> symboltable.setName(predicate,name));
        Model model = new Model(predicates);
        ClauseList cl = new ClauseList(clausesStrings.length,model,symboltable);
        for(Clause clause : clauseList) {cl.addClause(clause);}
        Status status = new Status();
        status.toBeExamined = true;
        status.clauseList = cl;
        return status;
    }
}
