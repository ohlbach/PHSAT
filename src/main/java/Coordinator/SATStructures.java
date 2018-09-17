package Coordinator;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Clauses.ClauseType;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.ImplicationGraph;
import Datastructures.Theory.Model;

import java.util.*;

/**
 * Created by ohlbach on 14.09.2018.
 */
public class SATStructures {
    public int predicates;
    private ClauseList orClauses = null;
    private ClauseList disjointClauses = null;
    private ClauseList equivalences = null;
    private HashMap<Integer,Integer> replacements = new HashMap<>();

    private Model model;
    private ImplicationGraph implicationGraph;
    private LinkedList<Integer> units = new LinkedList<>();


    public SATStructures(int predicates) {
        this.predicates = predicates;
        Model model = new Model(predicates);
        implicationGraph = new ImplicationGraph(predicates);}


    public Result addBasicClause(int[] basicClause) {
        Unsatisfiable result = null;
        units.clear();
        switch(ClauseType.getType(basicClause[1])) {
            case OR:
                result = addBasicORClause(basicClause);
                break;
            case AND:
                for(int i = 1; i < basicClause.length; ++i) {units.add(basicClause[i]);}
                break;
            case XOR:
                result = addBasicORClause(basicClause);
                if(result != null) {return result;}
                result = addBasicDISJOINTClause(basicClause);
                break;
            case DISJOINT:
                result = addBasicDISJOINTClause(basicClause);
                break;
            case EQUIV:
                 result = addBasicEQUIVClause(basicClause);
                break;}
        if(result != null) {return result;}
        return unitConsequences(units);
    }

    /** simplifies a basicORClause and turns them into either units, parts of the implication graph,
     * or a Clause, added to ofClauses.
     *
     * @param basicClause a basic clause
     * @return null or an Unsatisfiable object.
     */
    public Unsatisfiable addBasicORClause(int[] basicClause) {
        Clause clause = makeORClause(basicClause);
        if(clause != null) {
            switch(clause.size()) {
                case 0: return new Unsatisfiable(model,basicClause);
                case 1: units.add(clause.cliterals.get(0).literal); return null;
                case 2: HashSet<Integer> literals = implicationGraph.addClause(clause.cliterals.get(0).literal,clause.cliterals.get(1).literal);
                        if(literals != null) {units.addAll(literals);}  // Konsequensen der Implikationen
                        return null;}
            orClauses.addClause(clause);}
        return null;}

    /** turns a basicORClause into a clause. <br/>
     * False literals and double literals are ignored. <br/>
     * True literals and complementary literals indicate tautologies. <br/>
     * Literals are replaced by their representatives in an equivalence class.
     * Implied literals are removed, i.e.  p,q,r and p -&gt; r causes remove(p)
     *
     * @param basicClause
     * @return the new simplified clause, or null if the clause is just to be ignored.
     */
    Clause makeORClause(int[] basicClause) {
        Clause clause = new Clause(basicClause[0],ClauseType.OR,basicClause.length);
        for(int i = 2; i < basicClause.length;++i) {
            int literal = replaceEquivalence(basicClause[i]);
            if(model.isTrue(literal)  || clause.cliterals.contains(-literal)) {return null;}
            if(model.isFalse(literal) || clause.cliterals.contains(literal)) {continue;}
            CLiteral cLiteral = new CLiteral(literal);
            clause.addCLiteralDirectly(cLiteral);}

        for(int i = 0; i < clause.size(); ++i) {   // p,q,r  and p -> r: remove p
            CLiteral cLiteral1 = clause.cliterals.get(i);
            TreeSet implied = implicationGraph.getImplicants(cLiteral1.literal);
            if(!implied.isEmpty()) {
                for(CLiteral cLiteral2 : clause.cliterals) {
                    if(cLiteral1 != cLiteral2 && implied.contains(cLiteral2.literal)) {
                        clause.removeLiteral(cLiteral1);
                        --i;
                        break;}}}}
        return clause;}


    /** adds a basicDISJOINTClause to the disjointClauses. <br/>
     * A true literal causes all other literals to become false (appended at units)<br/>
     * Two true literals are a contradiction <br/>
     * A false literal or p,-p are ignored<br/>
     * A double literal p,p causes -p to become true. <br/>
     * unitConsequences are computed, and the disjointness clauses are added to the implicationGraph.
     *
     * @param basicClause
     * @return either an Unsatisfiable object, or null.
     */
    public Unsatisfiable addBasicDISJOINTClause(int[] basicClause) {
        Clause clause = makeDISJOINTClause(basicClause);
        if(clause != null) {
            int size = clause.size();
            switch(size) {
                case 0: return new Unsatisfiable("Disjointness clause " + Arrays.toString(basicClause) + " became false");
                case 1: return null;
                default:
                    disjointClauses.addClause(clause);
                    for(int i = 0; i < size; ++i) {
                        int literal = clause.cliterals.get(i).literal;
                        for(int j = i+1; j < size; ++j) {
                            units.addAll(implicationGraph.addImplication(literal,-clause.cliterals.get(j).literal));}}}}
        return null;}

    /** turns a basicDISJOINTClause into a clause. <br/>
     * A true literal causes all other literals to become false (appended at units)<br/>
     * Two true literals are a contradiction <br/>
     * A false literal or p,-p are ignored <br/>
     * A double literal p,p causes -p to become true.
     *
     * @param basicClause
     * @return either an Unsatisfiable object, or the new clause.
     */
    Clause makeDISJOINTClause(int[] basicClause) {
        int trueLiteral = 0;
        Clause clause = new Clause(basicClause[0],ClauseType.DISJOINT,basicClause.length);
        for(int i = 1; i < basicClause.length; ++i) {
            int literal = replaceEquivalence(basicClause[i]);
            if(model.isTrue(literal)) {
                if(trueLiteral != 0) {return new Clause(basicClause[0],ClauseType.DISJOINT,0);}
                else {trueLiteral = literal;}
                for(int j = 1; j < basicClause.length; ++j) { // one literal true: all others must be false
                    if(i != j) {units.add(replaceEquivalence(-basicClause[j]));}}
                return null;}

            if(model.isFalse(literal) || clause.contains(-literal) >= 0) {continue;}

            int position = clause.contains(literal);
            if(position >= 0) {
                units.add(-literal);
                clause.removeLiteralAtPosition(position);}

            CLiteral cLiteral = new CLiteral(literal);
            clause.addCLiteralDirectly(cLiteral);}

        for(int i = 0; i < clause.size(); ++i) {   // p,q,r  and p -> r:  not p
            CLiteral cLiteral1 = clause.cliterals.get(i);
            TreeSet implied = implicationGraph.getImplicants(cLiteral1.literal);
            if(!implied.isEmpty()) {
                for(CLiteral cLiteral2 : clause.cliterals) {
                    if(cLiteral1 != cLiteral2 && implied.contains(cLiteral2.literal)) {
                        clause.removeLiteral(cLiteral1);
                        units.add(-cLiteral1.literal);
                        --i;
                        break;}}}}

        return clause;}


    public Unsatisfiable addBasicEQUIVClause(int[] basicClause) {
        Clause clause = makeEQUIVClause(basicClause);
        if(clause != null) {
            switch(clause.size()) {
                case 0: return new Unsatisfiable("Equivalence clause " + Arrays.toString(basicClause) + " became false");
                case 1: return null;
                default:
                    equivalences.addClause(clause);
                    int representative = clause.cliterals.get(0).literal;
                    for(int i = 1; i < clause.cliterals.size(); ++i) {
                        int literal = clause.cliterals.get(i).literal;
                        replacements.put(literal,representative);
                        replacements.put(-literal,-representative);}}}
        return null;}



    /** turns a basicEQUIVClause into a clause. <br/>
     * A true literal causes all other literals to become true (appended at units)<br/>
     * A false literal causes all other literals to become false (appended at units)<br/>
     * p &lt;=&gt; -p is a contradiction.<br/>
     * A double literal p,p is ignored.<br/>
     * p,q,r and p -&gt; not r  causes all literals to become false.
     *
     * @param basicClause
     * @return either an Unsatisfiable object, or null.
     */
    Clause makeEQUIVClause(int[] basicClause) {
        Clause clause = new Clause(basicClause[0],ClauseType.EQUIV,basicClause.length);
        for(int i = 1; i < basicClause.length;++i) {
            int literal = replaceEquivalence(basicClause[i]);
            if(model.isTrue(literal)) {
                for(int j = 1; j < basicClause.length; ++j) {
                    if(i != j) {units.add(replaceEquivalence(basicClause[j]));}}
                return null;}

            if(model.isFalse(literal)) {
                for(int j = 1; j < basicClause.length; ++j) {
                    if(i != j) {units.add(-replaceEquivalence(basicClause[j]));}}
                return null;}

            if(clause.contains(literal) >= 0) {continue;}

            if(clause.contains(-literal) >= 0) {return new Clause(basicClause[0],ClauseType.EQUIV,0);}

            CLiteral cLiteral = new CLiteral(literal);
            clause.addCLiteralDirectly(cLiteral);}

        for(int i = 0; i < clause.size(); ++i) {   // p,q,r  and p -> -r:  all become false.
            CLiteral cLiteral1 = clause.cliterals.get(i);
            TreeSet implied = implicationGraph.getImplicants(cLiteral1.literal);
            if(!implied.isEmpty()) {
                for(CLiteral cLiteral2 : clause.cliterals) {
                    if(cLiteral1 != cLiteral2 && implied.contains(-cLiteral2.literal)) {
                        for(CLiteral cl : clause.cliterals) {units.add(-cl.literal);}
                        return null;}}}}
        return clause;}


    /** maps literals to their representative in the equivalence class.
     *
     * @param literal the literal
     * @return either the literal itself, or its representative.
     */
    int replaceEquivalence(int literal) {
        Integer replaced = replacements.get(literal);
        return (replaced == null) ? literal : replaced;}

    void replaceEquivalents(Clause equivalence) {
        int representative = equivalence.cliterals.get(0).literal;
        for(int i = 1; i < equivalence.cliterals.size(); ++i) {
            int literal = equivalence.cliterals.get(i).literal;
            replaceEquivalenceOR(representative,literal);
            replaceEquivalenceOR(-representative,-literal);
            replaceEquivalenceDISJOINT(representative,literal);
            replaceEquivalenceDISJOINT(-representative,-literal);
            replaceEquivalenceImplication(representative,literal);
            replaceEquivalenceImplication(-representative,-literal);

        }
    }

    void replaceEquivalenceOR(int representative, int literal) {
        for(CLiteral cLiteral : orClauses.literalIndex.getLiterals(literal)) {
            if(!orClauses.replaceBy(cLiteral,representative)) {
                Clause clause = cLiteral.getClause();
                switch(clause.size()) {
                    case 1: units.add(representative); return;
                    case 2: implicationGraph.addClause(clause.cliterals.get(0).literal, clause.cliterals.get(1).literal);
                }}}}


    Result replaceEquivalenceDISJOINT(int representative, int literal) {
        for(CLiteral cLiteral : disjointClauses.literalIndex.getLiterals(literal)) {
            if(!disjointClauses.replaceBy(cLiteral,representative)) {
                return new Unsatisfiable("Disjoint Clause " + cLiteral.getClause().toString() + "" +
                        " contains the equivalent literals " + literal + " and " + representative);}}
        return null;}

    Result replaceEquivalenceImplication(int representative, int literal) {
        units.addAll(implicationGraph.replaceEquivalences(representative,literal));
        units.addAll(implicationGraph.replaceEquivalences(-representative,-literal));
        return null;}


    public Result unitConsequences(LinkedList<Integer> units) {
        Result result;
        while(!units.isEmpty()) {
            int literal = units.pollFirst();
            switch(model.add(literal)) {
                case 1: continue;
                case -1: return new Unsatisfiable(model,literal);}
            if(orClauses != null) {orConsequences(literal, units);}
            if(disjointClauses != null) {disjointConsequences(literal,units);}
            if(equivalences != null) {
                equivalenceConsequences(literal,units);}}
     return null;}

    public void orConsequences(int literal, LinkedList<Integer> units) {
        orClauses.removeClausesWithLiteral(literal);
        for(CLiteral cliteral : orClauses.literalIndex.getLiterals(-literal)) {
            Clause clause = cliteral.getClause();
            switch(orClauses.removeLiteral(cliteral)) {
                case 1:
                    units.addLast(clause.cliterals.get(0).literal);
                    orClauses.removeClause(clause);
                break;
                case 2:
                    orClauses.removeClause(clause);
                    HashSet<Integer> lits = implicationGraph.addClause(clause.cliterals.get(0).literal,clause.cliterals.get(1).literal);
                    if(lits != null) {units.addAll(lits);}}}}


    public void disjointConsequences(int literal, LinkedList<Integer> units) {
        for(CLiteral cliteral : disjointClauses.literalIndex.getLiterals(literal)) {
            Clause clause = cliteral.getClause();
            for(CLiteral lit : clause.cliterals) {
                if(lit.literal != literal) {units.addLast(-lit.literal);}}}
        for(CLiteral cliteral : disjointClauses.literalIndex.getLiterals(-literal)) {
            Clause clause = cliteral.getClause();}}

    public void equivalenceConsequences(int literal, LinkedList<Integer> units) {
        for(CLiteral cliteral : equivalences.literalIndex.getLiterals(literal)) {
            Clause clause = cliteral.getClause();
            for(CLiteral lit : clause.cliterals) {
                if(lit.literal != literal) {units.addLast(lit.literal);}}
            equivalences.removeClause(clause);}
        for(CLiteral cliteral : equivalences.literalIndex.getLiterals(-literal)) {
            Clause clause = cliteral.getClause();
            for(CLiteral lit : clause.cliterals) {
                if(lit.literal != literal) {units.addLast(-lit.literal);}}
            equivalences.removeClause(clause);}}


}
