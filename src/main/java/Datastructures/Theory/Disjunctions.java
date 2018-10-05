package Datastructures.Theory;

import Algorithms.Algorithms;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.TreeSet;
import java.util.function.Consumer;

/**
 * Created by ohlbach on 21.09.2018.
 */
public class Disjunctions {
    private int predicates;
    private Model model;
    public ClauseList disjunctions;
    private ImplicationDAG implicationDAG = null;
    private EquivalenceClasses equivalences = null;
    /** reports true literals */
    public ArrayList<Consumer<Integer>> trueLiteralObservers = new ArrayList();
    public ArrayList<Consumer<Unsatisfiable>> unsatisfiabilityObservers = new ArrayList();

    public Disjunctions(int size, Model model, ImplicationDAG implicationDAG, EquivalenceClasses equivalences) {
        predicates = model.predicates;
        this.model = model;
        this.implicationDAG = implicationDAG;
        this.equivalences = equivalences;
        disjunctions = new ClauseList(size,predicates);
    }

    public void addLiteralRemovalObservers(Consumer<Clause> observer) {
        disjunctions.literalRemovalObservers.add(observer);}

    public void addLiteralReplacementObserver(Consumer<CLiteral> observer) {
        disjunctions.literalReplacementObservers.add(observer);}

    public void addPurityObserver(Consumer<Integer> observer) {
        disjunctions.addPurityObserver(observer);}

    /** simplifies a basicORClause and turns them into either units, parts of the implication graph,
     * or a Clause, added to ofClauses.
     *
     * @param basicClause a basic clause
     * @return null or an Unsatisfiable object.
     */
    public void addBasicDisjunction(int[] basicClause) {
        Clause clause = makeDisjunction(basicClause);
        for(int i = 0; i < 2; ++i) {
            if(clause == null) {return;}
            switch(clause.size()) {
                case 0: reportUnsatisfiable(model,basicClause); return;
                case 1: reportTrueLiteral(clause.getLiteral(0)); return;
                case 2: implicationDAG.addClause(clause.getLiteral(0),clause.getLiteral(1));
                    return;}
            if(i == 0) {clause = Algorithms.subsumedAndResolved(clause,disjunctions, implicationDAG);}}
        disjunctions.addClause(clause);}

    /** turns a basicClause into a clause. <br/>
     * False literals and double literals are ignored. <br/>~
     * True literals and complementary literals indicate tautologies. <br/>
     * Literals are replaced by their representatives in an equivalence class.
     * Implied literals are removed, i.e.  p,q,r and p -&gt; r causes remove(p)
     *
     * @param basicClause
     * @return the new simplified clause, or null if the clause is just to be ignored.
     */
    Clause makeDisjunction(int[] basicClause) {
        Clause clause = new Clause(""+basicClause[0],basicClause.length);
        for(int i = 2; i < basicClause.length;++i) {
            int literal = equivalences.mapToRepresentative(basicClause[i]);
            if(model.isTrue(literal)  || clause.cliterals.contains(-literal)) {return null;}
            if(model.isFalse(literal) || clause.cliterals.contains(literal)) {continue;}
            CLiteral cLiteral = new CLiteral(literal);
            clause.addCLiteralDirectly(cLiteral);}

        for(int i = 0; i < clause.size(); ++i) {   // p,q,r  and p -> r: remove p
            CLiteral cLiteral1 = clause.cliterals.get(i);
            TreeSet implied = implicationDAG.getImplicants(cLiteral1.literal);
            if(!implied.isEmpty()) {
                for(CLiteral cLiteral2 : clause.cliterals) {
                    if(cLiteral1 != cLiteral2 && implied.contains(cLiteral2.literal)) {
                        clause.removeLiteral(cLiteral1);
                        --i;
                        break;}}}}
        return clause;}


    public void makeTrue(int literal) {
        disjunctions.makeTrue(literal);}

    public void removeLiteral(int literal) {
        disjunctions.removeLiteral(literal);
    }

    public boolean isEmpty() {
        return disjunctions.isEmpty();
    }


    /** processes the consequences of a two-literal clause p,q.
     *  In all clauses with -p,q  -p is removed <br/>
     *  In all clauses with p,-q -q is removed <br/>
     *  In two clauses -p,rest1 and -q,rest2, where rest1 is subset of rest2, -q is removed, and vice versa.<br/>
     *  if in addition the two rests have equal length, one of the clauses is entirely removed.
     *
     * @param literal1
     * @param literal2
     */
    public void simplifyClauses(int literal1, int literal2) {
        ArrayList<Object> toBeRemoved = new ArrayList<>();
        for(CLiteral cLiteral : disjunctions.getLiterals(-literal1)) {
            if(cLiteral.clause.contains(literal2) >= 0) {toBeRemoved.add(cLiteral);}}
        for(CLiteral cLiteral : disjunctions.getLiterals(-literal2)) {
            if(cLiteral.clause.contains(literal1) >= 0) {toBeRemoved.add(cLiteral);}}
        for(Object cLiteral : toBeRemoved) {disjunctions.removeLiteral((CLiteral)cLiteral);}

        toBeRemoved.clear();
        for(CLiteral cLiteral1 : disjunctions.getLiterals(-literal1)) {
            Clause clause1 = cLiteral1.clause;
            int size1 = clause1.size();
            for(CLiteral cLiteral2 : disjunctions.getLiterals(-literal2)) {
                Clause clause2 = cLiteral2.clause;
                if(clause1.isSubset(cLiteral1,clause2)) {
                    toBeRemoved.add(cLiteral2);
                    if(size1 == clause2.size()) {toBeRemoved.add(clause1);}
                    continue;}
                if(clause2.isSubset(cLiteral2,clause1)) {
                    toBeRemoved.add(cLiteral1);
                    if(size1 == clause2.size()) {toBeRemoved.add(clause2);}}}}
        for(Object object : toBeRemoved) {
            if(object instanceof  Clause) {disjunctions.removeClause((Clause)object);}
            else {disjunctions.removeLiteral((CLiteral)object);}}
    }

    public void replaceByRepresentative(int representative, int literal) {
        disjunctions.replaceByRepresentative(representative,literal);}

    public ArrayList<Integer> pureLiterals() {
        return disjunctions.pureLiterals();}

    /** calls all trueLiteralObservers
     *
     * @param literal a true literal
     */
    private void reportTrueLiteral(int literal) {
        for(Consumer<Integer> observer : trueLiteralObservers) {observer.accept(literal);}}

    /** calls all unsatisfiabilityObservers with an Unsatisfiable object.
     *
     * @param model a model
     * @param basicClause a clause
     */
    private void reportUnsatisfiable(Model model, int[] basicClause) {
        for(Consumer<Unsatisfiable> observer : unsatisfiabilityObservers) {
            observer.accept(new Unsatisfiable("All literals in the clause " + Arrays.toString(basicClause) +
                    " are false in the model " + model.toString()));}}


}
