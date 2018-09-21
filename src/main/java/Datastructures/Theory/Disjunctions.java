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
    private ImplicationGraph implicationGraph = null;
    private EquivalenceClasses equivalences = null;/** reports true literals */
    public ArrayList<Consumer<Integer>> trueLiteralObservers = new ArrayList();
    public ArrayList<Consumer<Unsatisfiable>> unsatisfiabilityObservers = new ArrayList();

    public Disjunctions(int size, Model model, ImplicationGraph implicationGraph, EquivalenceClasses equivalences) {
        predicates = model.predicates;
        this.model = model;
        this.implicationGraph = implicationGraph;
        this.equivalences = equivalences;
        disjunctions = new ClauseList(size,predicates);
    }

    public void addLiteralRemovalObservers(Consumer<Clause> observer) {
        disjunctions.literalRemovalObservers.add(observer);}

    public void addLiteralReplacementObserver(Consumer<CLiteral> observer) {
        disjunctions.literalReplacementObservers.add(observer);}

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
                case 2: implicationGraph.addClause(clause.getLiteral(0),clause.getLiteral(1));
                    return;}
            if(i == 0) {clause = Algorithms.subsumedAndResolved(clause,disjunctions,implicationGraph);}}
        disjunctions.addClause(clause);}

    /** turns a basicORClause into a clause. <br/>
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
            TreeSet implied = implicationGraph.getImplicants(cLiteral1.literal);
            if(!implied.isEmpty()) {
                for(CLiteral cLiteral2 : clause.cliterals) {
                    if(cLiteral1 != cLiteral2 && implied.contains(cLiteral2.literal)) {
                        clause.removeLiteral(cLiteral1);
                        --i;
                        break;}}}}
        return clause;}


    public void makeTrue(int literal) {
        disjunctions.makeTrue(literal);}

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
