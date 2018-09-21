package Datastructures.Theory;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Utilities.Utilities;

import java.util.ArrayList;
import java.util.function.Consumer;

/**
 * Created by Ohlbach on 21.09.2018.
 */
public class DisjointnessClasses {
    private int predicates;
    private Model model;
    private ImplicationGraph implicationGraph;
    private EquivalenceClasses equivalenceClasses;
    public ClauseList disjointnessClasses = null;
    /** reports true literals */
    public ArrayList<Consumer<Clause>> disjointenessObservers = new ArrayList();
    /** reports contradictions like p = -p */
    public ArrayList<Consumer<Unsatisfiable>> unsatisfiabilityObservers = new ArrayList();
    /** reports new equivalences */
    public ArrayList<Consumer<Integer>> trueLiteralObservers = new ArrayList();

    /** generates a new instance.
     *
     * @param model    a model
     * @param implicationGraph an implication graph (or null)
     */
    public DisjointnessClasses(Model model, ImplicationGraph implicationGraph, EquivalenceClasses equivalenceClasses) {
        this.model = model;
        this.implicationGraph = implicationGraph;
        this.equivalenceClasses = equivalenceClasses;
        this.predicates = model.predicates;
        if(implicationGraph != null) {
            implicationGraph.implicationObservers.add((from,to) -> addDisjointness(from,-to));}
    };

    /** initialises the classes at first usage.*/
    private void initialize() {
        if(disjointnessClasses == null) {
            disjointnessClasses = new ClauseList(5,predicates);}
    }


    /** turns a basicClause into an equivalence class. <br/>
     * A true literal causes all other literals to become true <br/>
     * A false literal causes all other literals to become false <br/>
     * p &lt;=&gt; -p is a contradiction.<br/>
     * A double literal p,p is ignored.<br/>
     * p,q,r and p -&gt; not r  causes all literals to become false.<br/>
     * The corresponding observers are called.
     *
     * @param basicClause [clause-id,typenumber,literal1,...]
     */
    public void addDisjointnessClass(int[] basicClause) {
        assert basicClause.length > 2;
        if(basicClause.length == 3) {return;}
        initialize();
        String id = "D"+basicClause[0];
        Clause disjointeness = new Clause(id,basicClause.length-2);
        int trueLiteral = 0;
        for(int i = 2; i < basicClause.length; ++i) {
            int literal = mapToRepresentative(basicClause[i]);
            if(model.isTrue(literal)) {
                if(trueLiteral != 0) {reportUnsatisfiable(trueLiteral,literal); return;}
                else {trueLiteral = literal;}
                for(int j = 2; j < basicClause.length; ++j) {
                    if(i != j) {reportTrueLiteral(-mapToRepresentative(basicClause[j]));}}
                return;}

            if(model.isFalse(literal)) {continue;}
            if(disjointeness.contains(literal) >= 0) {reportUnsatisfiable(literal,literal); return;}
            if(disjointeness.contains(-literal) >= 0) {continue;}
            disjointeness.addCLiteralDirectly(new CLiteral(literal));}
        if(disjointeness.size() > 1) {disjointnessClasses.addClause(disjointeness);}}



    /** joins a new equivalence to the classes.
     * If one of the literals is already in an equivalence class, the other literal becomes also a  member of this class,
     * otherwise a new class is formed.
     *
     * @param literal1 a literal
     * @param literal2 a literal
     */
    public void addDisjointness(int literal1, int literal2) {
        initialize();
        literal1 = mapToRepresentative(literal1);
        literal2 = mapToRepresentative(literal2);
        if(literal1 == -literal2) {return;}  // they are disjoint anyway
        if(literal1 == literal2) {reportUnsatisfiable(literal1,literal2); return;} // p cannot be disjoint to p
        if(addToExisting(literal1,literal2) | addToExisting(literal2,literal1)) {return;}
        ArrayList<Integer> intersections = Utilities.intersection(implicationGraph.getImplicants(literal1),implicationGraph.getImplicants(literal2));
        if(intersections != null) {
            Clause disjointness = new Clause("D"+literal1+"!="+literal2,intersections.size()+2);
            disjointness.addCLiteralDirectly(new CLiteral(literal1));
            disjointness.addCLiteralDirectly(new CLiteral(literal2));
            for(int literal : intersections) {disjointness.addCLiteralDirectly(new CLiteral(-literal));}
            disjointnessClasses.addClause(disjointness);
            reportDisjointenss(disjointness);}}

    private boolean addToExisting(int literal1,int literal2) {
        boolean found = false;
        for(CLiteral cLiteral : disjointnessClasses.getLiterals(literal1)) {
            if(cLiteral.clause.contains(literal2) >= 0) {return true;}
            found |= addToExisting(cLiteral.clause,literal2);}
        return found;}


    private boolean addToExisting(Clause disjointness, int literal) {
        for(CLiteral cLiteral : disjointness.cliterals) {
            if(!implicationGraph.implies(cLiteral.literal,-literal)) {return false;}}
        disjointness.addCLiteralDirectly(new CLiteral(literal));
        reportDisjointenss(disjointness);
        return true;}

    public boolean isEmpty() {return disjointnessClasses.isEmpty();}


    /** maps literals to their representative in the equivalence class.
     *
     * @param literal the literal
     * @return either the literal itself, or its representative.
     */
    public int mapToRepresentative(int literal) {
        if(equivalenceClasses == null) {return literal;}
        return equivalenceClasses.mapToRepresentative(literal);}

    /** calls all trueLiteralObservers
     *
     * @param literal a true literal
     */
    private void reportTrueLiteral(int literal) {
        for(Consumer<Integer> observer : trueLiteralObservers) {observer.accept(literal);}}

    /** calls all disjointenessObservers
     *
     * @param clause a disjointeness clause
     */
    private void reportDisjointenss(Clause clause) {
        for(Consumer<Clause> observer : disjointenessObservers) {observer.accept(clause);}}

    /** calls all unsatisfiabilityObservers with an Unsatisfiable object.
     * This may be caused by an equivalence p = -p
     *
     * @param literal1 a literal
     * @param literal2 its negation.
     */
    private void reportUnsatisfiable(int literal1 , int literal2) {
        for(Consumer<Unsatisfiable> observer : unsatisfiabilityObservers) {
            observer.accept(new Unsatisfiable("Disjointenss " + literal1 + " = " +literal2 + " is false."));}}

    /** lists all equivalence classes
     *
     * @return all equivalence classes as string
     */
    public String toString() {return toString(null);}

    /** lists all equivalence classes
     *
     * @param symboltable a symboltable
     * @return all equivalence classes as string
     */
    public String toString(Symboltable symboltable) {
        return "Disjointenss Classes:\n" + disjointnessClasses.toString(symboltable);}
    }




