package Datastructures.Theory;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Utilities.Utilities;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/** A disjointness class is a set of literals which are pairwise contradictory.
 * Such a class may come from the input data, or be derived from ID_Implications.
 * Created by Ohlbach on 21.09.2018.
 */
public class DisjointnessClasses {
    private int predicates;    // number of predicates
    private Model model;       // a model
    private ImplicationDAG implicationDAG; // an implication graph (optional)
    private EquivalenceClasses equivalenceClasses; // optional
    /** the list of disjunctions representing disjoint literals */
    public ClauseList disjointnessClasses = null;
    /** reports changed disjointnss classes */
    private ArrayList<Consumer<Clause>> disjointnessObservers = new ArrayList();
    /** reports contradictions like p = -p */
    private ArrayList<Consumer<Unsatisfiable>> unsatisfiabilityObservers = new ArrayList();
    /** reports new true literals */
    private ArrayList<Consumer<Integer>> trueLiteralObservers = new ArrayList();


    /**  adds a true literal observer
     *
     * @param observer to be added*/
    public synchronized void addTrueLiteralObserver(Consumer<Integer> observer) {trueLiteralObservers.add(observer);}
    /** adds an unsatisfiability observer
     *
     * @param observer to be added*/
    public synchronized void addUnsatisfiabilityObserver(Consumer<Unsatisfiable> observer) {unsatisfiabilityObservers.add(observer);}
    /** adds an observer for disjointnesses.
     *
     * @param observer to be added*/
    public synchronized void addDisjointnessObserver(Consumer<Clause> observer) {disjointnessObservers.add(observer);}

    /** removes a true literal observer
     *
     * @param observer to be added*/
    public synchronized void removeTrueLiteralObserver(Consumer<Integer> observer) {trueLiteralObservers.remove(observer);}
    /** removes an unsatisfiability observer
     *
     * @param observer to be added*/
    public synchronized void removeUnsatisfiabilityObserver(Consumer<Unsatisfiable> observer) {unsatisfiabilityObservers.remove(observer);}
    /** removes an observer for disjointnesses.
     *
     * @param observer to be added*/
    public synchronized void removeDisjointnessObserver(Consumer<Clause> observer) {disjointnessObservers.remove(observer);}

    /** generates a new instance.
     *
     * @param model    a model
     * @param implicationDAG an implication graph (or null)
     * @param equivalenceClasses  for replacing literals by their representatives.
     */
    public DisjointnessClasses(Model model, ImplicationDAG implicationDAG, EquivalenceClasses equivalenceClasses) {
        this.model = model;
        this.implicationDAG = implicationDAG;
        this.equivalenceClasses = equivalenceClasses;
        this.predicates = model.predicates;
        if(implicationDAG != null) {
            implicationDAG.addImplicationObserver((from,to) -> addDisjointness(from,-to));}
    };

    /** initialises the classes at first usage.*/
    private void initialize() {
        if(disjointnessClasses == null) {
            disjointnessClasses = new ClauseList(5,predicates);}
    }


    /** turns a basicClause into a disjointness class. <br>
     * A true literal causes all other literals to become false <br>
     * A false literal is ignored <br>
     * Two true literals are a contradiction <br>
     * p &lt;=&gt; -p is ignored.<br>
     * A double literal p,p is a contradiction.<br>
     * The corresponding observers are called.
     * New disjunctions which are subsets of a new clause are deleted.
     * Literals occurring in several classes may cause joining of the classes.
     *
     * @param basicClause [clause-problemId,typenumber,literal1,...]
     * @return the result disjointness clause or null
     */
    public Clause addDisjointnessClass(int[] basicClause) {
        assert basicClause.length > 2;
        if(basicClause.length == 3) {return null;}
        initialize();
        String id = "D"+basicClause[0];
        Clause disjointness = new Clause(id,basicClause.length-2);
        int trueLiteral = 0;
        for(int i = 2; i < basicClause.length; ++i) {
            int literal = mapToRepresentative(basicClause[i]);
            if(model.isTrue(literal)) {
                if(trueLiteral != 0) {reportUnsatisfiable(trueLiteral,literal); return null;} // two true literals are not disjoint
                else {trueLiteral = literal;}
                for(int j = 2; j < basicClause.length; ++j) {
                    if(i != j) {reportTrueLiteral(-mapToRepresentative(basicClause[j]));}} // p true causes all other litrals to become false.
                return null;}

            if(model.isFalse(literal)) {continue;}  // false literals can be ignored
            if(disjointness.contains(literal) >= 0) {reportUnsatisfiable(literal,literal); return null;} // p disjoint p is false
            if(disjointness.contains(-literal) >= 0) {continue;}  // p disjoint -p is trivially true
            disjointness.addCLiteralDirectly(new CLiteral(literal));}
        if(disjointness.size() > 1) {
            disjointnessClasses.addClause(disjointness);
            subsume(disjointness);
            disjointness = joinClauses(disjointness);
            reportDisjointenss(disjointness);
            return disjointness;}
        return null;}



    /** joins a new disjointness p disjoint q.
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
        if(addToExisting(literal1,literal2) || addToExisting(literal2,literal1)) {return;}
        ArrayList<Integer> intersections = new ArrayList<>();
        Integer lit2 = literal2;
        implicationDAG.apply(literal1,true,(lit1->{if(implicationDAG.implies(lit2,lit1)){intersections.add(lit1);}}));
        if(intersections != null) {
            Clause disjointness = new Clause("D"+literal1+"!="+literal2,intersections.size()+2);
            disjointness.addCLiteralDirectly(new CLiteral(literal1));
            disjointness.addCLiteralDirectly(new CLiteral(literal2));
            for(int literal : intersections) {disjointness.addCLiteralDirectly(new CLiteral(-literal));}
            disjointnessClasses.addClause(disjointness);
            subsume(disjointness);
            reportDisjointenss(joinClauses(disjointness));}}

    /** tries to join two disjoint literals to existing disjointness classes.
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return true if a class has been found.
     */
    private boolean addToExisting(int literal1,int literal2) {
        boolean found = false;
        for(CLiteral cLiteral : disjointnessClasses.getLiterals(literal1)) {
            if(cLiteral.clause.contains(literal2) >= 0) {return true;} // both literals are already in a disjointness class
            found |= addToExisting(cLiteral.clause,literal2);}
        return found;}

    /** checks whether all literals in the disjointness class are disjoint with the new literal.
     *  If p is in the clause and r is the new literal, then p -&gt; -r must hold to ensure disjointness.<br>
     *  If the class is extended then it may subsume older disjunctions, and it may merge with older disjunctions.
     *
     * @param disjointness an existing disjointness class
     * @param literal  a literal
     * @return true if all literals in the class are´disjoint with the new literal.
     */
    private boolean addToExisting(Clause disjointness, int literal) {
        for(CLiteral cLiteral : disjointness.cliterals) {
            if(!implicationDAG.implies(cLiteral.literal,-literal)) {return false;}}
        disjointness.addCLiteralDirectly(new CLiteral(literal));
        subsume(disjointness);
        reportDisjointenss(joinClauses(disjointness));
        return true;}

    /** removes all disjunctions which are subsets of the given clause.
     * I p,q,r are disjoint then p,q and p,r and q,r are also disjoint.
     *
     * @param disjointness a disjointness clause.
     */
    private void subsume(Clause disjointness) {
        int size = disjointness.size();
        int timestamp = disjointnessClasses.timestamp;
        disjointnessClasses.timestamp += size+1;
        for(CLiteral cLiteral : disjointness.cliterals){
            for(Object otherCLiteral : disjointnessClasses.getLiterals(cLiteral.literal).toArray()) {
                Clause otherClause = ((CLiteral)otherCLiteral).clause;
                if(otherClause == disjointness || otherClause.size() > size) {continue;}
                if(otherClause.timestamp < timestamp) {otherClause.timestamp = timestamp; continue;}
                else {++otherClause.timestamp;}
                if(otherClause.timestamp - timestamp == otherClause.size()-1) {disjointnessClasses.removeClause(otherClause);}}}
            }

    /** joins disjointness classes if possible.
     * It collects all literals in all disjunctions containing any of the literals in the given clause.
     * Only those literals which are mutually disjoint are then inserted in a new clause
     *
     * @param disjointness a disjointness class
     * @return the old or a new disjointness class
     */
    private Clause joinClauses(Clause disjointness) {
        Object[] literals = literalUnion(disjointness);
        if(literals == null) {return disjointness;}
        int size = literals.length;
        if(size == disjointness.size()) {return disjointness;}
        Clause joinedClause = new Clause(disjointness.id+"j",size);
        for(int i = 0; i < size; ++i) {
            boolean disjoint = true;
            int literal = (Integer)literals[i];
            for(int j = i+1; j < size; ++j) {
                if(!areDisjoint(literal,(Integer)literals[j])) {disjoint = false; break;}}
            if(disjoint){joinedClause.addCLiteral(new CLiteral(literal));}}
        disjointnessClasses.addClause(joinedClause);
        subsume(joinedClause);
        return joinedClause;}

    /** collects all literals in all disjunctions containing one of the literals in the given clause.
     *
     * @param clause a disjointness clause
     * @return all literals in all disjunctions containing one of the literals in the given clause.
     */
    private Object[] literalUnion(Clause clause) {
        HashSet<Integer> literals = null;
        for(CLiteral cLiteral : clause.cliterals) {
            for(CLiteral cLiteral1 : disjointnessClasses.getLiterals(cLiteral.literal)) {
                if(cLiteral1.clause != clause) {
                    for(CLiteral cLiteral2 : cLiteral1.clause.cliterals) {
                        if(literals == null) {literals = new HashSet<>();}
                        literals.add(cLiteral2.literal);}}}}
        return (literals == null) ? null : literals.toArray();}


    /** checks if two literals are disjoint
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return true if the two literals are disjoint
     */
    public boolean areDisjoint(int literal1, int literal2) {
        if(literal1 == literal2) {return false;}
        if(literal1 == -literal2) {return true;}
        for(CLiteral cLiteral : disjointnessClasses.getLiterals(literal1)) {
            if(cLiteral.clause.contains(literal2) >= 0) {return true;}}
        return false;}

    /** returns true if there are no disjointness classes
     *
     * @return true if there are no disjointness classes
     */
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

    /** calls all disjointnessObservers
     *
     * @param clause a disjointness clause
     */
    private void reportDisjointenss(Clause clause) {
        for(Consumer<Clause> observer : disjointnessObservers) {observer.accept(clause);}}

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




