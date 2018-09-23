package Datastructures.Theory;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/** This class manages equivalence classes. The equivalence classes may come from basic disjunctions,
 * or from implications p -&gt; q and q -&gt; p.
 *
 * Created by ohlbach on 20.09.2018.
 */
public class EquivalenceClasses {
    private int predicates;
    private Model model;
    private ImplicationGraph implicationGraph;
    private ClauseList equivalenceClasses = null;
    private HashMap<Integer,Integer> replacements = null;
    /** reports true literals */
    public ArrayList<Consumer<Integer>> trueLiteralObservers = new ArrayList();
    /** reports contradictions like p = -p */
    public ArrayList<Consumer<Unsatisfiable>> unsatisfiabilityObservers = new ArrayList();
    /** reports new equivalences */
    public ArrayList<BiConsumer<Integer,Integer>> equivalenceObservers = new ArrayList();

    /** generates a new instance.
     *
     * @param model    a model
     * @param implicationGraph an implication graph (or null)
     */
    public EquivalenceClasses(Model model, ImplicationGraph implicationGraph) {
        this.model = model;
        this.implicationGraph = implicationGraph;
        this.predicates = model.predicates;
        if(implicationGraph != null) {
            implicationGraph.implicationObservers.add((from,to) -> {
                if(implicationGraph.implies(to,from)) {addEquivalence("E"+from+"="+to, from,to);}});}
        };

    /** initialises the classes at first usage.*/
    private void initialize() {
        if(equivalenceClasses == null) {
            equivalenceClasses = new ClauseList(5,predicates);
            replacements = new HashMap<>();}
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
    public void addEquivalenceClass(int[] basicClause) {
        assert basicClause.length > 2;
        if(basicClause.length == 3) {return;}
        initialize();
        String id = "E"+basicClause[0];
        for(int i = 2; i < basicClause.length; ++i) {
            int literal = mapToRepresentative(basicClause[i]);
            if(model.isTrue(literal)) {
                for(int j = 2; j < basicClause.length; ++j) {
                    if(i != j) {reportTrueLiteral(mapToRepresentative(basicClause[j]));}}
                return;}

            if(model.isFalse(literal)) {
                for(int j = 2; j < basicClause.length; ++j) {
                    if(i != j) {reportTrueLiteral(-mapToRepresentative(basicClause[j]));}}
                return;}}

        int representative = basicClause[2];
        for(int i = 3; i < basicClause.length; ++i) {addEquivalence(id,representative,basicClause[i]);}}


    /** joins a new equivalence to the classes.
     * If one of the literals is already in an equivalence class, the other literal becomes also a  member of this class,
     * otherwise a new class is formed.
     *
     * @param id       for identifying a new class
     * @param literal1 a literal
     * @param literal2 a literal
     */
    public void addEquivalence(String id, int literal1, int literal2) {
        initialize();
        literal1 = mapToRepresentative(literal1);
        literal2 = mapToRepresentative(literal2);
        if(literal1 == literal2) {return;}
        if(literal1 == -literal2) {reportUnsatisfiable(literal1,literal2); return;}

        Clause eqClass = null;
        int representative = literal1;
        int literal = literal2;
        LinkedList<CLiteral> classes = equivalenceClasses.getLiterals(literal1);
        if(!classes.isEmpty()) {eqClass = classes.getFirst().clause;}
        else {
            classes = equivalenceClasses.getLiterals(literal2);
            if(!classes.isEmpty()) {
                representative = literal2; literal = literal1;
                eqClass = classes.getFirst().clause;}
            else {eqClass = new Clause(id,2);
                eqClass.addCLiteralDirectly(new CLiteral(representative));
                equivalenceClasses.addClause(eqClass);}}

        eqClass.addCLiteralDirectly(new CLiteral(literal));
        if(implicationGraph != null) {
            for(CLiteral cLiteral : eqClass.cliterals) {
                if(implicationGraph.implies(cLiteral.literal,-literal)) {makeClassFalse(eqClass); return;}}}
        addReplacement(literal,representative);
        for(BiConsumer<Integer,Integer> observer : equivalenceObservers) {observer.accept(representative,literal);}
    }


    /** updates the replacements hash table
     *
     * @param literal        any class memeber
     * @param representative its representative in the class
     */
    private void addReplacement(int literal, int representative) {
        replacements.put(literal,representative);
        replacements.put(-literal,-representative);
    }

    /** maps literals to their representative in the equivalence class.
     *
     * @param literal the literal
     * @return either the literal itself, or its representative.
     */
    public int mapToRepresentative(int literal) {
        Integer replaced = replacements.get(literal);
        return (replaced == null) ? literal : replaced;}

    /** all members of the class become false literals.
     * The trueLiteralObservers are called.
     *
     * @param eqClass an equivalence class
     */
    private void makeClassFalse(Clause eqClass) {
        for(CLiteral cLiteral: eqClass.cliterals) {
            reportTrueLiteral(-cLiteral.literal);
            replacements.remove(cLiteral.literal);
            replacements.remove(-cLiteral.literal);}
        equivalenceClasses.removeClause(eqClass);}

    /** calls all trueLiteralObservers
     *
     * @param literal a true literal
     */
    private void reportTrueLiteral(int literal) {
        for(Consumer<Integer> observer : trueLiteralObservers) {observer.accept(literal);}}

    /** calls all unsatisfiabilityObservers with an Unsatisfiable object.
     * This may be caused by an equivalence p = -p
     *
     * @param literal1 a literal
     * @param literal2 its negation.
     */
    private void reportUnsatisfiable(int literal1 , int literal2) {
        for(Consumer<Unsatisfiable> observer : unsatisfiabilityObservers) {
            observer.accept(new Unsatisfiable("Equivalence " + literal1 + " = " +literal2 + " is false."));}}

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
        return "Equivalence Classes:\n" + equivalenceClasses.toString(symboltable);
    }

}
