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
 * or from the implication graph.
 *
 * Created by ohlbach on 20.09.2018.
 */
public class EquivalenceClasses {
    private int predicates;
    private Model model;
    private ImplicationDAG implicationDAG;
    private ClauseList equivalenceClasses = null;
    private HashMap<Integer,Integer> replacements = new HashMap<>();

    /** reports true literals */
    private ArrayList<Consumer<Integer>> trueLiteralObservers = new ArrayList();
    /** reports contradictions like p = -p */
    private ArrayList<Consumer<Unsatisfiable>> unsatisfiabilityObservers = new ArrayList();
    /** reports new equivalences */
    private ArrayList<BiConsumer<Integer,Integer>> equivalenceObservers = new ArrayList();

    /** adds a true literal observer
     *
     * @param observer to be added*/
    public synchronized void addTrueLiteralObserver(Consumer<Integer> observer) {trueLiteralObservers.add(observer);}
    /** adds an unsatisfiability observer
     *
     * @param observer to be added*/
    public synchronized void addUnsatisfiabilityObserver(Consumer<Unsatisfiable> observer) {unsatisfiabilityObservers.add(observer);}
    /** adds an observer for equivalences.
     *
     * @param observer to be added*/
    public synchronized void addEquivalenceObserver(BiConsumer<Integer,Integer> observer) {equivalenceObservers.add(observer);}

    /** removes a true literal observer
     *
     * @param observer to be added*/
    public synchronized void removeTrueLiteralObserver(Consumer<Integer> observer) {trueLiteralObservers.remove(observer);}
    /** removes an unsatisfiability observer
     *
     * @param observer to be added*/
    public synchronized void removeUnsatisfiabilityObserver(Consumer<Unsatisfiable> observer) {unsatisfiabilityObservers.remove(observer);}
    /** removes an observer for equivalences.
     *
     * @param observer to be added*/
    public synchronized void removeEquivalenceObserver(BiConsumer<Integer,Integer> observer) {equivalenceObservers.remove(observer);}

    /** generates a new instance.
     *
     * @param model    a model
     * @param implicationDAG an implication graph (or null)
     */
    public EquivalenceClasses(Model model, ImplicationDAG implicationDAG) {
        this.model = model;
        this.implicationDAG = implicationDAG;
        this.predicates = model.predicates;}


    /** initialises the classes at first usage.*/
    private void initialize() {
        if(equivalenceClasses == null) {
            equivalenceClasses = new ClauseList(5,predicates);
            replacements = new HashMap<>();}
    }

    /** checks if there are equivalence classes
     *
     * @return true if there are no equivalence classes
     */
    public boolean isEmpty() {
        return equivalenceClasses == null || equivalenceClasses.isEmpty();}


    /** turns a basicClause into an equivalence class. <br>
     * A true literal causes all other literals to become true <br>
     * A false literal causes all other literals to become false <br>
     * p &lt;=&gt; -p is a contradiction.<br>
     * A double literal p,p is ignored.<br>
     * p,q,r and p -&gt; not r  causes all literals to become false.<br>
     * The corresponding observers are called.
     *
     * @param basicClause [clause-problemId,typenumber,literal1,...]
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


    /** joins a new equivalence to the classes.<br>
     * If one of the literals is already in an equivalence class, the other literal becomes also a  member of this class,
     * otherwise a new class is formed.
     *
     * @param id       for identifying a new class
     * @param literal1 a literal
     * @param literal2 a literal
     * @return an equivalence class or null
     */
    public Clause addEquivalence(String id, int literal1, int literal2) {
        initialize();
        literal1 = mapToRepresentative(literal1);
        literal2 = mapToRepresentative(literal2);
        if(literal1 == literal2) {return null;}
        if(literal1 == -literal2) {reportUnsatisfiable(literal1,literal2); return null;}

        Clause eqClass = null;
        int representative = literal1;
        int literal = literal2;
        PriorityQueue<CLiteral> classes = equivalenceClasses.getLiterals(literal1);
        if(!classes.isEmpty()) {eqClass = classes.peek().clause;}
        else {
            classes = equivalenceClasses.getLiterals(literal2);
            if(!classes.isEmpty()) {
                representative = literal2; literal = literal1;
                eqClass = classes.peek().clause;}
            else {eqClass = new Clause(id,2);
                eqClass.addCLiteralDirectly(new CLiteral(representative));
                equivalenceClasses.addClause(eqClass);}}

        eqClass.addCLiteralDirectly(new CLiteral(literal));
        if(implicationDAG != null) {
            for(CLiteral cLiteral : eqClass.cliterals) {
                if(implicationDAG.implies(cLiteral.literal,-literal)) {makeClassFalse(eqClass); return null;}}}
        addReplacement(literal,representative);
        for(BiConsumer<Integer,Integer> observer : equivalenceObservers) {observer.accept(representative,literal);}
        return eqClass;
    }

    /** This method is used to add an equivalence class which has been derived in the Implication DAG.<br>
     *  None of the literals is supposed to be true or false.
     *  One of the literals, however, may already be in another equivalence class.
     *
     * @param equivalents equivalent literals
     * @return a new equivalence clause.
     */
    public Clause addEquivalence(int[] equivalents) {
        int representative = equivalents[0];
        if(representative < 0) {
            representative *= -1;
            for(int i = 0; i < equivalents.length; ++i) {equivalents[i] *= -1;}}
        String id = "E"+representative;
        Clause eqClass = null;
        for(int i = 1; i < equivalents.length;++i) {
            eqClass = addEquivalence(id,representative,equivalents[i]);}
        return eqClass;}



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

    /** completes a model by the equivalence classes.
     *
     */
    public void completeModel() {
        if(equivalenceClasses == null) {return;}
        int status;
        for(Clause eqClass : equivalenceClasses.getClauses(0)) {
            boolean allUndefined = true;
            for(CLiteral clit : eqClass.cliterals) {
                if(model.status(clit.literal) != 0) {allUndefined = false; break;}}
            if(allUndefined) {
                for(CLiteral clit : eqClass.cliterals) {
                    model.add(clit.literal);}}
            else {
                for(CLiteral clit : eqClass.cliterals) {
                    status = model.status(clit.literal);
                    if(status != 0) {
                        for(CLiteral clit1 : eqClass.cliterals) {model.setStatus(clit1.literal,status);}}
                        break;}}}}


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
        StringBuilder st = new StringBuilder();
        if(equivalenceClasses != null) {
            st.append("Equivalence Classes:\n" + equivalenceClasses.toString(symboltable)).append("\n");}
        if(!replacements.isEmpty()) {
            st.append("Replacements:\n");
            for(Map.Entry entry : replacements.entrySet()) {
                Integer from = (Integer)entry.getKey();
                Integer to = (Integer)entry.getValue();
                if(symboltable != null) {
                    st.append(symboltable.getLiteralName(from)).append(" -> ").append(symboltable.getLiteralName(to)).append("\n");}
                else{st.append(from).append(" -> ").append(to).append("\n");}}}
        return st.toString();}

}
