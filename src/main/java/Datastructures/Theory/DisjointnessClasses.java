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
    private ImplicationDAG implicationDAG; // an implication graph
    private EquivalenceClasses equivalences = null; // the equivalence classes
    /** the list of disjunctions representing disjoint literals */
    public ClauseList disjointnessClasses = null;
    /** reports changed disjointness classes */
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
     */
    public DisjointnessClasses(Model model, ImplicationDAG implicationDAG, EquivalenceClasses equivalences) {
        this.model = model;
        this.implicationDAG = implicationDAG;
        this.predicates = model.predicates;
        this.equivalences = equivalences;
        if(implicationDAG != null) {
            implicationDAG.addImplicationObserver((from,to) -> checkDisjointness(from));}
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
        String id = "D"+basicClause[0];
        Clause disjointness = new Clause(id,basicClause.length-2);
        int trueLiteral = 0;
        for(int i = 2; i < basicClause.length; ++i) {
            int literal = equivalences.mapToRepresentative(basicClause[i]);
            if(model.isTrue(literal)) {
                if(trueLiteral != 0) {reportUnsatisfiable(trueLiteral,literal); return null;} // two true literals are not disjoint
                else {trueLiteral = literal;}
                for(int j = 2; j < basicClause.length; ++j) {
                    if(i != j) {reportTrueLiteral(-basicClause[j]);}} // p true causes all other literals to become false.
                return null;}

            if(model.isFalse(literal)) {continue;}  // false literals can be ignored
            if(disjointness.contains(literal) >= 0) {reportUnsatisfiable(literal,literal); return null;} // p disjoint p is false
            if(disjointness.contains(-literal) >= 0) {continue;}  // p disjoint -p is trivially true
            disjointness.addCLiteralDirectly(new CLiteral(literal));}
        if(disjointness.size() > 2) {return insertClause(disjointness);}
        return null;}

    /** inserts a new clause into the disjointness list.
     * If the clause can be integrated into existing ones, it is done
     *
     * @param disjointness a new clause
     * @return either the clause itself or another one into which it is inserted.
     */
    private Clause insertClause(Clause disjointness) {
        initialize();
        disjointnessClasses.addClause(disjointness);
        subsume(disjointness);
        disjointness = joinClauses(disjointness);
        reportDisjointenss(disjointness);
        return disjointness;}


    /** This method is called when a new implication is inserted in the implicationDAG.
     * It checks if the implication causes the disjointness of several literals.
     *
     * @param literalNode a literal node int the implication DAG
     */
    public void checkDisjointness(ImplicationNode literalNode) {
        ArrayList<ImplicationNode> subnodes = literalNode.downNodes;
        int size = subnodes.size();
        if(size < 2) {return;}
        if(size < 32) checkDisjointnessInt(literalNode.literal,subnodes);
        if(size < 64) checkDisjointnessLong(literalNode.literal,subnodes);
        else {
            ArrayList<ImplicationNode> subs = new ArrayList<>();
            for(int i = 0; i < 64; ++i) {subs.add(subnodes.get(i));}
            checkDisjointnessLong(literalNode.literal,subs);}}

    /** It checks if the implication causes the disjointness of less than 32 literals.
     * Example:
     *    p     q     r
     *    \     /\    /
     *      -r     -p
     * @param literal  a literal which got new subnodes in the implication dag
     * @param subnodes its subnodes
     */
    private void checkDisjointnessInt(int literal, ArrayList<ImplicationNode> subnodes) {
        int size = subnodes.size();
        int[] list = new int[size];
        for(int i = 0; i < size; ++i) {
            ImplicationNode node = subnodes.get(i);
            int mask = 1 << i;
            for(ImplicationNode supernode :  node.upNodes) {
                int position = getPosition(supernode.literal,subnodes);
                if(position >= 0) {mask |= 1 << position;}}
            list[i] =  mask;}
        for(int bitmap : Utilities.largestSubsetsInt(size,
                (mask->!Utilities.forSome(mask, j -> (mask & list[j]) != mask)))) {
             addDisjointnessClass(bitmap,literal,subnodes);}}

    /** It checks if the implication causes the disjointness of less than 64 literals.
     * Example:
     *    p     q     r
     *    \     /\    /
     *      -r     -p
     * @param literal  a literal which got new subnodes in the implication dag
     * @param subnodes its subnodes
     */
    private void checkDisjointnessLong(int literal, ArrayList<ImplicationNode> subnodes) {
        int size = subnodes.size();
        long[] list = new long[size];
        for(int i = 0; i < size; ++i) {
            ImplicationNode node = subnodes.get(i);
            long mask = 1 << i;
            for(ImplicationNode supernode :  node.upNodes) {
                int position = getPosition(supernode.literal,subnodes);
                if(position >= 0) { mask |= ((long)1) << position;}}
            list[i] =  mask;}
        for(long bitmap : Utilities.largestSubsetsLong(size,
                (mask->!Utilities.forSome(mask, j -> (mask & list[j]) != mask)))) {
            addDisjointnessClass(bitmap,literal,subnodes);}}

    /** searches -literal in the nodes and returns its clausePosition
     *
     * @param literal  a literal
     * @param nodes a list of ImplicationNodes
     * @return the clausePosition of -literal in the nodes, or -1
     */
    private int getPosition(int literal, ArrayList<ImplicationNode> nodes) {
        for(int i = 0; i < nodes.size(); ++i) {
            if(-literal == nodes.get(i).literal) {return i;}}
        return -1;}

    /** adds a new disjointness class determined by the literal and the negated literals in the nodes,
     *  which are indicated by the 1's in the bitmap
     *
     * @param bitmap    a bitmap
     * @param literal  a literal
     * @param nodes a list of ImplicationNodes
     */
    private void addDisjointnessClass(int bitmap, int literal, ArrayList<ImplicationNode> nodes) {
        ArrayList<CLiteral<Clause>> literals = new ArrayList<>();
        StringBuilder st = new StringBuilder();
        st.append("D"+literal);
        literals.add(new CLiteral(literal));
        Utilities.forSome(bitmap,i->{
            int lit = -nodes.get(i).literal;
            st.append(""+lit);
            literals.add(new CLiteral(lit));
            return false;});
        Clause disjointness = new Clause(st.toString(),literals);
        insertClause(disjointness);
    }

    /** adds a new disjointness class determined by the literal and the negated literals in the nodes,
     *  which are indicated by the 1's in the bitmap
     *
     * @param bitmap    a bitmap
     * @param literal  a literal
     * @param nodes a list of ImplicationNodes
     */
    private void addDisjointnessClass(long bitmap, int literal, ArrayList<ImplicationNode> nodes) {
        ArrayList<CLiteral<Clause>> literals = new ArrayList<>();
        StringBuilder st = new StringBuilder();
        st.append("D"+literal);
        literals.add(new CLiteral(literal));
        Utilities.forSome(bitmap,i->{
            int lit = -nodes.get(i).literal;
            st.append(""+lit);
            literals.add(new CLiteral(lit));
            return false;});
        Clause disjointness = new Clause(st.toString(),literals);
        insertClause(disjointness);
    }


    public static void main(String[] args) {

    }



    /** tries to join two disjoint literals to existing disjointness classes.
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return true if a class has been found.
     */
    private boolean addToExisting(int literal1,int literal2) {
        boolean found = false;
        for(CLiteral<Clause> cLiteral : disjointnessClasses.getLiterals(literal1)) {
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
        for(CLiteral<Clause> cLiteral : disjointness) {
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
        for(CLiteral<Clause> cLiteral : disjointness.cliterals){
            for(Object otherCLiteral : disjointnessClasses.getLiterals(cLiteral.literal).toArray()) {
                Clause otherClause = ((CLiteral<Clause>)otherCLiteral).clause;
                if(otherClause == disjointness || otherClause.size() > size) {continue;}
                if(otherClause.timestamp < timestamp) {otherClause.timestamp = timestamp; continue;}
                else {++otherClause.timestamp;}
                if(otherClause.timestamp - timestamp == otherClause.size()-1) {disjointnessClasses.removeClause(otherClause);}}}
            }

    /** joins disjointness classes if possible.
     * It collects all literals in all clauses containing any of the literals in the given clause.
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

    /** collects all literals in all clauses containing one of the literals in the given clause.
     *
     * @param clause a disjointness clause
     * @return all literals in all disjunctions containing one of the literals in the given clause.
     */
    private Object[] literalUnion(Clause clause) {
        HashSet<Integer> literals = null;
        for(CLiteral<Clause> cLiteral : clause) {
            for(CLiteral<Clause> cLiteral1 : disjointnessClasses.getLiterals(cLiteral.literal)) {
                if(cLiteral1.clause != clause) {
                    for(CLiteral<Clause> cLiteral2 : cLiteral1.clause) {
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
        for(CLiteral<Clause> cLiteral : disjointnessClasses.getLiterals(literal1)) {
            if(cLiteral.clause.contains(literal2) >= 0) {return true;}}
        return false;}

    /** removes a true literal from the disjointness classes.
     * Classes with positive occurrences cause the other literals to become false.
     * In classes with negative occurrences, the literal is just removed.
     *
     * @param literal the true literal.
     */
    public void newTrueLiteral(int literal) {
        if(disjointnessClasses == null) {return;}
        Collection<CLiteral<Clause>>  lits = disjointnessClasses.getLiterals(literal);
        if(lits != null){
            for(Object clitObject : lits.toArray()) {
                CLiteral<Clause> clit = (CLiteral)clitObject;
                for(CLiteral clit1 : clit.clause.cliterals) {
                    if(clit != clit1) {reportTrueLiteral(-clit1.literal);}}
                disjointnessClasses.removeClause(clit.clause);}}
        lits = disjointnessClasses.getLiterals(-literal);
        if(lits != null){
            for(Object clitObject : lits.toArray()) {
                CLiteral<Clause> clit = (CLiteral)clitObject;
                if(clit.clause.size() == 2) {disjointnessClasses.removeClause(clit.clause);}
                else {disjointnessClasses.removeLiteral(clit);}}}
    }

    public void replaceByRepresentative(int representative, int literal) {
        if(disjointnessClasses == null) {return;}
        for(int i = 1; i >= -1; i -= 2) {
            literal *= i;
            representative *= i;
            for(Object clit : disjointnessClasses.literalIndex.getLiterals(literal).toArray()) {
                CLiteral<Clause> cliteral = (CLiteral)clit;
                Clause clause = cliteral.clause;
                if(clause.contains(-representative) >= 0) {
                    if(clause.size() == 2) {disjointnessClasses.removeClause(clause);}
                    else{disjointnessClasses.removeLiteral(cliteral);}
                    continue;} // p & -p are disjoint
                if(clause.contains(representative) >= 0)  {
                    reportUnsatisfiable(literal,representative); return;} // p & p cannot be disjoint.
                disjointnessClasses.literalIndex.removeLiteral(cliteral);
                cliteral.literal = representative;
                disjointnessClasses.literalIndex.addLiteral(cliteral);}}}

    /** returns true if there are no disjointness classes
     *
     * @return true if there are no disjointness classes
     */
    public boolean isEmpty() {return disjointnessClasses == null || disjointnessClasses.isEmpty();}


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
        if(disjointnessClasses == null) {return "";}
        return "Disjointenss Classes:\n" + disjointnessClasses.toString(symboltable);}
    }




