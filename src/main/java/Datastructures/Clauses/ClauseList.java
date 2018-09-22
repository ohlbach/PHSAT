package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Symboltable;
import Datastructures.Theory.ImplicationGraph;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.function.Consumer;
import java.util.stream.Stream;

/**
 * Created by ohlbach on 26.08.2018.
 *
 * This class is for collecting sets of clauses.
 * It supports inserting clauses, removing clauses and literals, retrieving clauses and literals.
 * A literal index is used to access literals and clauses quickly.
 * Removing literals and replacing them with representatives in an equivalence class can be
 * observed by corresponding consumer functions.
 */
public class ClauseList {
    public int predicates;
    public final ArrayList<Clause> clauses;              // the list of clauses
    private final HashMap<String,Clause> id2Clause;      // maps clause ids to clauses
    public final LiteralIndex literalIndex;              // maps literals to CLiterals
    public int timestamp = 0;                            // for algorithms
    public final ArrayList<Consumer<Clause>> literalRemovalObservers = new ArrayList<Consumer<Clause>>();
            // they are called when literals are removed.
    public final ArrayList<Consumer<CLiteral>> literalReplacementObservers = new ArrayList<Consumer<CLiteral>>();
    // they are called when literals are replaced by representatives in a equivalence class.


    /** creates a clause list. The number of clauses should be estimated.
     *
     * @param size       the estimated number of clauses
     * @pramm predicates the number of predicates.
     */
    public ClauseList(int size,int predicates) {
        this.predicates = predicates;
        clauses         = new ArrayList<Clause>(size);
        id2Clause       = new HashMap<>();
        literalIndex    = new LiteralIndex(predicates);}

    /** adds a clause to the list and updates the literal index
     *
     * @param clause to be added
     */
    public void addClause(Clause clause) {
        clauses.add(clause);
        id2Clause.put(clause.id,clause);
        for(CLiteral literal : clause.cliterals) {literalIndex.addLiteral(literal);}}

    /** returns a clause for the given number
     *
     * @param id the clause's id
     */
    public Clause getClause(String id) {return id2Clause.get(id);}

    /** returns the collection of literals with the given literal
     *
     * @param literal a literal
     * @return a (possibly empty) collection of literals with the given literal.
     */
    public LinkedList<CLiteral> getLiterals(int literal) {
        return literalIndex.getLiterals(literal);}

    /** removes a clause (silently). The clause itself is not changed.
     *
     * @param clause the clause to be removed
     */
    public void removeClause(Clause clause) {
        clauses.remove(clause);
        id2Clause.remove(clause.id);
        for(CLiteral cliteral : clause.cliterals) {literalIndex.removeLiteral(cliteral);}
    }

    /** removes a literal from the clause.<br/>
     * All literalRemovalObservers are called.
     *
     * @param cliteral the literal to be removed.
     */
    public void removeLiteral(CLiteral cliteral) {
        Clause clause = cliteral.clause;
        clause.removeLiteral(cliteral);
        literalIndex.removeLiteral(cliteral);
        for(Consumer observer : literalRemovalObservers) {observer.accept(clause);}}

    /** All clauses with the literal are removed. <br/>
     *  All negated literals are removed from the clauses. <br/>
     *  All literalRemovalObservers are called.
     *
     * @param literal a literal which is supposed to be true.
     */
    public void makeTrue(int literal) { //we must avoid concurrent modification errors!
        for(Object cliteral : literalIndex.getLiterals(literal).toArray()) {removeClause(((CLiteral)cliteral).clause);}
        for(Object cliteral : literalIndex.getLiterals(-literal).toArray()) {removeLiteral((CLiteral)cliteral);}}

    /** replaces a literal by its representative of an equivalence class in all clauses where ther literal and its negation occur.
     * If the clause then contains double literals, one of the is removed.<br/>
     * If the clause becomes a tautology, it is entirely (and silently) removed.<br/>
     * All literalReplacementObservers and literalRemovalObservers are called.
     *
     * @param literal       the literal to be replaced
     * @param representative the new literal
     */
    public void replaceByRepresentative(int representative, int literal) {
        for(int i = 1; i >= -1; i -= 2) {
            literal *= i;
            representative *= i;
            for(Object clit : literalIndex.getLiterals(literal).toArray()) {
                CLiteral cliteral = (CLiteral)clit;
                if(cliteral.clause.replaceBy(cliteral,representative)) { //replaced
                    literalIndex.removeLiteral(literal,cliteral);
                    literalIndex.addLiteral(cliteral);
                    if(cliteral.clause.contains(-representative) >= 0) {removeClause(cliteral.clause);}
                    else {for(Consumer observer : literalReplacementObservers) {observer.accept(cliteral);}}}
                else {literalIndex.removeLiteral(cliteral);  //removed
                    for(Consumer observer : literalRemovalObservers) {observer.accept(cliteral.clause);}}}}}

    /** gets the number of cLiterals containing the given literal
     *
     * @param literal a literal
     * @return the number of cLiterals containing the given literal
     */
    public int getOccurrences(int literal) {
        Collection list = literalIndex.getLiterals(literal);
        return(list == null) ? 0 : list.size();}

    /** checks whether there are no clauses with negated literals any more
     *
     * @param literal a literal
     * @return true if there are no negated literals anymore.
     */
    public boolean isPure(int literal) {
        Collection list = literalIndex.getLiterals(-literal);
        return(list == null) ? true : list.isEmpty();}

    /** generates a stream of CLiterals which are implied by the given literal
     *
     * @param literal           a literal
     * @param implicationGraph  the implication graph
     * @return  a stream of CLiterals l such that literal implies l (including the literal itself.)
     */
    public Stream<CLiteral> literalImplies(int literal, ImplicationGraph implicationGraph) {
        return  Stream.concat(
                literalIndex.getLiterals(literal).stream(),
                implicationGraph.getImplicants(literal).stream().flatMap(lit -> literalIndex.getLiterals(lit).stream()));}

    /** generates a stream of CLiterals which contradict the given literal<br/>
     * Example:  literal = p and p -> q: all CLiterals with -p and -q are returned.
     *
     * @param literal           a literal
     * @param implicationGraph  the implication graph
     * @return  a stream of CLiterals l such that literal implies l (including the literal itself.)
     */
    public Stream<CLiteral> literalContradict(int literal, ImplicationGraph implicationGraph) {
        return  Stream.concat(
                literalIndex.getLiterals(-literal).stream(),
                implicationGraph.getImplicants(literal).stream().flatMap(lit -> literalIndex.getLiterals(-lit).stream()));}


    /** generates a stream of CLiterals which  imply the given literal
     *
     * @param literal           a literal
     * @param implicationGraph  the implication graph
     * @return  a stream of CLiterals l such that l implies literal (including the literal itself.)
     */
    public Stream<CLiteral> literalIsImplied(int literal, ImplicationGraph implicationGraph) {
        return  Stream.concat(
                literalIndex.getLiterals(literal).stream(),
                implicationGraph.getImplicants(-literal).stream().flatMap(lit -> literalIndex.getLiterals(-lit).stream()));}


    /** the actual number of clauses
     *
     * @return the number of clauses
     */
    public int size() {return clauses.size();}

    /** checks if the clause set is empty
     *
     * @return true if the clause set is empty.
     */
    public boolean isEmpty() {return clauses.size() == 0;}

    /** generates a string with clauses
     *
     * @return a string with clauses
     */
    public String toString(){return toString(null);}

    /** generates a string with clauses
     *
     * @param symboltable for displaying the literals
     * @return a string with clauses
     */
    public String toString(Symboltable symboltable){
        StringBuffer st = new StringBuffer();
        int idlength = 0;
        for(Clause clause : clauses) {idlength = Math.max(idlength,clause.id.length());}
        for(Clause clause : clauses) {
            st.append(clause.toString(idlength,symboltable)).append("\n");}
        return st.toString();
    }





}
