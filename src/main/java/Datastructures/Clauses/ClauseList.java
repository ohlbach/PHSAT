package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Symboltable;
import Datastructures.Theory.ImplicationDAG;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.stream.Stream;

/**
 * Created by ohlbach on 26.08.2018.
 *
 * This class is for collecting sets of disjunctions.
 * It supports inserting disjunctions, removing disjunctions and literals, retrieving disjunctions and literals.
 * A literal index is used to access literals and disjunctions quickly.
 * Removing literals and replacing them with representatives in an equivalence class can be
 * observed by corresponding consumer functions.
 */
public class ClauseList {
    public int predicates;
    public final ArrayList<Clause> clauses;              // the list of disjunctions
    private final HashMap<String,Clause> id2Clause;      // maps clause ids to disjunctions
    public final LiteralIndex literalIndex;              // maps literals to CLiterals
    public int timestamp = 0;                            // for algorithms
    private final ArrayList<Consumer<CLiteral>> literalRemovalObservers = new ArrayList<>();
    // they are called when literals are removed.
    private final ArrayList<BiConsumer<CLiteral,Boolean>> literalReplacementObservers = new ArrayList<>();
    // they are called when literals are replaced by representatives in a equivalence class.
    private final ArrayList<Consumer<Clause>> clauseRemovalObservers = new ArrayList<>();
    // they are called when clauses are removed.


    /** creates a clause list. The number of disjunctions should be estimated.
     *
     * @param size       the estimated number of disjunctions
     * @pramm predicates the number of predicates.
     */
    public ClauseList(int size,int predicates) {
        this.predicates = predicates;
        clauses         = new ArrayList<Clause>(size);
        id2Clause       = new HashMap<>();
        literalIndex    = new LiteralIndex(predicates);}

    /** clones the entire clause list, except the observers.
     *
     * @return a clone of the clause list (without observers)
     */
    public ClauseList clone() {
        ClauseList newClauseList = new ClauseList(clauses.size(),predicates);
        for(Clause clause : clauses) {
            Clause newClause = clause.clone();
            newClauseList.clauses.add(clause);
            for(CLiteral cLiteral : newClause.cliterals) {
                newClauseList.literalIndex.addLiteral(cLiteral);}
            newClauseList.id2Clause.put(clause.id,newClause);}
        return newClauseList;}


    /** adds a purity observer
     *
     * @param observer a purity observer
     */
    public void addPurityObserver(Consumer<Integer> observer) {
        literalIndex.purityObservers.add(observer);}

    /** adds an observer which is called when a literal is removed from a clause
     *
     * @param observer a consumer function to be applied to a CLiteral
     */
    public void addLiteralRemovalObserver(Consumer<CLiteral> observer) {
        literalRemovalObservers.add(observer);}

    /** adds an observer which is called after a literal is replaced by another one (a representative in an equivalence class).
     * It is called for the new CLiteral and the old literal (Integer).
     *
     * @param observer a consumer function to be applied to a CLiteral
     */
    public void addLiteralReplacementObserver(BiConsumer<CLiteral,Boolean> observer) {
        literalReplacementObservers.add(observer);}


    /** adds an observer which is called when a clause is removed
     *
     * @param observer a consumer function to be applied to a removed clause
     */
    public void addClauseRemovalObserver(Consumer<Clause> observer) {
        clauseRemovalObservers.add(observer);}

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

    /** removes a clause and calls the clauseRemovalObservers. The clause itself is not changed.
     *
     * @param clause the clause to be removed
     */
    public void removeClause(Clause clause) {
        clauses.remove(clause);
        id2Clause.remove(clause.id);
        for(CLiteral cliteral : clause.cliterals) {literalIndex.removeLiteral(cliteral);}
        for(Consumer<Clause> observer : clauseRemovalObservers) {observer.accept(clause);}
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
        for(Consumer observer : literalRemovalObservers) {observer.accept(cliteral);}}

    /** removes all clauses with the given (pure) literal
     *
     * @param literal a pure literal
     */
    public void removeLiteral(int literal) {
        for(Object cLiteral : literalIndex.getLiterals(literal).toArray()) {
            removeClause(((CLiteral)cLiteral).clause);}}

    /** All disjunctions with the literal are removed. <br/>
     *  All negated literals are removed from the disjunctions. <br/>
     *  All literalRemovalObservers are called.
     *
     * @param literal a literal which is supposed to be true.
     */
    public void makeTrue(int literal) { //we must avoid concurrent modification errors!
        for(Object cliteral : literalIndex.getLiterals(literal).toArray()) {removeClause(((CLiteral)cliteral).clause);}
        for(Object cliteral : literalIndex.getLiterals(-literal).toArray()) {removeLiteral((CLiteral)cliteral);}}

    /** replaces a literal by its representative in an equivalence class in all clauses containing the literal and its negation.
     * If the clause then contains double literals, one of the is removed.<br/>
     * If the clause becomes a tautology, it is entirely removed.<br/>
     * The corresponding observers are called.<br/>
     * LiteralReplacementObservers are called before the replacement (with parameter true) <br/>
     * and after the replacement (with parameter false)
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
                Clause clause = cliteral.clause;
                if(clause.contains(-representative) >= 0) {removeClause(clause); continue;}    // tautology
                if(clause.contains(representative) >= 0)  {removeLiteral(cliteral); continue;} // double literals
                for(BiConsumer observer : literalReplacementObservers) {observer.accept(cliteral, true);}
                literalIndex.removeLiteral(cliteral);
                cliteral.literal = representative;
                literalIndex.addLiteral(cliteral);
                for(BiConsumer observer : literalReplacementObservers) {observer.accept(cliteral, false);}}}}

    /** gets the number of cLiterals containing the given literal
     *
     * @param literal a literal
     * @return the number of cLiterals containing the given literal
     */
    public int getOccurrences(int literal) {
        Collection list = literalIndex.getLiterals(literal);
        return(list == null) ? 0 : list.size();}

    /** checks whether there are no disjunctions with negated literals any more
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
     * @param implicationDAG  the implication DAG
     * @return  a stream of CLiterals l such that literal implies l (including the literal itself.)
     */
    public Stream<CLiteral> stream(int literal, ImplicationDAG implicationDAG, boolean down) {
        Stream<CLiteral>[] stream = new Stream[]{null};
        implicationDAG.apply(literal,down, (lit -> {
                if(stream[0] == null) {stream[0] =  literalIndex.getLiterals(lit).stream();}
                else {stream[0] = Stream.concat(stream[0], literalIndex.getLiterals(lit).stream());}}));
        return stream[0];}


    /** generates a stream of CLiterals which contradict the given literal<br/>
     * Example:  literal = p and p -> q: all CLiterals with -p and -q are returned.
     *
     * @param literal           a literal
     * @param implicationDAG  the implication DAG
     * @return  a stream of CLiterals l such that literal implies l (including the literal itself.)
     */
    public Stream<CLiteral> streamContradicting(int literal, ImplicationDAG implicationDAG) {
        Stream<CLiteral>[] stream = new Stream[]{null};
        implicationDAG.apply(-literal,false, (lit -> {
            if(stream[0] == null) {stream[0] =  literalIndex.getLiterals(lit).stream();}
            else {stream[0] = Stream.concat(stream[0], literalIndex.getLiterals(lit).stream());}}));
        return stream[0];}




    /** applies a consumer to all CLiterals which are implied by the given literal (including the literal itself)
     *
     * @param literal        a literal
     * @param implicationDAG the implications
     * @param down           if true then the ImplicationDAG is followed downwards, otherwise upwards.
     * @param consumer       a function to be applied to a CLiteral
     */
    public void apply(int literal, ImplicationDAG implicationDAG, boolean down, Consumer<CLiteral> consumer) {
        implicationDAG.apply(literal,down, (lit -> {
            for(CLiteral cLiteral : literalIndex.getLiterals(lit)) {consumer.accept(cLiteral);}}));}


    /** applies a consumer to all CLiterals which contradict the given literal (including the -literal itself)
     *
     * @param literal        a literal
     * @param implicationDAG the implications
     * @param consumer       a function to be applied to a CLiteral
     */
    public void applyContradicting(int literal, ImplicationDAG implicationDAG, Consumer<CLiteral> consumer) {
        implicationDAG.apply(-literal,false, (lit -> {
            for(CLiteral cLiteral : literalIndex.getLiterals(lit)) {consumer.accept(cLiteral);}}));}


    /** the actual number of disjunctions
     *
     * @return the number of disjunctions
     */
    public int size() {return clauses.size();}

    /** checks if the clause set is empty
     *
     * @return true if the clause set is empty.
     */
    public boolean isEmpty() {return clauses.size() == 0;}


    /** returns all pure literals
     *
     * @return all pure literals.
     */
    public ArrayList<Integer> pureLiterals() {
        return literalIndex.pureLiterals();}

    /** generates a string with disjunctions
     *
     * @return a string with disjunctions
     */
    public String toString(){return toString(null);}

    /** generates a string with disjunctions
     *
     * @param symboltable for displaying the literals
     * @return a string with disjunctions
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
