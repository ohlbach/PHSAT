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
 * This class is for collecting sets of clauses in different groups.
 * It supports inserting clauses, removing clauses and literals, retrieving clauses and literals.
 * A literal index is used to access literals and clauses quickly.
 * Removing literals and replacing them with representatives in an equivalence class can be
 * observed by corresponding consumer functions.
 * <br/>
 * The clauses are sorted according to a comparator (default: clause length) <br/>
 * The literals in the literal index are also sorted (default: clause length)
 */
public class ClauseList {
    public int predicates;
    private final PriorityQueue<Clause>[] clauses;        // the list of clauses
    private final HashMap<String,Clause> id2Clause;      // maps clause ids to disjunctions
    public final LiteralIndex literalIndex;              // maps literals to CLiterals
    public int groups = 1;                               // the total number of clause groups
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
     * @param predicates the number of predicates.
     */
    public ClauseList(int size,int predicates) {
        this.predicates = predicates;
        clauses         = new PriorityQueue[]{new PriorityQueue<Clause>(size,Comparator.comparingInt(cl->cl.size()))};
        id2Clause       = new HashMap<>();
        literalIndex    = new LiteralIndex(predicates);}

    /** creates a clause list. The number of disjunctions should be estimated.
     *
     * @param comparator the list of comparators.
     * @param predicates the number of predicates.
     */
    public ClauseList(int predicates,Comparator<Clause>... comparator ) {
        this.predicates = predicates;
        id2Clause       = new HashMap<>();
        literalIndex    = new LiteralIndex(predicates);
        groups          = comparator.length;
        clauses = new PriorityQueue[groups];
        for(int i = 0; i < groups; ++i) {clauses[i] =  new PriorityQueue<Clause>(comparator[i]);}
    }


    /** returns the clauses of the given group
     *
     * @param group a clause group
     * @return the clauses of this group
     */
    public PriorityQueue<Clause> getClauses(int group) {return clauses[group];}


    /** adds a purity observer
     *
     * @param observer a purity observer
     */
    public void addPurityObserver(Consumer<Integer> observer) {
        literalIndex.purityObservers.add(observer);}

    /** removes a purity observer */
    public void removePurityObserver(Consumer<Integer> observer) {
        literalIndex.purityObservers.remove(observer);}


    /** adds an observer which is called when a literal is removed from a clause
     *
     * @param observer a consumer function to be applied to a CLiteral
     */
    public void addLiteralRemovalObserver(Consumer<CLiteral> observer) {
        literalRemovalObservers.add(observer);}

    /** removes a literal removal observer */
    public void removeLiteralRemovalObserver(Consumer<CLiteral> observer) {
        literalRemovalObservers.remove(observer);}


    /** adds an observer which is called after a literal is replaced by another one (a representative in an equivalence class).
     * It is called for the new CLiteral and the old literal (Integer).
     *
     * @param observer a consumer function to be applied to a CLiteral
     */
    public void addLiteralReplacementObserver(BiConsumer<CLiteral,Boolean> observer) {
        literalReplacementObservers.add(observer);}

    /** removes a literal replacement observer */
    public void removeLiteralReplacementObserver(BiConsumer<CLiteral,Boolean> observer) {
        literalReplacementObservers.remove(observer);}


    /** adds an observer which is called when a clause is removed
     *
     * @param observer a consumer function to be applied to a removed clause
     */
    public void addClauseRemovalObserver(Consumer<Clause> observer) {
        clauseRemovalObservers.add(observer);}

    /** removes a ClauseRemovalObserver*/
    public void removeClauseRemovalObserver(Consumer<Clause> observer) {
        clauseRemovalObservers.remove(observer);}


    /** adds a clause to the list and updates the literal index
     *
     * @param clause to be added
     */
    public void addClause(Clause clause) {
        clauses[0].add(clause);
        integrateClause(clause);}

    /** adds a clause to the list and updates the literal index
     *
     * @param clause to be added
     * @param group the clause group where the clause is to be added
     */
    public void addClause(Clause clause, int group) {
        clauses[group].add(clause);
        integrateClause(clause);}


    /** integrates the clause in the literal indices and the id-map.
     *
      * @param clause the clause to be integrated.
     */
    protected void integrateClause(Clause clause) {
        id2Clause.put(clause.id,clause);
        for(CLiteral literal : clause.cliterals) {literalIndex.addLiteral(literal);}}


    /** returns a clause for the given number
     *
     * @param id the clause's problemId
     */
    public Clause getClause(String id) {return id2Clause.get(id);}

    /** returns the collection of literals with the given literal
     *
     * @param literal a literal
     * @return a (possibly empty) collection of literals with the given literal.
     */
    public PriorityQueue<CLiteral> getLiterals(int literal) {
        return literalIndex.getLiterals(literal);}

    /** removes a clause and calls the clauseRemovalObservers. The clause itself is not changed.
     *
     * @param clause the clause to be removed
     */
    public void removeClause(Clause clause) {
        for(int group = 0; group < groups; ++ group) clauses[group].remove(clause);
        clause.removed = true;
        id2Clause.remove(clause.id);
        for(CLiteral cliteral : clause.cliterals) {literalIndex.removeLiteral(cliteral);}
        for(Consumer<Clause> observer : clauseRemovalObservers) {observer.accept(clause);}
    }

    /** removes a clause and calls the clauseRemovalObservers. The clause itself is not changed.
     *
     * @param clause the clause to be removed
     * @param group the clause group where the clause is
     */
    public void removeClause(Clause clause, int group) {
        clauses[group].remove(clause);
        clause.removed = true;
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
        int group = 0;
        if(groups > 1) {
            boolean found = false;
            for(; group < groups; ++group) {if(clauses[group].contains(clause)) {found = true; break;}}
            if(!found) {return;}}
        clauses[group].remove(clause);
        for(CLiteral clit : clause.cliterals) {literalIndex.removeLiteral(clit);}
        clause.removeLiteral(cliteral);
        clauses[group].add(clause);
        for(CLiteral clit : clause.cliterals) {literalIndex.addLiteral(clit);}
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
     * @param implicationDAG the ID_Implications
     * @param down           if true then the ImplicationDAG is followed downwards, otherwise upwards.
     * @param consumer       a function to be applied to a CLiteral
     */
    public void apply(int literal, ImplicationDAG implicationDAG, boolean down, Consumer<CLiteral> consumer) {
        implicationDAG.apply(literal,down, (lit -> {
            for(CLiteral cLiteral : literalIndex.getLiterals(lit)) {consumer.accept(cLiteral);}}));}


    /** applies a consumer to all CLiterals which contradict the given literal (including the -literal itself)
     *
     * @param literal        a literal
     * @param implicationDAG the ID_Implications
     * @param consumer       a function to be applied to a CLiteral
     */
    public void applyContradicting(int literal, ImplicationDAG implicationDAG, Consumer<CLiteral> consumer) {
        implicationDAG.apply(-literal,false, (lit -> {
            for(CLiteral cLiteral : literalIndex.getLiterals(lit)) {consumer.accept(cLiteral);}}));}


    /** the actual number of clauses
     *
     * @return the number of clauses
     */
    public int size() {
        int size = 0;
        for(int group = 0; group < groups; ++group) {size += clauses[group].size();}
        return size;}

    /** the number of clauses in the group
     *
     * @param group a clause group
     * @return the number of clauses in the group
     */
    public int size(int group) {
        return clauses[group].size();}

    /** checks if the clause set is empty
     *
     * @return true if the clause set is empty.
     */
    public boolean isEmpty() {
        for(int group = 0; group < groups; ++group) {
            if(clauses[group].size() > 0) {return false;}}
        return true;}


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
        StringBuilder st = new StringBuilder();
        int[] idlength = new int[]{0};
        for(int group = 0; group < groups; ++group) {
            for(Clause clause : clauses[group]) {idlength[0] = Math.max(idlength[0],clause.id.length());}}

        for(int group = 0; group < groups; ++group) {
            if(groups > 1) {st.append("Clause group " + group+"\n");}
            clauses[group].stream().sorted(clauses[group].comparator()).forEach(clause->
                st.append(clause.toString(idlength[0],symboltable)).append("\n"));}
        if(groups > 1) {st.append("\n");}
        return st.toString();
    }
}
