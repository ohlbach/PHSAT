package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Literals.LiteralIndexSorted;
import Datastructures.Symboltable;
import Datastructures.Theory.ImplicationDAG;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.stream.Stream;

/** Muss noch verändert werden */

/**
 * Created by ohlbach on 26.08.2018.
 *
 * This class is for collecting sets of clauses in different groups.
 * It supports inserting clauses, removing clauses and literals, retrieving clauses and literals.
 * A literal index is used to access literals and clauses quickly.
 * Removing literals and replacing them with representatives in an equivalence class can be
 * observed by corresponding observer functions.
 * <br>
 * The clauses are sorted according to a comparator (default: clause length) <br>
 * The literals in the literal index are also sorted (default: clause length)
 */
public class ClauseList {
    public int predicates;
    public ArrayList<Clause> clauses;                   // the list of clauses
    private final HashMap<Integer,Clause> id2Clause;      // maps clause ids to disjunctions
    public final LiteralIndex literalIndex;              // maps literals to CLiterals
    public int timestamp = 0;                            // for algorithms
    public int positiveClauses = 0;
    public int negativeClauses = 0;
    public int mixedClauses = 0;
    public ClauseStructure structure = ClauseStructure.MIXED;
    // they are called when literals are removed.
    private final ArrayList<BiConsumer<CLiteral,Boolean>> literalReplacementObservers = new ArrayList<>();
    // they are called when literals are replaced by representatives in a equivalence class.
    private final ArrayList<Consumer<Clause>> clauseRemovalObservers = new ArrayList<>();
    // they are called when clauses are removed.
    private final ArrayList<Consumer<ClauseStructure>> clauseStructureObservers = new ArrayList<>();
    // they are called when the clause structure becomes POSITIVE or NEGATIVE.

    /** creates a clause list. The number of disjunctions should be estimated.
     *
     * @param size       the estimated number of clauses
     * @param predicates the number of predicates.
     */
    public ClauseList(int size,int predicates) {
        this.predicates = predicates;
        clauses         = new ArrayList<>();
        id2Clause       = new HashMap<>();
        literalIndex    = new LiteralIndexSorted(predicates);}




        public ArrayList<CLiteral<Clause>> getLiterals(int literal) {return null;}

    /** adds an observer which is called after a literal is replaced by another one (a representative in an equivalence class).
     * It is called for the new CLiteral and the old literal (Integer).
     *
     * @param observer a consumer function to be applied to a CLiteral
     */
    public synchronized void addLiteralReplacementObserver(BiConsumer<CLiteral,Boolean> observer) {
        literalReplacementObservers.add(observer);}

    /** removes a literal replacement observer
     *
     * @param observer an observer*/
    public synchronized void removeLiteralReplacementObserver(BiConsumer<CLiteral,Boolean> observer) {
        literalReplacementObservers.remove(observer);}


    /** adds an observer which is called when a clause is removed
     *
     * @param observer a consumer function to be applied to a removed clause
     */
    public synchronized void addClauseRemovalObserver(Consumer<Clause> observer) {
        clauseRemovalObservers.add(observer);}

    /** removes a ClauseRemovalObserver
     *
     * @param observer an observer*/
    public synchronized void removeClauseRemovalObserver(Consumer<Clause> observer) {
        clauseRemovalObservers.remove(observer);}

    /** adds an observer which is called when the clauseStructure becomes POSITIVE or NEGATIVE
     *
     * @param observer a consumer function to be applied to a removed clause
     */
    public synchronized void addClauseStructureObserver(Consumer<ClauseStructure> observer) {
        clauseStructureObservers.add(observer);}

    /** removes a ClauseStructureObserver
     *
     * @param observer an observer*/
    public synchronized void removeClauseStructureObserver(Consumer<ClauseStructure> observer) {
        clauseStructureObservers.remove(observer);}

    /** removes all ClauseStructureObserver
     */
    public synchronized void removeClauseStructureObservers() {
        clauseStructureObservers.clear();}


    /** adds a clause to the list and updates the literal index
     *
     * @param clause to be added
     */
    public void addClause(Clause clause) {
        clause.listPosition = clauses.size();
        clauses.add(clause);
        integrateClause(clause);}


    /** integrates the clause in the literal indices and the id-map.
     *
      * @param clause the clause to be integrated.
     */
    protected void integrateClause(Clause clause) {
        id2Clause.put(clause.id,clause);
        for(CLiteral<Clause> literal : clause) {literalIndex.addLiteral(literal);}
        ClauseStructure oldStructure = structure;
        switch(clause.structure) {
            case MIXED:    ++mixedClauses; break;
            case NEGATIVE: ++negativeClauses; break;
            case POSITIVE: ++positiveClauses;}
        determineStructure();
        if(oldStructure == ClauseStructure.MIXED && structure != ClauseStructure.MIXED) {
            for(Consumer<ClauseStructure> observer: clauseStructureObservers) {observer.accept(structure);}}
        }

    /** sets the structure feature */
    private void determineStructure() {
        /*
        if(positiveClauses == 0 && negativeClauses == 0) {structure =  ClauseStructure.BOTH; return;}
        structure = ClauseStructure.MIXED;
        if(negativeClauses == 0) {structure = ClauseStructure.POSITIVE;}
        else {if(positiveClauses == 0) {structure = ClauseStructure.NEGATIVE;}}
        */}


    /** returns a clause for the given number
     *
     * @param id the clause's id
     * @return the clause or null
     */
    public Clause getClause(String id) {return id2Clause.get(id);}


    /** removes a clause and calls the clauseRemovalObservers. The clause itself is not changed.
     *
     * @param clause the clause to be removed
     */
    public void removeClause(Clause clause) {
        clauses.remove(clause);
        updateRemoval(clause);}

    /** removes the clause from the id-map and from the literal index, and calls the observers
     *
     * @param clause the removed clause.
     */
    private void updateRemoval(Clause clause) {
        ClauseStructure oldStructure = structure;
        clause.removed = true;
        id2Clause.remove(clause.id);
        for(CLiteral<Clause> cliteral : clause) {literalIndex.removeLiteral(cliteral);}
        switch(clause.structure) {
            case MIXED:    --mixedClauses; break;
            case NEGATIVE: --negativeClauses; break;
            case POSITIVE: --positiveClauses;}
        determineStructure();
        if(oldStructure == ClauseStructure.MIXED && structure != ClauseStructure.MIXED) {
            for(Consumer<ClauseStructure> observer: clauseStructureObservers) {observer.accept(structure);}}
        for(Consumer<Clause> observer : clauseRemovalObservers) {observer.accept(clause);}
    }




    /** removes a literal from the clause.<br>
     * All literalRemovalObservers are called.
     *
     * @param cliteral the literal to be removed.
     */
    public void removeLiteral(CLiteral<Clause> cliteral) {
        /*
        ClauseStructure oldStructure = structure;
        Clause clause = cliteral.clause;
        int group = 0;
        if(groups > 1) {
            boolean found = false;
            for(; group < groups; ++group) {if(clauses[group].contains(clause)) {found = true; break;}}
            if(!found) {return;}}
        switch(clause.structure) {
            case MIXED:    --mixedClauses; break;
            case NEGATIVE: --negativeClauses; break;
            case POSITIVE: --positiveClauses;}
        clauses[group].remove(clause);
        for(CLiteral<Clause> clit : clause) {literalIndex.removeLiteral(clit);}
        clause.removeLiteral(cliteral);
        clauses[group].add(clause);
        for(CLiteral<Clause> clit : clause) {literalIndex.addLiteral(clit);}
        switch(clause.structure) {
            case MIXED:    ++mixedClauses; break;
            case NEGATIVE: ++negativeClauses; break;
            case POSITIVE: ++positiveClauses;}
        determineStructure();
        if(oldStructure == ClauseStructure.MIXED && structure != ClauseStructure.MIXED) {
            for(Consumer<ClauseStructure> observer: clauseStructureObservers) {observer.accept(structure);}}
            */}

    /** removes all clauses with the given (pure) literal
     *
     * @param literal a pure literal
     */
    public void removeLiteral(int literal) {
        for(Object cLiteral : literalIndex.getLiterals(literal)) {
            removeClause(((CLiteral<Clause>)cLiteral).clause);}}

    /** All disjunctions with the literal are removed. <br>
     *  All negated literals are removed from the disjunctions. <br>
     *  All literalRemovalObservers are called.
     *
     * @param literal a literal which is supposed to be true.
     */
    public void makeTrue(int literal) { //we must avoid concurrent modification errors!
        for(Object cliteral : literalIndex.getLiterals(literal).toArray()) {removeClause(((CLiteral<Clause>)cliteral).clause);}
        for(Object cliteral : literalIndex.getLiterals(-literal).toArray()) {removeLiteral((CLiteral)cliteral);}}

    /** replaces a literal by its representative in an equivalence class in all clauses containing the literal and its negation.
     * If the clause then contains double literals, one of the is removed.<br>
     * If the clause becomes a tautology, it is entirely removed.<br>
     * The corresponding observers are called.<br>
     * LiteralReplacementObservers are called before the replacement (with parameter true) <br>
     * and after the replacement (with parameter false)
     *
     * @param literal       the literal to be replaced
     * @param representative the new literal
     */
    public void replaceByRepresentative(int representative, int literal) {
        ClauseStructure oldStructure = structure;
        for(int i = 1; i >= -1; i -= 2) {
            literal *= i;
            representative *= i;
            for(Object clit : literalIndex.getLiterals(literal).toArray()) {
                CLiteral<Clause> cliteral = (CLiteral)clit;
                Clause clause = cliteral.clause;
                if(clause.contains(-representative) >= 0) {removeClause(clause); continue;}    // tautology
                if(clause.contains(representative) >= 0)  {removeLiteral(cliteral); continue;} // double literals
                for(BiConsumer observer : literalReplacementObservers) {observer.accept(cliteral, true);}
                literalIndex.removeLiteral(cliteral);
                cliteral.literal = representative;
                literalIndex.addLiteral(cliteral);
                for(BiConsumer observer : literalReplacementObservers) {observer.accept(cliteral, false);}}}
        if(oldStructure == ClauseStructure.MIXED && structure != ClauseStructure.MIXED) {
            for(Consumer<ClauseStructure> observer: clauseStructureObservers) {observer.accept(structure);}}}

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
     * @param down             if true then the DAG is traversed downwards, otherwise upwards
     * @return  a stream of CLiterals l such that literal implies l (including the literal itself.)
     */
    public Stream<CLiteral> stream(int literal, ImplicationDAG implicationDAG, boolean down) {
        Stream<CLiteral>[] stream = new Stream[]{null};
        implicationDAG.apply(literal,down, (lit -> {
                if(stream[0] == null) {stream[0] =  literalIndex.getLiterals(lit).stream();}
                else {stream[0] = Stream.concat(stream[0], literalIndex.getLiterals(lit).stream());}}));
        return stream[0];}


    /** generates a stream of CLiterals which contradict the given literal<br>
     * Example:  literal = p and p -&gt; q: all CLiterals with -p and -q are returned.
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
    public void apply(int literal, ImplicationDAG implicationDAG, boolean down, Consumer<CLiteral<Clause>> consumer) {
        implicationDAG.apply(literal,down, (lit -> {
            for(Object cLiteral : literalIndex.getLiterals(lit)) {consumer.accept((CLiteral<Clause>)cLiteral);}}));}


    /** applies a consumer to all CLiterals which contradict the given literal (including the -literal itself)
     *
     * @param literal        a literal
     * @param implicationDAG the ID_Implications
     * @param consumer       a function to be applied to a CLiteral
     */
    public void applyContradicting(int literal, ImplicationDAG implicationDAG, Consumer<CLiteral<Clause>> consumer) {
        implicationDAG.apply(-literal,false, (lit -> {
            for(Object  cLiteral : literalIndex.getLiterals(lit)) {consumer.accept((CLiteral<Clause>)cLiteral);}}));}


    /** the actual number of clauses
     *
     * @return the number of clauses
     */
    public int size() {/*
        int size = 0;
        for(int group = 0; group < groups; ++group) {size += clauses[group].size();}
        return size;*/
        return 0;}

    /** the number of clauses in the group
     *
     * @param group a clause group
     * @return the number of clauses in the group
     */
    public int size(int group) {
        /*return clauses[group].size();*/
        return 0;}

    /** checks if the clause set is empty
     *
     * @return true if the clause set is empty.
     */
    public boolean isEmpty() {/*
        for(int group = 0; group < groups; ++group) {
            if(!clauses[group].isEmpty()) {return false;}}*/
        return true;}


    /** checks if the clause group is empty
     *
     * @param group a clause group
     * @return true if the clause group is empty.
     */
    public boolean isEmpty(int group) {
        /*return clauses[group].isEmpty();*/
        return false;}


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
        /*
        StringBuilder st = new StringBuilder();
        int[] idlength = new int[]{0};
        for(int group = 0; group < groups; ++group) {
            for(Clause clause : clauses[group]) {idlength[0] = Math.max(idlength[0],clause.id.length());}}

        for(int group = 0; group < groups; ++group) {
            if(groups > 1) {st.append("Clause group " + group+"\n");}
            clauses[group].stream().sorted(clauses[group].comparator()).forEach(clause->
                st.append(clause.toString(idlength[0],symboltable)).append("\n"));}
        if(groups > 1) {st.append("\n");}
        st.append("Clause Structure: " + structure);
        st.append(" posititives: " + positiveClauses + ", negatives " + negativeClauses + ", mixed " + mixedClauses + "\n");
        return st.toString();
        */
        return "";
    }
}
