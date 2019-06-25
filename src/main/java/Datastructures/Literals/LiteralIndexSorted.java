package Datastructures.Literals;

import Utilities.Sizable;

import java.util.*;

/**
 * Created by ohlbach on 25.06.2019.
 */
public class LiteralIndexSorted<Clause extends Sizable> extends LiteralIndex<Clause> {
    private TreeSet<CLiteral<Clause>>[] posOccurrences;  // maps each positive predicate to the list of occurrences
    private TreeSet<CLiteral<Clause>>[] negOccurrences;  // maps each negative predicate to the list of occurrences
    private TreeSet<CLiteral<Clause>> emptyList = new TreeSet();;
    private Comparator<CLiteral<Clause>> comparator;

    /** constructs an index for a given number of predicates
     *
     * @param predicates the number of predicates
     */
    public LiteralIndexSorted(int predicates) {
        super(predicates);
        posOccurrences = new TreeSet[predicates + 1];
        negOccurrences = new TreeSet[predicates + 1];
        comparator = Comparator.comparingInt(lit->lit.clause.size());
    }

    /** constructs an index for a given number of predicates
     *
     * @param predicates the number of predicates
     */
    public LiteralIndexSorted(int predicates, Comparator<CLiteral<Clause>> comparator) {
        super(predicates);
        posOccurrences = new TreeSet[predicates + 1];
        negOccurrences = new TreeSet[predicates + 1];
        this.comparator = comparator;
    }


    /** adds a literal to the index
     *
     * @param cliteral the literal to be added
     */
    public void addLiteral(CLiteral<Clause> cliteral) {
        int literal = cliteral.literal;
        int predicate = Math.abs(literal);
        TreeSet<CLiteral<Clause>>[] list = literal > 0 ? posOccurrences : negOccurrences;
        TreeSet<CLiteral<Clause>> lits = list[predicate];
        if(lits == null) {
            lits = new TreeSet(comparator);
            list[predicate] = lits;}
        lits.add(cliteral);}

    /** removes the literal from the index (in constant time)
     *
     * @param cliteral the literal to be removed.
     */
    public void removeLiteral(CLiteral<Clause> cliteral) {
        int literal = cliteral.literal;
        TreeSet<CLiteral<Clause>> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        if(list == null) {return;}
        int size = list.size()-1;
        if(size == 0) {
            if(literal > 0) {posOccurrences[literal] = null;}
            else            {negOccurrences[-literal] = null;}
            signalPurity(-literal);}
        else {list.remove(cliteral);}
    }


    /** returns the CLiterals with the given literal (integer)
     *
     * @param literal the literal (integer)
     * @return the list of occurrences (CLiterals)
     */
    public AbstractCollection<CLiteral<Clause>> getLiterals(int literal) {
        assert literal != 0 && (Math.abs(literal) <= predicates);
        TreeSet<CLiteral<Clause>> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return list == null ? emptyList : list;}

    /** returns the number of cLiterals indexed by this literal
     *
     * @param literal a literal
     * @return the number of cLiterals indexed by this literal
     */
    public int size(int literal) {
        TreeSet<CLiteral<Clause>> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return list == null ? 0 : list.size();}

    public boolean isEmpty(int literal) {
        TreeSet<CLiteral<Clause>> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return list == null || list.isEmpty();}

    public Iterator<CLiteral<Clause>> iterator(int literal) {
        TreeSet<CLiteral<Clause>> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return (list == null) ? emptyList.iterator() : list.iterator();}


}
