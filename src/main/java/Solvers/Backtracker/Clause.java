package Solvers.Backtracker;

import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import Solvers.Normalizer.NormalizerOld;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class Clause {
    /** the identifier for the clause. */
    protected int id;

    /** the quantifier */
    protected Quantifier quantifier;

    /** the lower limit for Interval clauses. */
    protected int min = 0;
    /** the upper limit for Interval clauses. */
    protected int max = 0;

    /** the sum of all multiplicities of the literals. */
    protected int expandedSize = 0;

    /** the list of all Literal objects in the clause. */
    protected ArrayList<Literal> literals = new ArrayList<>();

    /** true if there are literals with multiplicities &gt; 1. */
    protected boolean hasMultiplicities = false;

    protected int timestamp = 0;

    /** the next clause in a doubly quantified list. */
    protected Clause nextClause;

    /** the previous clause in a doubly quantified list. */
    protected Clause previousClause;

    /** this constructor turns a nomalizedClause to a clause for the Backtracker solver.
     *
     * @param normalizedClause a normalized and simplified clause from the Normalizer.
     */
    public Clause(IntArrayList normalizedClause) {
        id = normalizedClause.getInt(0);
        quantifier = NormalizerOld.getQuantifier(normalizedClause);
        min = NormalizerOld.getMin(normalizedClause);
        max = NormalizerOld.getMax(normalizedClause);
        expandedSize = NormalizerOld.getExpandedSize(normalizedClause);
        hasMultiplicities = NormalizerOld.hasMultiplicities(normalizedClause);
        for(int i = NormalizerOld.literalsStart; i <= normalizedClause.size()-2; i +=2) {
            Literal literal = new Literal(normalizedClause.get(i),normalizedClause.get(i+1));
            literals.add(literal);
            literal.clause = this;}}


    boolean isEmpty() {
        return literals.isEmpty();}



    /** returns the number of Literal objects in the clause.
     *
     * @return the number of Literal objects in the clause.
     */
    public int size() {return literals.size();}

    /** returns the sum of the literal's multiplicities.
     *
     * @return the sum of the literal's multiplicities.
     */
    public int expandedSize() {return expandedSize;}

    /** turns the clause into a string.
     *
     * @return a string representation of the clause.
     */
    public String toString() {return toString(null,0);}

    /** turns the clause into a string.
     *
     * @param symboltable null or a symboltable.
     * @param size 0 or the length of the clause number string.
     * @return a string representation of the clause.
     */
    public String toString(Symboltable symboltable, int size) {
        StringBuilder st = new StringBuilder();
        st.append((size == 0) ? id : String.format("%"+size+"s",id)).append(": ");
        switch(quantifier) {
            case OR: break;
            case EXACTLY:
            case ATLEAST:  st.append(quantifier.abbreviation).append(min).append(" "); break;
            case ATMOST:   st.append(quantifier.abbreviation).append(max).append(" "); break;
            case INTERVAL: st.append("[").append(min).append(",").append(max).append("] ");}
        if(literals.size() > 0) {
            int length = literals.size()-1;
            for(int i = 0; i < length; ++i) {
                st.append(literals.get(i).toString(symboltable)).append(quantifier.separator);}
            st.append(literals.get(length).toString(symboltable));}
        return st.toString();}

}
