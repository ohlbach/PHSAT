package Solvers.Walker;

import Datastructures.Clauses.Quantifier;
import Datastructures.LinkedItem;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;

import java.util.ArrayList;

/** A Clause object is essentially a collection of Literal objects.
 * The clauses are represented in interval-normalform [min,max].
 *  Nevertheless, the clauses keep their original connective.
 *  A clause may be part of a doubly quantified list (list of false clauses).
 */
public class Clause extends LinkedItem {
    /** the identifier for the clause. */
    protected int id;
    protected int version;

    /** the quantifier */
    protected Quantifier quantifier;

    /** true if the connective is OR. */
    protected boolean isDisjunction;

    /** the lower limit for Interval clauses. */
    protected int min = 0;
    /** the upper limit for Interval clauses. */
    protected int max = 0;

    /** the sum of all multiplicities of the literals. */
    protected int expandedSize = 0;

    /** the number of true literals in the local model. */
    protected int trueLiterals = 0;

    /** flag to indicate that the clause is true in the local model. */
    protected boolean isLocallyTrue;

    /** the list of all Literal objects in the clause. */
    protected ArrayList<Literal> literals = new ArrayList<>();

    /** true if there are literals with multiplicities &gt; 1. */
    protected boolean hasMultiplicities = false;

    /** the inference step which caused the derivation of this clause. */
    protected InferenceStep inferenceStep;

    /** a timestamp to be used by various algorithms. */
    protected int timestamp = 0;

    public Clause(Solvers.Normalizer.Clause clause) {
        id = clause.id;
        version = clause.version;
        quantifier = clause.quantifier;
        min = clause.min;
        max = clause.max;
        expandedSize = clause.expandedSize;
        literals = new ArrayList<>(clause.literals.size()/2);
        for(int i = 0; i < clause.literals.size()-1; i +=2) {
            Literal literalObject = new Literal(clause.literals.get(i),clause.literals.get(i+1));
            literalObject.clause = this;
            literals.add(literalObject);}
    }


    /** checks if the clause is true because of its limits.
     *
     * @return true if the clause is true because of its limits.
     */
    boolean isTrue() {
        return min <= 0 && max >= expandedSize ;}

    /** checks if the clause is false because of its limits.
     *
     * @return true if the clause is false because of its limits.
     */
    boolean isFalse() {
        return min > expandedSize || max < 0 || max < min;}


    /** finds the Literal with the given literal.
     *
     * @param literal a literal.
     * @return null or a Literal with the given literal.
     */
    Literal findLiteral(int literal) {
        for(Literal literalObject : literals) {
            if(literalObject.literal == literal) return literalObject;}
        return null;}

    /** replaces the old literal by the new one.
     *
     * @param oldLiteral a literal in the clause.
     * @param newLiteral a new literal.
     */
    void replaceLiteral(Literal oldLiteral, Literal newLiteral) {
        for(int i = 0; i < literals.size(); ++i) {
            if(literals.get(i) == oldLiteral) {literals.set(i,newLiteral); return;}}}


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
        String name = Integer.toString(id);
        if(version != 0) name += "."+version;
        StringBuilder st = new StringBuilder();
        st.append((size == 0) ? name : String.format("%"+size+"s",name)).append(": ");
        switch(quantifier) {
            case OR: break;
            case EXACTLY:
            case ATLEAST:  st.append(quantifier.abbreviation).append(min).append(" "); break;
            case ATMOST:   st.append(quantifier.abbreviation).append(max).append(" "); break;
            case INTERVAL: st.append("[").append(min).append(",").append(max).append("] ");}
        if(!literals.isEmpty()) {
            int length = literals.size()-1;
            for(int i = 0; i < length; ++i) {
                st.append(literals.get(i).toString(symboltable)).append(quantifier.separator);}
            st.append(literals.get(length).toString(symboltable));}
        return st.toString();}

}
