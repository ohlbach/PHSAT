package Solvers.Walker;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import Solvers.Simplifier.UnsatClause;

import java.util.ArrayList;
import java.util.Arrays;

/** A Clause object is essentially a collection of Literal objects.
 * The clauses are represented in interval-normalform [min,max].
 *  Nevertheless, the clauses keep their original connective.
 *  A clause may be part of a doubly quantified list (list of false clauses).
 */
public class Clause {
    /** the identifier for the clause. */
    protected int id;

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

    /** the number of literals which are true in the local model. */
    protected int locallyTrueLiterals;

    /** the list of all Literal objects in the clause. */
    protected ArrayList<Literal> literals = new ArrayList<>();

    /** true if there are literals with multiplicities &gt; 1. */
    protected boolean hasMultiplicities = false;

    /** the inference step which caused the derivation of this clause. */
    protected InferenceStep inferenceStep;

    /** a timestamp to be used by various algorithms. */
    protected int timestamp = 0;

    /** the next clause in a doubly quantified list. */
    protected Clause nextClause;

    /** the previous clause in a doubly quantified list. */
    protected Clause previousClause;

    /** indicates that the clause is still in the doubly connected list. */
    protected boolean isInList = false;


    /** The constructor turns an InputClause int[]-array into a Clause object.
     *
     * @param inputClause InputClause int[]-array.
     */
    public Clause(int[] inputClause) {
        id = inputClause[0];
        quantifier = Quantifier.getQuantifier(inputClause[1]);
        isDisjunction = quantifier == Quantifier.OR;
        inferenceStep = new InfInputClause(id);
        int length = inputClause.length;
        int start = quantifier.firstLiteralIndex;
        literals = new ArrayList<>(length-start);
        inputClause = Arrays.copyOf(inputClause,length);
        for(int i = start; i < length; ++i) {
            int literal1 = inputClause[i];
            if(literal1 == 0) continue;
            int multiplicity = 1;
            for(int j = i+1; j < length; ++j) {
                if(literal1 == inputClause[j]) {
                    ++multiplicity;
                    inputClause[j] = 0;}}
            expandedSize += multiplicity;
            Literal literalObject = new Literal(literal1,multiplicity);
            literalObject.clause = this;
            literals.add(literalObject);}
        hasMultiplicities = expandedSize > literals.size();

        switch(quantifier) {
            case OR:       min = 1;              max = expandedSize;   break;
            case ATLEAST:  min = inputClause[2]; max = expandedSize;   break;
            case ATMOST:   min = 0;              max = inputClause[2]; break;
            case EXACTLY:  min = inputClause[2]; max = inputClause[2]; break;
            case INTERVAL: min = inputClause[2]; max = inputClause[3];
        }
    }

    /** constructs a new clause from a list of literal,multiplicity pairs.
     * This constructor is basically for test purposes.
     *
     * @param id         the identifier.
     * @param quantifier the connective.
     * @param min        the lower limit.
     * @param max        the upper limit.
     * @param items      pairs literal,multiplicity.
     */
    public Clause(int id, Quantifier quantifier, int min, int max, int... items) {
        this.id = id;
        this.min = min;
        this.max = max;
        this.quantifier = quantifier;
        isDisjunction = quantifier == Quantifier.OR;
        literals = new ArrayList<>(items.length/2);
        for(int i = 0; i < items.length; i +=2) {
            int literal = items[i];
            int multiplicity = Math.min(items[i+1], max);
            expandedSize += multiplicity;
            Literal literalObject = new Literal(literal,multiplicity);
            literalObject.clause = this;
            literals.add(literalObject);}
        hasMultiplicities = expandedSize > literals.size();}

    /** This is a constructor for a disjunction.
     *
     * @param id             the identifier.
     * @param literalNumbers the literals of the disjunction.
     */
    public Clause(int id,  int... literalNumbers) {
        this.id = id;
        this.quantifier = Quantifier.OR;
        expandedSize = literalNumbers.length;
        for(int literal : literalNumbers) {
            Literal literalObject = new Literal(literal,1);
            literalObject.clause = this;
            literals.add(literalObject);}}


    /** checks if the clause is true because of its limits.
     *
     * @return true if the clause is true because of its limits.
     */
    protected boolean isTrue() {
        return min <= 0 && max >= expandedSize ;}

    /** checks if the clause is false because of its limits.
     *
     * @return true if the clause is false because of its limits.
     */
    protected boolean isFalse() {
        return min > expandedSize || max < 0 || max < min;}




    /** finds the Literal with the given literal.
     *
     * @param literal a literal.
     * @return null or a Literal with the given literal.
     */
    protected Literal findLiteral(int literal) {
        for(Literal literalObject : literals) {
            if(literalObject.literal == literal) return literalObject;}
        return null;}



    /** removes complementary pairs from the clause.
     * <br>
     * Example: atleast 4 p^3, -p^2, q, r -> atleast 2 p,q,r. <br>
     * Example: atleast 2 p^2, -p^1,q,r -> atleast 0 q,r -> true.<br>
     * Example: [0,2] p,-p,q,-q,r,-r -> unsatisfiable <br>
     * Example: [0,2] p,-p,q,-q,r,s -> -r&amp;-s <br>
     *
     * @param inputClause the original input clause (for the UnsatClause).
     * @return true if the clause became a true clause.
     * @throws UnsatClause if the clause is unsatisfiable.
     */
    protected boolean removeComplementaryLiterals(int[] inputClause) throws Unsatisfiable {
        for(int i = 0; i < literals.size()-1; ++i) {
            Literal literalObject1 = literals.get(i);
            int literal1 = literalObject1.literal;
            int multiplicity1 = literalObject1.multiplicity;
            for(int j = i+1; j < literals.size(); ++j) {
                Literal literalObject2 = literals.get(j);
                if(literalObject2.literal == -literal1) {
                    int multiplicity2 = literalObject2.multiplicity;
                    if(multiplicity1 == multiplicity2) {
                        min -= multiplicity1;
                        max -= multiplicity1;
                        literals.remove(j);
                        literals.remove(i--);
                        expandedSize -= multiplicity1;
                        break;}
                    if(multiplicity1 > multiplicity2) {
                        min -= multiplicity2;
                        max -= multiplicity2;
                        literalObject1.multiplicity -= multiplicity2;
                        literals.remove(j);
                        expandedSize -= multiplicity2;
                        break;}
                    min -= multiplicity1; // multiplicity1 < multiplicity2
                    max -= multiplicity1;
                    literalObject2.multiplicity -= multiplicity1;
                    literals.remove(i--);
                    expandedSize -= multiplicity1;
                    break;}}}
        if(literals.isEmpty() || (min <= 0 && max >= expandedSize)) return true;
        if(max < 0) throw new UnsatClause("","",inputClause);
        if(max == 0) {
            for(Literal literalObject : literals) {literalObject.literal *= -1; literalObject.multiplicity = 1;}
            min = 0; max = literals.size();
            quantifier = Quantifier.AND;
            return false;}
        min = Math.max(min,0);
        hasMultiplicities = expandedSize > literals.size();
        if(min == 1 && max == expandedSize) {
            quantifier = Quantifier.OR;
            isDisjunction = true;
            hasMultiplicities = false;
            for(Literal literalObject : literals) literalObject.multiplicity = 1;}
        return false;}


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
