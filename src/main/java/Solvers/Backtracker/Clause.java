package Solvers.Backtracker;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Solvers.Simplifier.UnsatClause;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Arrays;

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

    /** the next clause in a doubly quantified list. */
    protected Clause nextClause;

    /** the previous clause in a doubly quantified list. */
    protected Clause previousClause;

    private int[] inputClause;


    /** The constructor turns an InputClause int[]-array into a Clause object.
     *
     * @param inputClause InputClause int[]-array.
     */
    public Clause(int[] inputClause, String problemId, String solverId, IntArrayList trueLiterals) throws Unsatisfiable {
        this.inputClause = inputClause;
        id = inputClause[0];
        quantifier = Quantifier.getQuantifier(inputClause[1]);
        if(quantifier == Quantifier.OR) {makeDisjunction(inputClause,trueLiterals); return;}
        if(quantifier == Quantifier.EQUIV) {makeEquivalence(inputClause,problemId,solverId); return;}
        trueLiterals.clear();
        int length = inputClause.length;
        int start = quantifier.firstLiteralIndex;
        literals = new ArrayList<>(length-start);
        inputClause = Arrays.copyOf(inputClause,length);
        int complementaryLiterals = 0;
        for(int i = start; i < length; ++i) {
            int literal1 = inputClause[i];
            if(literal1 == 0) continue;
            int multiplicity = 1;
            for(int j = i+1; j < length; ++j) {
                int literal2 = inputClause[j];
                if(literal2 == literal1) {
                    ++multiplicity;
                    inputClause[j] = 0; continue;}
                if(literal2 == -literal1) {
                    --multiplicity; ++complementaryLiterals;
                    inputClause[j] = 0;}}
            if(multiplicity <= 0) continue;
            expandedSize += multiplicity;
            Literal literalObject = new Literal(literal1,multiplicity);
            literalObject.clause = this;
            literals.add(literalObject);}
        hasMultiplicities = expandedSize > literals.size();

        switch(quantifier) {
            case ATLEAST:
                min = inputClause[2] - complementaryLiterals;  max = expandedSize;
                if(min <= 0) {literals.clear(); return;} // tautology
                if(min > expandedSize)  throw new UnsatClause(problemId,solverId,inputClause);
                if(min == expandedSize) {
                    for(Literal literalObject : literals) trueLiterals.add(literalObject.literal);
                    literals.clear();
                    return;}
                for(Literal literalObject : literals) {
                    if(literalObject.multiplicity > min) {
                        expandedSize -= literalObject.multiplicity - min;
                        literalObject.multiplicity = min;}}
                return;
            case ATMOST:   min = 0; max = inputClause[2] - complementaryLiterals;
                break;
            case EXACTLY:
                min = inputClause[2]  - complementaryLiterals;
                max = inputClause[2]  - complementaryLiterals;
                break;
            case INTERVAL:
                min = Math.max(0,inputClause[2]  - complementaryLiterals);
                max = inputClause[3] - complementaryLiterals;}

        if(max < 0)   throw new UnsatClause(problemId,solverId,inputClause);
        if(max == 0)  {
            for(Literal literalObject : literals) trueLiterals.add(-literalObject.literal);
            literals.clear();
            return;}
        for(int i = 0; i < literals.size(); ++i) {
            Literal literalObject = literals.get(i);
            if(literalObject.multiplicity > max) {
                literals.remove(i--);
                expandedSize -= literalObject.multiplicity;
                trueLiterals.add(-literalObject.literal);}}
    }

    /** turns a disjunction into a clause object.
     *  Multiple literals are ignored.<br>
     *  If the clause is a tautology, then the literals are emptied.
     *
     * @param inputClause an inputClause array.
     */
    void makeDisjunction(int[] inputClause, IntArrayList trueLiterals) {
        int start = quantifier.firstLiteralIndex;
        int length = inputClause.length;
        literals = new ArrayList<>(length-start);
        for(int i = start; i < length; ++i) {
            int literal1 = inputClause[i];
            boolean multiple = false;
            for(int j = start; j < i; ++j) {
                int literal2 = inputClause[j];
                if(literal2 == -literal1) {literals.clear(); return;} // tautology
                if(literal2 == literal1)  {multiple = true; break;}}
            if(multiple) continue;
            Literal literalObject = new Literal(literal1,1);
            literalObject.clause = this;
            literals.add(literalObject);}
        hasMultiplicities = false;
        expandedSize = literals.size();
        min = 1; max = expandedSize;
        if(max == 1) {
            trueLiterals.add(literals.get(0).literal);
            literals.clear();}}

    void makeEquivalence(int[] inputClause, String problemId, String solverId) throws Unsatisfiable{
        int start = quantifier.firstLiteralIndex;
        int length = inputClause.length;
        literals = new ArrayList<>(length-start);
        for(int i = start; i < length; ++i) {
            int literal1 = inputClause[i];
            boolean multiple = false;
            for(int j = start; j < i; ++j) {
                int literal2 = inputClause[j];
                if(literal2 == -literal1) throw new UnsatClause(problemId, solverId,inputClause); // tautology
                if(literal2 == literal1)  {multiple = true; break;}}
            if(multiple) continue;
            Literal literalObject = new Literal(literal1,1);
            literalObject.clause = this;
            literals.add(literalObject);}
        if(literals.size() == 1) literals.clear();}

    boolean isEmpty() {
        return literals.isEmpty();}

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


    /** removes complementary pairs from the clause.
     * <br>
     * Example: atleast 4 p^3, -p^2, q, r -> atleast 2 p,q,r. <br>
     * Example: atleast 2 p^2, -p^1,q,r -> atleast 0 q,r -> true.<br>
     * Example: [0,2] p,-p,q,-q,r,-r -> unsatisfiable <br>
     * Example: [0,2] p,-p,q,-q,r,s -> -r&amp;-s <br>
     *
     * @param problemId the problem's identifier.
     * @param solverId the solver's identifier.
     * @return true if the clause became a true clause.
     * @throws UnsatClause if the clause is unsatisfiable.
     */
    boolean removeComplementaryLiterals(String problemId, String solverId) throws Unsatisfiable {
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
        if(max < 0) throw new UnsatClause(problemId,solverId,inputClause);
        if(max == 0) {
            for(Literal literalObject : literals) {literalObject.literal *= -1; literalObject.multiplicity = 1;}
            min = 0; max = literals.size();
            quantifier = Quantifier.AND;
            return false;}
        min = Math.max(min,0);
        hasMultiplicities = expandedSize > literals.size();
        if(min == 1 && max == expandedSize) {
            quantifier = Quantifier.OR;
            hasMultiplicities = false;
            for(Literal literalObject : literals) literalObject.multiplicity = 1;}
        return false;}

    /** reduces the clause's multiplicities, if necessary.
     * If min &gt; 0 then the multiplicities are reduces to min;<br>
     * If min = 0 then the atmost-clause is (virtually) turned into an atleast clause.<br>
     * The multiplicities are reduced in this version to 'negMin'.<br>
     * The clause is then (virtually) turned back into an atmost-clause.
     */
    void reduceMultiplicities() {
        if(min > 0) {
            for(Literal literalObject : literals) {
                if(literalObject.multiplicity > min) {
                    expandedSize -= literalObject.multiplicity - min;
                    literalObject.multiplicity = min;}}
            return;}
        int negMin = expandedSize - max;
        for(Literal literalObject : literals) {
            if(literalObject.multiplicity > negMin) {
                expandedSize -= literalObject.multiplicity - negMin;
                literalObject.multiplicity = negMin;}}
        max = expandedSize - negMin;
    }


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
