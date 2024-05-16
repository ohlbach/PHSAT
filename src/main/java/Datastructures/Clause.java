package Datastructures;

import Datastructures.Clauses.Quantifier;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Represents a Clause in a logic system.
 * <P>
 * The clause extends LinkedItem and can therefore be part of a doubly connected linked list.
 *
 * @param <Literal> the type of Literal objects in the clause.
 */
public class Clause<Literal extends Datastructures.Literal> extends LinkedItem {
    /** the identifier for the clause. */
    public int id;
    /** the version number (for simplified clauses) */
    public int version;
    /** the quantifier */
    public Quantifier quantifier;
    /** the lower limit for Interval clauses. */
    public int min;
    /** the upper limit for Interval clauses. */
    public int max;
    /** the sum of all multiplicities of the literals. */
    public int expandedSize;

    /** the list of all Literal objects in the clause. */
    public ArrayList<Literal> literals = new ArrayList<>();

    /** null or a list of inference steps */

    public ArrayList<InferenceStep> inferenceSteps = null;
    /**
     * Constructs a Clause object with the given parameters.
     *
     * @param id the id of the clause
     * @param version the version of the clause
     * @param quantifier the quantifier of the clause
     * @param min the minimum value of the clause
     * @param max the maximum value of the clause
     * @param expandedSize the expanded size of the clause
     */
    public Clause(int id, int version, Quantifier quantifier, int min, int max, int expandedSize) {
        this.id = id;
        this.version =version;
        this.quantifier =quantifier;
        this.min =min;
        this.max =max;
        this.expandedSize = expandedSize;}


    /**
     * Classify the quantifier based on the values of min and max.
     * Possible quantifiers are EXACTLY, ATLEAST, ATMOST, and INTERVAL.
     * If min is equal to max, the quantifier is set to EXACTLY.
     * If max is equal to the expandedSize, the quantifier is set to ATLEAST or OR (depending on min).
     * If min is equal to 0, the quantifier is set to ATMOST.
     * Otherwise, the quantifier is set to INTERVAL.
     */
    public void classifyQuantifier() {
        if(min == max) {quantifier = Quantifier.EXACTLY; return;}
        if(max == expandedSize) {
            quantifier = (min == 1) ? Quantifier.OR : Quantifier.ATLEAST;
            return;}
        if(min == 0) {quantifier = Quantifier.ATMOST; return;}
        quantifier = Quantifier.INTERVAL;}

    /**
     * Checks if the literals list is empty.
     *
     * @return true if the literals list is empty, false otherwise.
     */
    boolean isEmpty() {
        return literals.isEmpty();}

    /** finds the Literal with the given literal.
     *
     * @param literal a literal.
     * @return null or a Literal with the given literal.
     */
    Literal findLiteral(int literal) {
        for(Literal literalObject : literals) {
            if(literalObject.literal == literal) return literalObject;}
        return null;}

    public int simplify(boolean trackReasoning, Consumer<Literal> literalRemover,
                         BiConsumer<Integer,InferenceStep> reportTruth, Consumer<String> monitor, Symboltable symboltable) {
        int result = simplifyRecursively(trackReasoning, literalRemover, reportTruth, monitor,symboltable);
        return result;
    }

    public int simplifyRecursively(boolean trackReasoning, Consumer<Literal> literalRemover,
                        BiConsumer<Integer,InferenceStep> reportTruth, Consumer<String> monitor, Symboltable symboltable) {
        if(min <= 0 && max >= expandedSize) return 1; // true clause
        if(max < min || min > expandedSize) return -1;  // false clause

        int sign = 0;
        if(quantifier == Quantifier.AND || (min == expandedSize)) {sign = 1;}
        else {if (max == 0) sign = -1;}

        if(sign != 0) {
            for(Literal literalObject : literals) {
                int literal = sign*literalObject.literal;
                reportTruth.accept(literal,null);} // all literals have a truth value.
            if(monitor != null) monitor.accept("All literals in clause " + toString(symboltable,0) + " are true.");
            return 1;}

        if(quantifier == Quantifier.OR) return 0;  // no further simplification possible.

        IntArrayList models = new IntArrayList();
        int nModels = 1 << literals.size();
        int minValue = Integer.MAX_VALUE;
        int maxValue = 0;
        for (int model = 0; model < nModels; ++model) {
            int trueLiterals = 0;
            for (int j = 0; j < literals.size(); j++) {
                Literal literalObject = literals.get(j);
                int literal = literalObject.literal;
                if((literal > 0 && ((model & (1 << j)) != 0)) ||
                   (literal < 0 && ((model & (1 << j)) == 0))) trueLiterals += literalObject.multiplicity;
                 }
            if(min <= trueLiterals && trueLiterals <= max) {
                models.add(model);
                minValue = Math.min(minValue,trueLiterals);
                maxValue = Math.max(maxValue,trueLiterals);}}

        if(models.isEmpty()) return -1; // unsatisfiable clause [3,3] p^2,q^2

        if(min != minValue || max != maxValue) ++version; // [2,3] p^2,q^2 -> [2,2] p^2,q^2
        if(min != minValue) {
             if(monitor != null) monitor.accept("Clause "+ toString(symboltable,0) + " min increased to " + minValue);
             min = minValue;}
        if(max != maxValue) {
            if(monitor != null) monitor.accept("Clause "+ toString(symboltable,0) + " max reduced to " + maxValue);
            max = maxValue;}

        if(models.size() == 1) { // [3,3] p,-q,r
            int model = models.getInt(0);
            for (int j = 0; j < literals.size(); j++) {
                Literal literalObject = literals.get(j);
                int literal = literalObject.literal;
                if((model & (1 << j)) != 0) reportTruth.accept(literal,null);
                else reportTruth.accept(-literal,null);}
            if(monitor != null) monitor.accept("Clause " + toString(symboltable,0) + ": single model " +model);
            return 1;}

        boolean changed = false;
        for(int j = 0; j < literals.size(); ++j) {
             int mask = 1 << j;
             boolean allTrue = true; boolean allFalse = true;
             for(int model: models) {
                 if((model & mask) != 1) allTrue = false; else allFalse = false;}
             sign = 0;
             if(allTrue) sign = 1; else {if(allFalse) sign = -1;}
             if(sign != 0) {
                 changed = true;
                 reportTruth.accept(sign*literals.get(j).literal,null);
                 removeLiteral(j, sign == 1);
                 j--;}}
        if(changed) return simplifyRecursively(trackReasoning, literalRemover, reportTruth, monitor, symboltable);




        return 0;
    }

    public void removeLiteral(int j, boolean isTrue) {
        Literal literalObject = literals.get(j);
        literals.remove(j);
        expandedSize -= literalObject.literal;
        if(isTrue) {
            min = Math.max(0, min- literalObject.multiplicity); max -= literalObject.multiplicity;
            expandedSize = 0;
            for(int i = 0; i < literals.size(); ++i) {
                Literal litObject = literals.get(i);
                litObject.multiplicity = Math.min(min, litObject.literal);
                expandedSize += litObject.multiplicity;}}
        classifyQuantifier();}


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

    /** checks if the clause is true because of its limits.
     *
     * @return true if the clause is true because of its limits.
     */
    public boolean isTrue() {
        return min <= 0 && max >= expandedSize ;}

    /** checks if the clause is false because of its limits.
     *
     * @return true if the clause is false because of its limits.
     */
    public boolean isFalse() {
        return min > expandedSize || max < 0 || max < min;}

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
                st.append(literals.get(i).toString(symboltable,0)).append(quantifier.separator);}
            st.append(literals.get(length).toString(symboltable,0));}
        return st.toString();}


}
