package Datastructures;

import Datastructures.Clauses.Quantifier;
import InferenceSteps.InfClauseSimplification;
import InferenceSteps.InfTrueLiteralInClause;
import InferenceSteps.InferenceStep;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * Represents a Clause in a logic system.
 * <P>
 * The clause extends LinkedItem and can therefore be part of a doubly connected linked list.
 *
 * @param <Literal> the type of Literal objects in the clause.
 */
public class Clause<Literal extends Datastructures.Literal> extends LinkedItem {

    /** the maximal number of literals where simplification by investigating all its models is still feasible. */
    private static final int maxModelSize = 12;
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

    /**Initializes a new Clause object.
     * <br>
     * multiple occurrences or literals are comprised into one Literal object.<br>
     * Complementary literals are removed.<br>
     * The resulting clause may be a tautology or unsatisfiable.
     *
     * @param inputClause           the input clause as generated by the Generator classes.
     * @param literalConstructor    the function to construct a Literal object from a literal
     */
    public Clause(int[] inputClause, Function<Integer,Literal> literalConstructor) {
        id = inputClause[0];
        quantifier = Quantifier.getQuantifier(inputClause[1]);
        assert(quantifier != null && quantifier != Quantifier.EQUIV && quantifier != Quantifier.AND);
        int firstLiteralIndex = quantifier.firstLiteralIndex;
        expandedSize = inputClause.length-firstLiteralIndex;
        switch(quantifier) {
            case OR: min = 1;                    max = expandedSize;   break;
            case ATLEAST:  min = inputClause[2]; max = expandedSize;   break;
            case ATMOST:   min = 0;              max = inputClause[2]; break;
            case EXACTLY:  min = inputClause[2]; max = min;            break;
            case INTERVAL: min = inputClause[2]; max = inputClause[3]; break;}
        literals = new ArrayList<>(expandedSize);
        for (int i = firstLiteralIndex; i < inputClause.length; i++) {
            int literal = inputClause[i];
            boolean multiple = false;
            for(Literal literalObject : literals) {
                if(literal == literalObject.literal) {
                    ++literalObject.multiplicity;
                    multiple = true; break;}
                if(literal == -literalObject.literal) {
                    --min; --max;
                    --literalObject.multiplicity;
                    if(literalObject.multiplicity == 0) literals.remove(literalObject);
                    multiple = true; break;}}
            if(!multiple) {literals.add(literalConstructor.apply(literal));}}
        expandedSize = 0;
        min = Math.max(0,min);
        for(Literal literalObject : literals) {
            if(min > 0) literalObject.multiplicity = Math.min(min, literalObject.multiplicity);
            expandedSize += literalObject.multiplicity;}
        max = Math.min(max,expandedSize);
        classifyQuantifier();}

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

    /**
     * Simplifies a clause recursively based on various conditions and rules.
     *
     * @param trackReasoning   Flag indicating whether reasoning should be tracked.
     * @param literalRemover   Consumer for removing literals.
     * @param reportTruth      BiConsumer for reporting truth.
     * @param monitor          Consumer for monitoring progress.
     * @param symboltable      Symboltable containing symbols and their values.
     * @return Result indicating the simplification outcome:
     *                           1: The clause is simplified to a true clause.
     *                           0: No further simplification is possible.
     *                          -1: The clause is simplified to a false clause.
     */
    public int simplify(boolean trackReasoning, Consumer<Literal> literalRemover,
                         BiConsumer<Integer,InferenceStep> reportTruth, Consumer<String> monitor, Symboltable symboltable) {
        int versionBefore = version;
        if(trackReasoning) {
            int[] clone = simpleClone();
            IntArrayList trueLiterals = new IntArrayList();
            int result = simplifyRecursively(trackReasoning, literalRemover,
                    (literal,inferenceStep) -> {trueLiterals.add((int)literal); reportTruth.accept(literal,inferenceStep);},
                    monitor,symboltable);
            if(result == -1) {inferenceSteps.add(new InfClauseSimplification(clone,null,null)); return result;}
            if(result == 1 || versionBefore == version) return result;
            addInferenceStep(new InfClauseSimplification(clone,this,trueLiterals));
            return result;}
        else return simplifyRecursively(trackReasoning, literalRemover, reportTruth, monitor,symboltable);}

    /**
     * Simplifies a clause recursively based on various conditions and rules.
     *
     * @param trackReasoning   Flag indicating whether reasoning should be tracked.
     * @param literalRemover   Consumer for removing literals.
     * @param reportTruth      BiConsumer for reporting truth.
     * @param monitor          Consumer for monitoring progress.
     * @param symboltable      Symboltable containing symbols and their values.
     * @return Result indicating the simplification outcome:
     *                           1: The clause is simplified to a true clause.
     *                           0: No further simplification is possible.
     *                          -1: The clause is simplified to a false clause.
     */
    public int simplifyRecursively(boolean trackReasoning, Consumer<Literal> literalRemover,
                        BiConsumer<Integer,InferenceStep> reportTruth, Consumer<String> monitor, Symboltable symboltable) {
        if(min <= 0 && max >= expandedSize) return 1; // true clause, e.g. [0,0] empty
        if(max < min || min > expandedSize) return -1;  // false clause, e.g. [1,1] empty (false literal removed from unit clause)

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

        if(literals.size() > maxModelSize) {
            if(monitor != null) {monitor.accept("Clause " + toString(symboltable,0) +
                    " has more than " + maxModelSize + " literal. Simplification may become incomplete.");}
            return 0;}

        IntArrayList models = getModels(monitor,symboltable);

        if(models.isEmpty()) return -1; // unsatisfiable clause [3,3] p^2,q^2

        if(models.size() == 1) { // [3,3] p,-q,r
            singletonModel(models.getInt(0), trackReasoning,reportTruth,monitor,symboltable);
            return 1;}

        if(extractTrueLiterals(models,trackReasoning, literalRemover, reportTruth, monitor, symboltable))
            return simplifyRecursively(trackReasoning, literalRemover, reportTruth, monitor, symboltable);

        if(extractIrrelevantLiterals(models, literalRemover, monitor, symboltable))
            return simplifyRecursively(trackReasoning, literalRemover, reportTruth, monitor, symboltable);

        if(divideByGCD(monitor,symboltable))
            return simplifyRecursively(trackReasoning, literalRemover, reportTruth, monitor, symboltable);

        return 0;
    }

    /** Computes the list of models for the clause.
     * <p>
     *  A model is an integer where bit i=1 means the i-th literal is true. <br>
     *  Example: clause p,q,r,-s: i = 1 means true(p),false(q,r,-s). i = 3 means true(p,q), false(r,-s).<br>
     *  As a side effect: the min- and max-values are narrowed according to the extreme values of all models.<br>
     *  Example: [2,3] p^2,q^2 -&gt; [2,2] p^2,q^2
     *
     * @return An IntArrayList containing the models for the clause.
     */
    protected IntArrayList getModels(Consumer<String> monitor, Symboltable symboltable) {
        IntArrayList models = new IntArrayList();
        int nModels = 1 << literals.size();
        int minValue = Integer.MAX_VALUE;
        int maxValue = 0;
        for (int model = 0; model < nModels; ++model) {
            int trueLiterals = 0;
            for (int j = 0; j < literals.size(); j++) {
                Literal literalObject = literals.get(j);
                int literal = literalObject.literal;
                if(literal > 0 && ((model & (1 << j)) != 0)) trueLiterals += literalObject.multiplicity;
            }
            if(min <= trueLiterals && trueLiterals <= max) {
                models.add(model);
                minValue = Math.min(minValue,trueLiterals);
                maxValue = Math.max(maxValue,trueLiterals);}}

        if(min != minValue || max != maxValue) {
            ++version;
            if(min != minValue) {
                if(monitor != null) monitor.accept("Clause "+ toString(symboltable,0) + " min increased to " + minValue);
                min = minValue;
                expandedSize = 0;
                for(Literal literalObject : literals) {
                    literalObject.multiplicity = Math.min(min,literalObject.multiplicity);
                    expandedSize += literalObject.multiplicity;}}
            if(max != maxValue) {
                if(monitor != null) monitor.accept("Clause "+ toString(symboltable,0) + " max reduced to " + maxValue);
                max = maxValue;}}
        return models;}

    /** Makes all literals of a clause with a single model true.
     * <br>
     * Example: [3,3] p^2,-q  p and -q must be true.
     *
     * @param model           an integer representing the model, where bit i=1 means the i-th literal is true.
     * @param trackReasoning  a boolean indicating whether reasoning should be tracked.
     * @param reportTruth     a BiConsumer function for reporting truth.
     * @param monitor         a Consumer function for monitoring.
     * @param symboltable     a Symboltable object for symbol table operations.
     */
    protected void singletonModel(int model,boolean trackReasoning,
                                       BiConsumer<Integer,InferenceStep> reportTruth, Consumer<String> monitor, Symboltable symboltable) {
        int[] clone = trackReasoning ? simpleClone() : null;
        for (int j = 0; j < literals.size(); j++) {
            int literal = literals.get(j).literal;
            if((model & (1 << j)) == 0) literal = -literal;
            InferenceStep step = trackReasoning ? new InfTrueLiteralInClause(clone,literal): null;
            reportTruth.accept(literal,step);}
        if(monitor != null) monitor.accept("Clause " + toString(symboltable,0) + ": single model: " + modelString(model,symboltable));}

    /**Extracts literals which are true/false in all models of the clause.
     *
     * @param models             the list of models to extract true literals from
     * @param trackReasoning     a boolean indicating whether reasoning should be tracked
     * @param literalRemover     a function for removing literals
     * @param reportTruth        a function for reporting truth
     * @param monitor            a function for monitoring
     * @param symboltable        the symbol table
     * @return true if any true literals were extracted, false otherwise
     */
    protected boolean extractTrueLiterals(IntArrayList models,boolean trackReasoning,Consumer<Literal> literalRemover,
                                  BiConsumer<Integer,InferenceStep> reportTruth, Consumer<String> monitor, Symboltable symboltable) {
        int[] clone = trackReasoning ? simpleClone() : null;
        boolean changed = false;
        for(int j = literals.size()-1; j >= 0; --j) {
            int mask = 1 << j;
            boolean allTrue = true; boolean allFalse = true;
            for(int model: models) {
                if((model & mask) == 0) allTrue = false; else allFalse = false;}
            int sign = 0;
            if(allTrue) sign = 1; else {if(allFalse) sign = -1;}
            if(sign != 0) {
                ++version;
                changed = true;
                int literal = sign*literals.get(j).literal;
                InferenceStep step = trackReasoning ? new InfTrueLiteralInClause(clone,literal): null;
                reportTruth.accept(literal,step);
                if(monitor != null) {
                    monitor.accept("Clause " + toString(symboltable,0) + ": has true literal " + Symboltable.toString(literal,symboltable));}
                removeLiteral(j, sign == 1);
                literalRemover.accept(literals.get(j));}}
        if(changed) classifyQuantifier();
        return changed;}

    /**Extracts literals whose truth is irrelevant for the truth of the clause.
     * <br>
     * Example: [2,3] p^2,q^2,r  -&gt; [2,3] p^2,q^2 (atleast one of p or q must be true, regardless of r)<br>
     * (-&gt; [2,2] p^2,q^2 -&gt; [1,2] p,q)
     *
     * @param models             the list of models to extract true literals from
     * @param literalRemover     a function for removing literals
     * @param monitor            a function for monitoring
     * @param symboltable        the symbol table
     * @return true if any of the literals were extracted, false otherwise
     */
    protected boolean extractIrrelevantLiterals(IntArrayList models,Consumer<Literal> literalRemover,
                                          Consumer<String> monitor, Symboltable symboltable) {
        int mSize = models.size();
        if(mSize % 2 == 1) return false; // no irrelevant literal
        int mSize2 = mSize/2;

        boolean changed = false;
        for(int j = literals.size()-1; j >= 0; --j) {
            int mask = 1 << j;
            int counter = 0;
            boolean next = false;
            for(int model : models) {
                if((model & mask) == 0) {
                    ++counter;
                    if(counter > mSize2) {next = true; break;} // not irrelevant
                    boolean found = false;
                    for(int model1 : models) {
                        if((model1 & mask) != 0 && (model|mask) == model1) {found = true; break;}}
                    if(!found) {next = true; break;}}}
            if(next || counter != mSize2) continue;
            ++version;
            changed = true;
            if(monitor != null) monitor.accept("Clause " + toString(symboltable,0) +
                    " irrelevant literal " + Symboltable.toString(literals.get(j).literal,symboltable) + " removed");
            literalRemover.accept(literals.get(j)); // inference step
            removeLiteral(j, false);}
        if(changed) classifyQuantifier();
        return changed;}


    /** divides the limits and the multiplicities by their greatest common divisor.
     *
     * @param monitor          null or a monitor.
     * @param symboltable      null or a symboltable.
     * @return true if the clause is changed.
     */
    protected boolean divideByGCD(Consumer<String> monitor, Symboltable symboltable) {
        if(min <= 1 || max == 1) return false;
        int gcd = Utilities.gcd(min,max);
        if(gcd == 1) return false;

        for(Literal literalObject : literals) {
            gcd = Utilities.gcd(gcd,literalObject.multiplicity);
            if(gcd == 1) return false;}

        int[] clause = (monitor != null) ? simpleClone() : null;
        ++version;
        min /= gcd;
        max /= gcd;
        expandedSize = 0;
        for(Literal literalObject : literals) {
            literalObject.multiplicity /= gcd;
            expandedSize += literalObject.multiplicity;}
        classifyQuantifier();
        if(monitor != null) {monitor.accept("Clause " + toString(clause,symboltable)+
                " + divided by GCD -> " + toString(symboltable,0));}
        return true;}



    /**Removes th j-th literal from the list of literals in the clause.
     * <p>
     * Updates the expandedSize and adjusts the multiplicity.
     * If isTrue is true, it also updates the min and max values of the remaining literals.
     * The quantifier is also updated.
     *
     * @param j       the index of the literal to be removed
     * @param isTrue  whether the removed literal is true or not
     */
    public void removeLiteral(int j, boolean isTrue) {
        Literal literalObject = literals.get(j);
        literals.remove(j);
        expandedSize -= literalObject.literal;
        if(isTrue) {
            min = Math.max(0, min - literalObject.multiplicity); max -= literalObject.multiplicity;
            expandedSize = 0;
            for (Literal litObject : literals) {
                litObject.multiplicity = Math.min(min, litObject.multiplicity);
                expandedSize += litObject.multiplicity;}}
        max = Math.min(max,expandedSize);
        classifyQuantifier();}

    /** Add an inference step to the list of inference steps.
     *
     * @param step The inference step to be added.
     */
    private void addInferenceStep(InferenceStep step) {
        if(step != null) {
            if(inferenceSteps == null) inferenceSteps = new ArrayList<>();
            inferenceSteps.add(step);}}



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

    /**
     * Creates a simple clone of the literal as an int-array: [id,version,quantifier,min,max,literal1,literal1.multiplicity,...]
     *
     * @return A clone of the literal with the essential information.
     */
    protected int[] simpleClone() {
        int[] clone = new int[5+2*literals.size()];
        clone[0] = id;
        clone[1] = version;
        clone[2] = quantifier.ordinal();
        clone[3] = min;
        clone[4] = max;
        for(int i = 0; i < literals.size(); ++i) {
            clone[5+2*i]   = literals.get(i).literal;
            clone[5+2*i+1] = literals.get(i).multiplicity;}
        return clone;}

    /**
     * Converts an int array representation of a clone to a string representation.
     *
     * @param clone the int array representation of a clone
     * @param symboltable the Symboltable object used for symbol lookup
     * @return the string representation of the clone
     */
    public static String toString(int[] clone,Symboltable symboltable) {
        String name = Integer.toString(clone[0]);
        if(clone[1] != 0) name += "."+clone[1];
        StringBuilder st = new StringBuilder();
        st.append(name).append(": ");
        Quantifier quantifier = Quantifier.getQuantifier(clone[2]);
        switch(Objects.requireNonNull(quantifier)) {
            case OR: break;
            case EXACTLY:
            case ATLEAST:  st.append(quantifier.abbreviation).append(clone[3]).append(" "); break;
            case ATMOST:   st.append(quantifier.abbreviation).append(clone[4]).append(" "); break;
            case INTERVAL: st.append("[").append(clone[3]).append(",").append(clone[4]).append("] ");}
        for(int i = 5; i < clone.length; i += 2) {
            int literal = clone[i]; int multiplicity = clone[i+1];
            st.append(Symboltable.toString(literal,symboltable));
            if(multiplicity > 1) st.append("^").append(multiplicity);
            if(i < clone.length-1)st.append(",");}
        return st.toString();}

    /**
     * Generates a string representation of the given model.
     *
     * @param model The integer representation of the model.
     * @param symboltable The given symbol table.
     * @return The string representation of the model.
     */
    public String modelString(int model, Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        for(int i = 0; i < literals.size(); ++i) {
            int sign = ((model & 1 << i) != 0) ? 1: -1;
            st.append(Symboltable.toString(sign*literals.get(i).literal ,symboltable));
            if(i < literals.size()-1) st.append(",");}
        return st.toString();}

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
