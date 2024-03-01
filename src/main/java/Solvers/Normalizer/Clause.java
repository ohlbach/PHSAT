package Solvers.Normalizer;

import Datastructures.Clauses.Quantifier;
import Datastructures.LinkedItem;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import InferenceSteps.NMISEquivalentLiteral;
import Management.Monitor.Monitor;
import Solvers.Normalizer.NMInferenceSteps.NMISTrueLiteral;
import Solvers.Normalizer.NMInferenceSteps.NMInferenceStep;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.IntPredicate;

/** This class represents clauses to be used in the Normalizer method.
 * Clause is a subclass of LinkedItem. Therefore, it can become part of a (single) doubly linked list.
 * Since the clause class has variables min and max, it can represent all versions of quantified clauses.
 * Nevertheless, the quantifier variable is always set to the most specific Quantifier.
 * <br>
 * The constructor just creates a new clause without any simplifications.
 * Simplifications are be done in the simplify method.
 * Each simplification keeps the clause's identifier, but increments its version number.
 * The simplifications can be tracked by adding NMInferenceStep objects to the inferenceSteps list.
 * The NMInferenceStep objects keep a clone of the clause before the simplification.
 * Therefore, inference steps can be verified by checking if the simplified clause is true in all
 * models of the original clause (see the verify-method of NMInferenceStep).
 * <br>
 * In most concrete examples of clauses, there should not be redundancies like large multiplicities
 * or true or false clauses. Therefore, the algorithms need not be optimized to simplify clauses as fast as possible.
 */
public class Clause extends LinkedItem {
    /**The original identifier of the input clause. */
    public int id;
    /** indicates simplified versions of the clause. The version of the original clause is 0.*/
    public int version = 0;

    /** the quantifier, but not EQUIV */
    public Quantifier quantifier;

    /** the lower limit for the quantification*/
    public int min;

    /** the upper limit for the quantification */
    public int max;

    /** indicates that the clause is a tautology, e.g. p,-p or [0,2] p,q */
    public boolean isTrue = false;

    /** indicates that the clause is false, e.g. [1,2] p^3*/
    public boolean isFalse = false;

    /** literal_1,multiplicity_1,literal2,multiplicity_2, ... */
    public IntArrayList literals;

    /** the original input clause */
    public int[] inputClause;

    /** the number of the literals, multiplied by their multiplicities */
    public int expandedSize = 0;

    /** An id for a potential monitor */
    private final String monitorId = "Normalizer.clause";

    /** A list of inference steps which transformed the clause */
    public ArrayList<NMInferenceStep> inferenceSteps;

    /** Creates a Clause object from the given input clause.
     * The constructor does not apply to AND and EQUIV clauses.
     * Multiple occurrences of literals are comprised into one occurrence with corresponding multiplicities.<br>
     * The clause's quantifier is optimized to represent the most specific Quantifier.
     * For example, a disjunction which is actually a unit clause gets the quantifier AND.
     *
     * @param inputClause the input clause.
     * @param trackReasoning if true then inference steps are generated
     * @param monitor        null or a monitor
     * @param symboltable    null or a symboltable.
     */
    public Clause(int[] inputClause, boolean trackReasoning, Monitor monitor, Symboltable symboltable) {
        this.inputClause = inputClause;
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
        literals = new IntArrayList(2*expandedSize);
        for (int i = firstLiteralIndex; i < inputClause.length; i++) {
            int literal = inputClause[i];
            boolean multiple = false;
            for(int j = 0; j < literals.size(); j += 2) {
                if(literal == literals.getInt(j)) {
                    literals.set(j+1,literals.getInt(j+1)+1);
                    multiple = true; break;}}
            if(!multiple) {literals.add(literal); literals.add(1);}}
        classifyClause(trackReasoning,monitor,symboltable);}

    public Clause() {}

    /**
     * Creates a logical clone of the Clause object.
     * Only the essential attributes are cloned.
     *
     * @return a new Clause object with the same logical values as the original object.
     */
    protected Clause clone() {
        Clause clause = new Clause();
        clause.id = id;
        clause.version = version;
        clause.quantifier = quantifier;
        clause.literals = literals.clone();
        clause.min = min;
        clause.max = max;
        clause.expandedSize = expandedSize;
        return clause;}

    /**
     * Classifies the clause based on its properties.
     * <br>
     * - If the quantifier is AND, returns immediately.<br>
     * - If the clause is true, returns immediately.<br>
     * - If the min value is equal to the expanded size, sets the quantifier to AND and returns.<br>
     * - If the min value is 1 and the max value is equal to the expanded size, sets the quantifier to OR and returns.<br>
     * - If the min value is equal to the max value, sets the quantifier to EXACTLY and returns.<br>
     * - If the min value is 0, sets the quantifier to ATMOST and returns.<br>
     * - If the max value is equal to the expanded size, sets the quantifier to ATLEAST and returns.<br>
     * - Otherwise, sets the quantifier to INTERVAL.
     */
    private void classifyClause(boolean trackReasoning, Monitor monitor, Symboltable symboltable) {
        if(quantifier == Quantifier.AND) return;
        if(min <= 0 && max == expandedSize) {isTrue  = true; return;}
        if(min > expandedSize)              {isFalse = true; return;}
        if(min == expandedSize)             {
            if(trackReasoning) inferenceSteps.add(new NMInferenceStep("andConversion", clone()));
            if(monitor != null) monitor.println(monitorId,"Clause " + toString(symboltable,0) + " turned into a conjunction.");
            quantifier = Quantifier.AND;
            return;}
        if(max == 0){
            Clause cloned = (trackReasoning || monitor != null) ? clone() : null;
            if(trackReasoning) inferenceSteps.add(new NMInferenceStep("negatedAndConversion",cloned));
            quantifier = Quantifier.AND;
            for(int i = 0; i < literals.size()-1; i +=2) literals.set(i,-literals.getInt(i));
            if(monitor != null) monitor.println(monitorId,"Clause " + cloned.toString(symboltable,0) +
                    " turned into negated conjunction " + toString(symboltable,0));
            return;}
        if(min == 1 && max == expandedSize) {quantifier = Quantifier.OR;      return;}
        if(min == max)                      {quantifier = Quantifier.EXACTLY; return;}
        if(min == 0)                        {quantifier = Quantifier.ATMOST;  return;}
        if(max == expandedSize)             {quantifier = Quantifier.ATLEAST; return;}
        quantifier = Quantifier.INTERVAL;}

    /**
     * Checks if the interval spans the entire clause (e.g. [0,3] p,q,r)
     *
     * @return true if the conditions for "isTrue" are met, false otherwise.
     */
    public boolean isTrue() {
        if(min <= 0 && max == expandedSize) {isTrue = true; return true;}
        return false;}

    /**
     * Checks if making the given literal true, makes the entire clause true.
     *
     * @param literal the literal to check
     * @return true making the literal true, makes the entire clause true.
     */
    public boolean isTrue(int literal) {
        if(min > 0) {
            for(int i = 0; i < literals.size()-1; i += 2) {
                if(literal == literals.getInt(i)) {
                    if(literals.getInt(i+1) >= min) return true;}} // multiplicity(literal) >= min makes the clause true.
            return false;}
        else return true;}

    /**
     * Checks if the clause is true in the given model.
     * The method also works for clauses with complementary literals.
     *
     * @param model the true literals
     * @return true if the clause is true in the given model.
     */
    public boolean isTrue(IntPredicate model) {
        if(quantifier == Quantifier.AND) {
            for(int i = 0; i < literals.size()-1; i += 2) {
                if(!model.test(literals.getInt(i))) return false;}
            return true;}

        int trueLiterals = 0;
        for(int i = 0; i < literals.size()-1; i += 2) {
            int literal = literals.getInt(i);
            boolean complementary = false;
            for(int j = 0; j < i; j +=2) {
                if(literals.getInt(j) == -literal) {
                    trueLiterals += Math.abs(literals.getInt(i+1) - literals.getInt(j+1));
                    complementary = true; break;}}
            if(!complementary && model.test(literals.getInt(i))) trueLiterals += literals.getInt(i+1);}
        return min <= trueLiterals && trueLiterals <= max;}

    /** tests if the clause is a disjunction
     *
     * @return true if the quantifier of the clause is a disjunction.
     */
    public boolean isDisjunction() {
        return quantifier == Quantifier.OR;}

    /**
     * Checks if the method has multiplicities.
     *
     * @return true if the expandedSize is equal to the double of the size of the literals list, false otherwise.
     */
    public boolean hasMultiplicities() {
        return 2*expandedSize != literals.size();  }

    /** simplifies the clause.
     * <br>
     * 1. removes multiplicities &gt; min <br>
     * 2. removes complementary literals <br>
     * 3. reduces to essential literals <br>
     * 4. divides by GCD <br>
     * 5. finds true literals.
     *
     * @param trackReasoning if true then inference steps are generated
     * @param monitor        null or a monitor
     * @param symboltable    null or a symboltable.
     * @return null or a conjunction (either the changed clause itself or a newly generated clause).
     */
    public Clause simplify(boolean trackReasoning, Monitor monitor, Symboltable symboltable) {
        if(isTrue || isFalse) return null;
        removeMultiplicities(trackReasoning,monitor,symboltable);                  // does not change its status.
        if(removeComplementaries(trackReasoning,monitor,symboltable)) return null; // may be a tautology
        if(quantifier == Quantifier.OR) return null;  // no further simplifications possible.
        if(reduceToEssentialLiterals(trackReasoning,monitor,symboltable))
            return (quantifier == Quantifier.AND) ? this : null;
        divideByGCD(trackReasoning,monitor,symboltable);
        return findTrueLiterals(trackReasoning,monitor,symboltable);} // clause may have become false.

    /**Removes the multiplicities &gt; min from the clause (if min &gt; 0).
     * <br>
     * Example:  [2,3] p^3,q,r,s -&gt; [2,3] p^2,q,r,s.<br>
     * Removes in particular multiple occurrences of literals in disjunctions.
     *
     * @param trackReasoning true to track the reasoning, false otherwise
     * @param monitor the monitor to print information to, can be null
     * @param symboltable the symbol table, can be null
     * @return true if the multiplicities were removed, false otherwise
     */
    public boolean removeMultiplicities(boolean trackReasoning, Monitor monitor, Symboltable symboltable) {
        assert(quantifier != Quantifier.EQUIV);
        if(min == 0 || quantifier == Quantifier.AND) return false;
        boolean doReduction = false;
        for(int i = 1; i < literals.size(); i += 2) {
            if (literals.getInt(i) > min) {doReduction = true; break;}}
        if(!doReduction) return false;

        if(trackReasoning) addInferenceStep(new NMInferenceStep("removeMultiplicities", clone()));
        String clauseBefore = monitor != null ? toString(symboltable,0) : "";
        for(int i = 1; i < literals.size(); i += 2) {
            int multiplicity = literals.getInt(i);
            if (multiplicity > min) {
                expandedSize -= multiplicity - min;
                literals.set(i,min);}}
        max = Math.min(max,expandedSize);
        ++version;
        classifyClause(trackReasoning, monitor, symboltable);
        if(monitor != null) monitor.println(monitorId,"Multiplicities removed from clause " + clauseBefore +
                ".  New clause: " + toString(symboltable,0));
        return true;}

    /** This method removes complementary literals like p,-p.
     *  Examples:  [1,4] p,q,r,-p -&gt; [0,2] q,r (tautology) <br>
     *  [1,3] p^2,q,r,-p^2 -&gt; [0,1] q,r  (two 'truth')<br>
     *  [1,3] p^2,q,r,-p -&gt; [0,2] p,q,r (one 'truth')<br>
     *
     * @param trackReasoning true if an inference step is to be added.
     * @param monitor        null or a monitor
     * @param symboltable    null or a symboltable
     * @return true if the clause is true (tautology) otherwise false
     */
    boolean removeComplementaries(boolean trackReasoning, Monitor monitor, Symboltable symboltable) {
        boolean complementaryFound = false;
        for(int i = 0; i < literals.size()-1; i+=2) {
            int literal = literals.getInt(i);
            for(int j = 0; j < i; j +=2) {
                if (literal == -literals.getInt(j)) { complementaryFound = true; break;}}
            if(complementaryFound) break;}
        if(complementaryFound) {if(quantifier == Quantifier.OR) {isTrue = true; return true;}}
        else return false;

        if(trackReasoning) addInferenceStep(new NMInferenceStep("removeComplementaries", clone()));
        String clauseBefore = (monitor != null) ? toString(symboltable,0) : "";

        for(int i = 0; i < literals.size(); i+=2) {
            int literal = literals.getInt(i);
            for(int j = 0; j < i; j +=2) {
                if (literal == -literals.getInt(j)) { // complementary found
                    int multiplicityBack = literals.getInt(i+1);
                    int multiplicityFront = literals.getInt(j+1);
                    if(multiplicityBack == multiplicityFront) {
                        literals.removeInt(i+1);literals.removeInt(i);
                        literals.removeInt(j+1);literals.removeInt(j);
                        min -= multiplicityBack; max -= multiplicityBack; expandedSize -= 2*multiplicityBack;
                        i -= 4;
                        break;}
                    if(multiplicityFront > multiplicityBack) {
                        literals.set(j+1,multiplicityFront-multiplicityBack);
                        literals.removeInt(i+1);literals.removeInt(i); // remove back
                        min -= multiplicityBack; max -= multiplicityBack; expandedSize -= 2*multiplicityBack;
                        i -= 2;
                        break;}
                    literals.set(i+1,multiplicityBack-multiplicityFront);
                    literals.removeInt(j+1);literals.removeInt(j); // remove front
                    min -= multiplicityFront; max -= multiplicityFront; expandedSize -= 2*multiplicityFront;
                    i -= 2;
                    break;}}}
        min = Math.max(0,min);
        if(min > 0) {   // min has been reduced. Reduce multiplicities to min.
            for(int i = 1; i < literals.size(); i += 2) {
                int multiplicity = literals.getInt(i);
                if(multiplicity > min) {
                    literals.set(i,min);
                    expandedSize -= multiplicity - min;}}}
        classifyClause(trackReasoning, monitor, symboltable);
        ++version;
        if(monitor != null) {
            monitor.println("Normalizer.clause","Complementary Literals in Clause " +
                    clauseBefore + " -> " + toString(symboltable,0));}
        return isTrue;}



    /** This method extracts true literals from a clause.
     * There are two ways to find true literals. <br>
     * 1. min == expanded size. Example: atleast 2 p,q (both p and q must be true).<br>
     *    This comprises also unit clauses like p (p must be true).<br>
     *    In this case the clause is just turned into a conjunction.
     * 2. In clauses like atmost 2 p^3,q,r (multiplicity(p) &gt; max) p must be false.
     *    In this case the false literals are removed and a new clause (conjunction) is built from the negated literals.
     *
     * @param trackReasoning if true then an inference step is generated
     * @param monitor        if != null then the step is monitored
     * @param symboltable    null or a symboltable.
     * @return null or a (old or new) conjunction.
     */
    private Clause findTrueLiterals(boolean trackReasoning, Monitor monitor, Symboltable symboltable) {
        if(min == expandedSize) {
            if(trackReasoning) addInferenceStep(new NMInferenceStep("findTrueLiterals",clone()));
            if(monitor != null) {
                monitor.println(monitorId,"Clause " + toString(symboltable,0) +
                    " causes all its literals to become true ");}
            ++version;
            quantifier = Quantifier.AND;
            return this;}
        if(max < expandedSize) {
            IntArrayList trueLiterals = new IntArrayList();
            for(int i = literals.size()-2; i >= 0;  i -=2) {
              if(literals.getInt(i+1) > max) {trueLiterals.add(-literals.getInt(i)); trueLiterals.add(1);}}
            if(trueLiterals.isEmpty()) return null;

            NMInferenceStep step = trackReasoning ? new NMInferenceStep("findTrueLiterals",clone()) : null;
            for(int i = literals.size()-2; i >= 0;  i -=2) {
                int multiplicity = literals.getInt(i+1);
                if(multiplicity > max) {
                    literals.removeInt(i+1);
                    literals.removeInt(i);
                    expandedSize -= multiplicity;}}
            if(literals.isEmpty()) isFalse = true;

            Clause clause = new Clause();
            clause.literals = trueLiterals;
            clause.id = id;
            clause.version = version+1;
            clause.inferenceSteps = new ArrayList<>();
            clause.quantifier = Quantifier.AND;
            if(trackReasoning) clause.addInferenceStep(step);
            return clause;}
     return null;}

    /** Add an inference step to the list of inference steps.
     *
     * @param step The inference step to be added.
     */
    private void addInferenceStep(NMInferenceStep step) {
        if(step != null) {
            if(inferenceSteps == null) inferenceSteps = new ArrayList<>();
            inferenceSteps.add(step);}}




    /** divides the limits and the multiplicities by their greatest common divisor.
     *
     * @param trackReasoning   controls generation of inference steps.
     * @param monitor          null or a monitor.
     * @param symboltable      null or a symboltable.
     * @return true if the clause is changed.
     */
    protected boolean divideByGCD(boolean trackReasoning, Monitor monitor,Symboltable symboltable) {
        if(quantifier == Quantifier.OR) return false;
        int gcd;
        if(min > 0) {
            if(max > 0) gcd = Utilities.gcd(min,max);
            else gcd = min;}
        else gcd = max;

        if(gcd == 1) return false;

        for(int i = 1; i < literals.size(); i +=2) {
            gcd = Utilities.gcd(gcd,Math.abs(literals.getInt(i))); // multiplicities
            if(gcd == 1) return false;}

        if(trackReasoning) addInferenceStep(new NMInferenceStep("divideByGCD",clone()));
        int[] clauseBefore = monitor != null ? toIntArray() : null;

        ++version;
        min /= gcd;
        max /= gcd;
        for(int i = 1; i < literals.size(); i +=2) {
            int multiplicity = literals.getInt(i) / gcd;
            expandedSize -= literals.getInt(i) - multiplicity;
            literals.set(i,multiplicity);}

        classifyClause(trackReasoning, monitor, symboltable);

        if(monitor != null) {
            monitor.println(monitorId, "Divide by GCD in Clause " +
                    arrayToString(clauseBefore, symboltable) + " => " + toString(symboltable, 0));}
        return true;}

    /** The method reduces clauses to their essential literals.
     * If the sum of the multiplicities of those literals with multiplicity &lt; min is smaller than min,
     * one of the literals with multiplicity == min must be true.
     *<br>
     * Example: &gt;= 2 p^2,q^2,r. One of p,q is sufficient to make the clause true.<br>
     * Therefore it is reduced to p,q
     *
     * @param trackReasoning true if the reasoning is to be tracked.
     * @param monitor        null or a monitor.
     * @param symboltable    null or a symboltable.
     * @return true if the clause has been changed.
     */
    boolean reduceToEssentialLiterals(boolean trackReasoning, Monitor monitor,Symboltable symboltable)  {
        if(quantifier == Quantifier.OR || quantifier == Quantifier.AND) return false;
        int remainingMultiplicity = 0;
        for(int i = 1; i < literals.size(); i +=2) {
            if(literals.getInt(i) < min) remainingMultiplicity += literals.getInt(i);}
        if(remainingMultiplicity >= min) return false;

        if(trackReasoning) {addInferenceStep(new NMInferenceStep("reduceToEssentialLiterals",clone()));

        int[] clauseBefore = (monitor != null) ? toIntArray() : null;
        for(int i = literals.size()-2; i >= 0; i -=2) {
            if(literals.getInt(i+1) < min) {
                literals.removeInt(i+1);literals.removeInt(i);}
            else literals.set(i+1,1);}
        min = 1; max = literals.size()/2;
        expandedSize = max;
        classifyClause(trackReasoning, monitor, symboltable);
        ++version;
        if(monitor != null) {
            monitor.println(monitorId, "Reduce to Essential Literals in Clause " +
                    arrayToString(clauseBefore, symboltable) + " => " + toString(symboltable, 0));}}
        return true;}


    /**Applies a true literal to the clause, simplifying it by removing the corresponding literal and updating the clause properties.
     * <br>
     * Notice that the clause can become true or false.
     *
     * @param trueLiteral The true literal to be applied to the clause.
     * @param inferenceStep The inference step associated with the application of the true literal.
     * @param trackReasoning Indicates whether reasoning steps should be tracked.
     * @param monitor The monitor used for printing information.
     * @param symboltable The symbol table used for converting literals to strings.
     * @return true if the clause was successfully simplified by applying the true literal, false otherwise.
     */
    boolean applyTrueLiteral(int trueLiteral, InferenceStep inferenceStep, boolean trackReasoning, Monitor monitor, Symboltable symboltable) {
        int[] clauseBefore = monitor != null ? toIntArray() : null;
        NMISTrueLiteral step = trackReasoning ? new NMISTrueLiteral("applyTrueLiteral", trueLiteral, inferenceStep, clone()) : null;
        boolean literalFound = false;
        for(int i = literals.size()-2; i >= 0; i -= 2) {
            int literal = literals.getInt(i);
            if(Math.abs(literal) == Math.abs(trueLiteral)) {
                int multiplicity = literals.getInt(i+1);
                literals.removeInt(i+1);
                literals.removeInt(i);
                expandedSize -= multiplicity;
                literalFound = true;
                if(literal == trueLiteral) {min -= multiplicity; max -= multiplicity;}
                break;}}
        if(!literalFound) return false; // should not happen
        min = Math.max(0,min);
        max = Math.min(max,expandedSize);
        ++version;
        classifyClause(trackReasoning, monitor, symboltable);
        if(trackReasoning) addInferenceStep(step);
        if(monitor != null) {
            String result = isTrue ? "true" : (isFalse ? "false" :toString(symboltable,0));
            monitor.println(monitorId,"Clause " + arrayToString(clauseBefore,symboltable) +
                            " simplified by true literal " + Symboltable.toString(trueLiteral,symboltable) + " to " + result);
                return true;}
        return true;}

    /** The method replaces the equivalentLiteral by the representative literal.
     *
     * @param representative     the representative literal of an equivalence class.
     * @param equivalentLiteral  the corresponding equivalent literal
     * @param inferenceStep      the inference step that caused the equivalence
     * @param trackReasoning     controls generation of inference steps
     * @param monitor            null or a monitor
     * @param symboltable        null or a symboltable
     * @return                   true if the clause was changed.
     */
    boolean replaceEquivalentLiterals(int representative, int equivalentLiteral, InferenceStep inferenceStep,
                                      boolean trackReasoning, Monitor monitor, Symboltable symboltable) {
        int[] clauseBefore = monitor != null ? toIntArray() : null;
        NMISEquivalentLiteral step = trackReasoning ? new NMISEquivalentLiteral("replaceEquivalentLiterals", representative, equivalentLiteral, inferenceStep, clone()) : null;
        boolean found = false;
        for(int i = 0; i < literals.size()-1; i += 2) {
            int literal = literals.getInt(i);
            if(Math.abs(equivalentLiteral) == Math.abs(literal)) {
                literals.set(i, (literal == equivalentLiteral) ?  representative : -representative);
                found = true;
                break;}}
        if(!found) return false;
        if(trackReasoning) addInferenceStep(step);
        if(monitor != null) {
            monitor.println(monitorId,"In clause " + arrayToString(clauseBefore,symboltable) +
                    " literal " + Symboltable.toString(equivalentLiteral,symboltable) +
                    " replaced by " +  Symboltable.toString(representative,symboltable) + " => " + toString(symboltable,0));}
        removeMultiplicities(trackReasoning,monitor,symboltable);
        removeComplementaries(trackReasoning,monitor,symboltable);
        return true;
    }


    /**
     * Checks the expanded size of the clause by summing the multiplicities of the literals.
     * If the calculated size is different from the stored expanded size, it returns the calculated size.
     * Otherwise, it returns -1.
     *
     * @return the calculated expanded size if different from the stored expanded size, -1 otherwise
     */
    public int checkExpandedSize() {
        int size = 0;
        for(int i = 1; i < literals.size(); i +=2) {
            size += literals.getInt(i);}
        if(size != expandedSize) return size;
        return -1;}


    /**
         * Converts the Clause object into an array of integers.
         * The returned array includes the id, version, quantifier, min, max, expandedSize and literal1, multiplicity1,... of the clause.
         * If the literals list is empty, the returned array will have a length of 6.
         * Otherwise, the length of the returned array will be 6 plus the size of the literals list.
         *
         * @return an array of integers representing the Clause object
         */
    public int[] toIntArray() {
        int[] clause = new int[6+literals.size()];
        clause[0] = id;
        clause[1] = version;
        clause[2] = quantifier.ordinal();
        clause[3] = min;
        clause[4] = max;
        clause[5] = expandedSize;
        for (int i = 0; i < literals.size(); i++) {
            clause[i + 6] = literals.getInt(i);}
        return clause;}

    /** turns the array-version to a string.
     *
     * @param clause      a clause as an array
     * @param symboltable null or a symboltable
     * @return the clause as a string.
     */
    public String arrayToString(int[] clause, Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append(clause[0]);
        if(clause[1] != 0) st.append(",").append(clause[1]);
        st.append(": ");
        Quantifier quantifier = Quantifier.getQuantifier(clause[2]);
        assert(quantifier != null);
        int min = clause[3];
        int max = clause[4];
        switch (quantifier) {
            case OR: break;
            case INTERVAL:st.append("[").append(min).append(",").append(max).append("] "); break;
            case ATLEAST: st.append(quantifier.abbreviation).append(min).append(" ");      break;
            case ATMOST:  st.append(quantifier.abbreviation).append(max).append(" ");      break;
            case EXACTLY: st.append(quantifier.abbreviation).append(min).append(" ");      break;}
        for(int i = 6; i < clause.length; i+=2) {
            int multiplicity = clause[i+1];
            st.append(Symboltable.toString(clause[i],symboltable));
            if(multiplicity>1) st.append("^").append(multiplicity);
            if(i < clause.length-2) st.append(",");}
        return st.toString();}

    public String arrayListToString(IntArrayList array, Symboltable symboltable) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < array.size(); i++) {
            sb.append(Symboltable.toString(array.getInt(i), symboltable));
            if (i < array.size() - 1) sb.append(",");}
        return sb.toString(); }

    public String deductions(Symboltable symboltable) {
        StringBuilder deductions = new StringBuilder();
        Clause clause = this;
        for (NMInferenceStep step : inferenceSteps) {
            deductions.append(step.toString(clause,symboltable)).append("\n");
            clause = step.clause;}
        return deductions.toString();}

    /** turns the clause into a string.
     *
     * @param symboltable null or a symboltable.
     * @param size 0 or the length of the clause number string.
     * @return a string representation of the clause.
     */
    @Override
    public String toString(Symboltable symboltable, int size) {
        StringBuilder st = new StringBuilder();
        String id = Integer.toString(this.id);
        if(version != 0) id += "."+version;
        st.append((size == 0) ? id : String.format("%"+size+"s", id)).append(": ");
        switch (quantifier) {
            case OR: break;
            case INTERVAL:st.append("[").append(min).append(",").append(max).append("] "); break;
            case ATLEAST: st.append(quantifier.abbreviation).append(min).append(" ");break;
            case ATMOST:  st.append(quantifier.abbreviation).append(max).append(" ");break;
            case EXACTLY: st.append(quantifier.abbreviation).append(min).append(" ");break;
            case AND:     st.append(quantifier.abbreviation).append(" ");break;
        }
        for(int i = 0; i < literals.size(); i+=2) {
            st.append(Symboltable.toString(literals.getInt(i),symboltable));
            int multiplicity = literals.getInt(i+1);
            if(multiplicity > 1) st.append("^").append(multiplicity);
            if(i < literals.size()-2) st.append(",");}
        return st.toString();}

}
