package Solvers.Normalizer;

import Datastructures.Clauses.Quantifier;
import Datastructures.LinkedItem;
import Datastructures.Results.UnsatClause;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import InferenceSteps.InfRemoveComplementaries;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
import Solvers.Normalizer.NMInferenceSteps.NMISAndConversion;
import Solvers.Normalizer.NMInferenceSteps.NMISdivideByGCD;
import Utilities.BiConsumerWithUnsatisfiable;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class Clause extends LinkedItem {
    /**The original identifier of the input clause. */
    public int id;
    /** indicates simplified versions of the clause. The version of the original clause is 0.*/
    public int version = 0;

    /** the quantifier, but not AND or EQUIV */
    public Quantifier quantifier;

    /** the lower limit for the quantification*/
    public int min;

    /** the upper limit for the quantification */
    public int max;

    /** literal_1,multiplicity_1,literal2,multiplicity_2, ... */
    public IntArrayList literals;

    /** the original input clause */
    public int[] inputClause;

    /** the number of the literals, multiplied by their multiplicities */
    public int expandedSize = 0;

    /** true if at least one literal occurs more than once */
    public boolean hasMultiplicities;

    private String monitorId = "Normalizer.clause";

    ArrayList<InferenceStep> inferenceSteps;

    /** Creates a Clause object from the given input clause.
     * The constructor does not apply to AND and EQUIV clauses.
     * Multiple occurrences of literals are comprised into one occurrence with corresponding multiplicities.<br>
     * The only simplifications are done or OR- or ATLEAST-clauses:
     * Multiplicities are limited to min. <br>
     * Example: atleast 2 p,p,p q -&gt; atleast 2 p^2,q  (This should not happen in real examples)<br>
     * A clause with quantifier OR and just a single literal gets its quantifier changed to AND.
     *
     * @param inputClause the input clause.
     */
    public Clause(int[] inputClause) {
        id = inputClause[0];
        quantifier = Quantifier.getQuantifier(inputClause[1]);
        assert(quantifier != Quantifier.EQUIV && quantifier != Quantifier.AND);
        int firstLiteralIndex = quantifier.firstLiteralIndex;
        expandedSize = inputClause.length-firstLiteralIndex;
        switch(quantifier) {
            case OR: min = 1;                    max = expandedSize; break;
            case ATLEAST:  min = inputClause[2]; max = expandedSize; break;
            case ATMOST:   min = 0;              max = inputClause[2]; break;
            case EXACTLY:  min = inputClause[2]; max = min; break;
            case INTERVAL: min = inputClause[2]; max = inputClause[3]; break;}
        literals = new IntArrayList(2*expandedSize);
        for (int i = firstLiteralIndex; i < inputClause.length; i++) {
            int literal = inputClause[i];
            boolean multiplicit = false;
            for(int j = 0; j < literals.size(); j += 2) {
                if(literal == literals.getInt(j)) {
                    multiplicit = true;
                    int multiplicity = literals.getInt(j+1);
                    if((quantifier == Quantifier.OR || quantifier == Quantifier.ATLEAST) && multiplicity == min) {
                        --expandedSize; --max; break;}
                    literals.set(j+1,multiplicity+1); break;}}
            if(multiplicit) {hasMultiplicities = true; continue;}
            literals.add(literal); literals.add(1);}
        if(quantifier == Quantifier.OR && expandedSize == 1) quantifier = Quantifier.AND;}

    private Clause() {}

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

    public boolean isTrue() {
        return min <= 0 && max == expandedSize;}

    /** tests if the clause is a disjunction
     *
     * @return true if the quantifier of the clause is a disjunction.
     */
    public boolean isDisjunction() {
        return quantifier == Quantifier.OR;}

    /** This method removes complementary literals like p,-p.
     *  Examples:  [1,4] p,q,r,-p -&gt; [0,2] q,r (tautology) <br>
     *  [1,3] p^2,q,r,-p^2 -&gt; [0,1] q,r  (two 'truth')<br>
     *  [1,3] p^2,q,r,-p -&gt; [1,2] p,q,r (one 'truth')<br>
     *  The limits and the resulting multiplicities are not checked.
     *
     * @param trackReasoning true if an inference step is to be added.
     * @param monitor        null or a monitor
     * @param symboltable    null or a symboltable
     * @return true if the clause is true (tautology)
     */
    boolean removeComplementaries(boolean trackReasoning, Monitor monitor, Symboltable symboltable) {
        int[] previousClause = null;
        for(int i = 0; i < literals.size(); i+=2) {
            int literal = literals.getInt(i);
            for(int j = 0; j < i; j +=2) {
                if (literal == -literals.getInt(j)) { // complementary found
                    if((trackReasoning || monitor != null) && previousClause == null) previousClause = toIntArray();
                    int multiplicity1 = literals.getInt(i+1);
                    int multiplicity2 = literals.getInt(j+1);
                    if(multiplicity1 == multiplicity2) {
                        literals.removeInt(i+1);literals.removeInt(i);
                        literals.removeInt(j+1);literals.removeInt(j);
                        min -= multiplicity1; max -= multiplicity1; expandedSize -= 2*multiplicity1;
                        i -= 4;
                        break;}
                    if(multiplicity1 < multiplicity2) {
                        literals.set(j+1,multiplicity2-multiplicity1);
                        literals.removeInt(i+1);literals.removeInt(i);
                        min -= multiplicity1; max -= multiplicity1; expandedSize -= multiplicity1;
                        i -= 2;
                        break;}
                    if(multiplicity1 > multiplicity2) {
                        literals.set(i+1,multiplicity1-multiplicity2);
                        literals.removeInt(j+1);literals.removeInt(j);
                        min -= multiplicity2; max -= multiplicity2; expandedSize -= multiplicity2;
                        break;}}}}
        min = Math.max(0,min);
        if(monitor != null) {
            monitor.println("Normalizer.clause","Complementary Literals in Clause " +
                    arrayToString(previousClause,symboltable) + " -> " + toString(symboltable,0));}
        if(trackReasoning) {
            if(inferenceSteps == null) inferenceSteps = new ArrayList<>(2);
            inferenceSteps.add(new InfRemoveComplementaries(previousClause, toIntArray()));}
        return isTrue();}


    /**
     *
     * @param model
     * @param trackReasoning
     * @param monitor
     * @param symboltable
     * @return true if the clause is still useful.
     * @throws Unsatisfiable
     */
    boolean reduceNumbers(BiConsumerWithUnsatisfiable<Integer,InferenceStep> model, boolean trackReasoning,
                          Monitor monitor, Symboltable symboltable) throws Unsatisfiable {
        if(expandedSize <= 0 || min > max || min > expandedSize) {
            throw new UnsatClause(null,null,inputClause);} // anpassen
        if(min == 0 && max == expandedSize) {
            if(monitor != null) monitor.println("Normalizer.clause", "Clause " + toString(symboltable,0) + " is true");
            return false;}

        InferenceStep step = null;  // anpassen
        IntArrayList falseLiterals = null;
        int[] clauseBefore;
        for(int i = 0; i < literals.size(); i += 2) {
            int multiplicity = literals.getInt(i+1); // multiplicities > min can be reduced to min.
            if(multiplicity > min) {expandedSize -= multiplicity-min; literals.set(i,min); continue;}
            if(multiplicity > max) { // literals with multiplicities > max must be false.
                if(falseLiterals == null) falseLiterals = new IntArrayList(1);
                falseLiterals.add(i);}}

        if(falseLiterals != null) {
            clauseBefore = toIntArray();
            for(int index = falseLiterals.size()-1; index >= 0; index -= 2) {
                int literal = literals.get(index);
                falseLiterals.set(index,literal);
                literals.removeInt(index+1);
                literals.removeInt(index);
                model.accept(-literal,step);}
            if(monitor != null) {
                monitor.println(monitorId,"Clause " + arrayToString(clauseBefore,symboltable) +
                        " causes the following false literals: " + arrayListToString(falseLiterals,symboltable) +
                        " new clause: " + toString(symboltable,0));}}

        if(min > expandedSize) throw new UnsatClause(null,null,inputClause);
        andConversion(trackReasoning,monitor,symboltable);

        if(min == 0) { // atmost-clause turned into atleast-clause
            clauseBefore = toIntArray();
            int minOld = min;
            min = expandedSize-max; max = expandedSize-minOld;
            for(int i = 0; i < literals.size(); i += 2) {literals.set(i,-literals.getInt(i));}
            quantifier = Quantifier.ATLEAST;
            if(monitor != null) {
                monitor.println(monitorId,"Clause " + arrayToString(clauseBefore,symboltable) +
                        " is turned into an atleast-clause " + toString(symboltable,0));}}

        if(quantifier != quantifier.OR) reduceToEssentialLiterals(trackReasoning,monitor,symboltable);
        if(divideByGCD(symboltable,trackReasoning,monitor)) {
            andConversion(trackReasoning,monitor,symboltable);}
        if(min == 1 && max == expandedSize) quantifier = Quantifier.OR;
        if(min == max) quantifier = Quantifier.EXACTLY;

        return true;
    }

    /** A clause like [3,3] p,q,r  (where min = expandedSize) can only be true if all literals are true.
     * If this is the case then the clause is turned into a conjunction.<br>
     * This happens in particular if the clause is a unit clause.<br>
     * Notice that the multiplicities are not changed. They are irrelevant.
     *
     * @param trackReasoning if true then an inference step is generated
     * @param monitor        if != null then the step is monitored
     * @param symboltable    null or a symboltable.
     * @return true if the clause has become a conjunction.
     */
    private boolean andConversion(boolean trackReasoning, Monitor monitor, Symboltable symboltable) {
     if(min == expandedSize) {
         if(trackReasoning) addInferenceStep(new NMISAndConversion(clone()));
        if(monitor != null) {
            monitor.println(monitorId,"Clause " + toString(symboltable,0) +
                    " causes all its literals to become true ");}
         ++version;
         quantifier = Quantifier.AND;
        return true;}
     return false;}

    /** Add an inference step to the list of inference steps.
     *
     * @param step The inference step to be added.
     */
    private void addInferenceStep(InferenceStep step) {
        if(step != null) {
            if(inferenceSteps == null) inferenceSteps = new ArrayList<>();
            inferenceSteps.add(step);}}

    boolean applyTrueLiteral(int literal, boolean isTrue, InferenceStep inferenceStep,boolean trackReasoning, Monitor monitor, Symboltable symboltable) {
        return true;
    }

    void applyTrueLiterals(Model model, boolean trackReasoning, Monitor monitor, Symboltable symboltable) {
        int[] clauseBefore = (trackReasoning || monitor != null) ? toIntArray() : null;
        boolean simplified = false;
        for(int i = literals.size()-2; i >= 0; i -= 2) {
            int literal = literals.getInt(i);
            switch (model.status(literal)) {
                case 1:
                    expandedSize -= literals.getInt(i+1);
                case -1:
                    simplified = true;
                    literals.removeInt(i+1);
                    literals.removeInt(i);}}
        if(simplified) {
            if(monitor != null) {
                monitor.println(monitorId,"Clause " + arrayToString(clauseBefore,symboltable) +
                        " simplified by true literals to " + toString(symboltable,0));}
            if(trackReasoning) {
                // to be done
            }}}


    /** divides the limits and the multiplicities by their greatest common divisor.
     *
     * @return true if the clause is changed.
     */
    protected boolean divideByGCD(Symboltable symboltable, boolean trackReasoning, Monitor monitor) {
        int gcd;
        if(min > 0) {
            if(max > 0) gcd = Utilities.gcd(min,max);
            else gcd = min;}
        else gcd = max;

        if(gcd == 1) return false;

        for(int i = 1; i < literals.size(); i +=2) {
            gcd = Utilities.gcd(gcd,Math.abs(literals.getInt(i)));
            if(gcd == 1) return false;}

        if(trackReasoning) addInferenceStep(new NMISdivideByGCD(clone(),gcd));
        int[] clauseBefore = monitor != null ? toIntArray() : null;

        min /= gcd;
        max /= gcd;
        for(int i = 1; i < literals.size(); i +=2) {
            int multiplicity = literals.getInt(i) / gcd;
            expandedSize -= literals.getInt(i) - multiplicity;
            literals.set(i,multiplicity);}

        if(monitor != null) {
            monitor.println(monitorId, "Divide by GCD in Clause " +
                    arrayToString(clauseBefore, symboltable) + " -> " + toString(symboltable, 0));}
        return true;}

    /** The method reduces clauses to their essential literals.
     *
     * Example: &gt;= 2 p^2,q^2,r. One of p,q is sufficient to make the clause true.<br>
     * Therefore it is reduced to p,q
     *
     * @param trackReasoning
     * @param monitor
     * @param symboltable
     * @return
     */
    boolean reduceToEssentialLiterals(boolean trackReasoning, Monitor monitor,Symboltable symboltable)  {
        int remainingMultiplicity = 0;
        for(int i = 1; i < literals.size(); i +=2) {
            if(literals.getInt(i) < min) remainingMultiplicity += literals.getInt(i);}
        if(remainingMultiplicity > min) return false;
        int[] clauseBefore = (trackReasoning || monitor != null) ? toIntArray() : null;
        for(int i = literals.size()-1; i >= 0; i -=2) {
            if(literals.getInt(i+1) < min) {
                literals.removeInt(i+1);literals.removeInt(i);}
            else literals.set(i+1,1);}
        min = 1; max = literals.size()/2;
        expandedSize = max;
        quantifier = Quantifier.OR;
        if(monitor != null) {
            monitor.println(monitorId, "Reduce to Essential Literals in Clause " +
                    arrayToString(clauseBefore, symboltable) + " -> " + toString(symboltable, 0));}
        if(trackReasoning) {} // tp be done
        return true;}

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
     * Determines if the literals of two clauses are equal.
     *
     * @param clause The clause to compare with.
     * @param symboltable null or the symboltable to convert literals to strings.
     * @param errorPrefix The prefix to include in error messages.
     * @param errors A StringBuilder to store error messages.
     * @return {@code true} if the literals of the clauses are equal, {@code false} otherwise.
     */
    public boolean literalsAreEqual(Clause clause, Symboltable symboltable,  String errorPrefix, StringBuilder errors) {
        if(clause.literals.size() != literals.size()) {
            errors.append(errorPrefix + ": literals of clause " + toString(symboltable,0) +" and " +
                        clause.toString(symboltable,0) + " have different size.\n");
            return false;}
        boolean result = true;
        for(int i = 0; i < literals.size()-1; i +=2) {
            if(literals.getInt(i) != clause.literals.getInt(i)) {
                errors.append(errorPrefix + ": literals of clause " + toString(symboltable, 0) + " and " +
                        clause.toString(symboltable, 0) + " are different at position " + i+"\n");
                result = false;}}
        return result;}

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
        if(clause[1] != 0) st.append(","+clause[1]);
        st.append(": ");
        Quantifier quantifier = Quantifier.getQuantifier(clause[2]);
        int min = clause[3];
        int max = clause[4];
        switch (Quantifier.getQuantifier(clause[2])) {
            case OR: break;
            case INTERVAL:st.append("[").append(min).append(",").append(max).append("] "); break;
            case ATLEAST: st.append(quantifier.abbreviation).append(min).append(" ");break;
            case ATMOST:  st.append(quantifier.abbreviation).append(max).append(" ");break;
            case EXACTLY: st.append(quantifier.abbreviation).append(min).append(" ");break;}
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
            case EXACTLY: st.append(quantifier.abbreviation).append(min).append(" ");break;}
        for(int i = 0; i < literals.size(); i+=2) {
            st.append(Symboltable.toString(literals.get(i),symboltable));
            int multiplicity = literals.get(i+1);
            if(multiplicity > 1) st.append("^"+multiplicity);
            if(i < literals.size()-2) st.append(",");}
        return st.toString();}

}
