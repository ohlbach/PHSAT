package Solvers.Normalizer;

import Datastructures.Clauses.Quantifier;
import Datastructures.LinkedItem;
import Datastructures.Symboltable;
import InferenceSteps.InfRemoveComplementaries;
import InferenceSteps.InferenceStep;
import Management.Monitor.Monitor;
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

    ArrayList<InferenceStep> inferenceSteps;

    /** Creates a Clause object from the given input clause.
     * The constructor does not apply to AND and EQUIV clauses.
     * Multiple occurrences of literals are comprised into one occurrence with corresponding multiplicities.
     * No simplifications are done in the constructor.
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
            case OR: min = 1; max = expandedSize; break;
            case ATLEAST:  min = inputClause[2]; max = expandedSize; break;
            case ATMOST:   min = 0; max = inputClause[2]; break;
            case EXACTLY:  min = inputClause[2]; max = min; break;
            case INTERVAL: min = inputClause[2]; max = inputClause[3]; break;}
        literals = new IntArrayList(2*expandedSize);
        for (int i = firstLiteralIndex; i < inputClause.length; i++) {
            int literal = inputClause[i];
            boolean multiplicit = false;
            for(int j = 0; j < literals.size(); j += 2) {
                if(literal == literals.getInt(j)) {literals.set(j+1,literals.get(j+1)+1); multiplicit = true; break;}}
            if(multiplicit) {hasMultiplicities = true; continue;}
            literals.add(literal); literals.add(1);}}

    /** This method removes complementary literals like p,-p.
     *  Examples:  [1,4] p,q,r,-p -&gt; [0,2] q,r (tautology) <br>
     *  [1,3] p^2,q,r,-p^2 -&gt; [0,1] q,r  (two 'truth')<br>
     *  [1,3] p^2,q,r,-p -&gt; [1,2] p,q,r (one 'truth')<br>
     *  The limits and the resulting multiplicities are not checked.
     *
     * @param trackReasoning true if an inference step is to be added.
     * @param monitor        null or a monitor
     * @param symboltable    null or a symboltable
     * @return
     */
    void removeComplementaries(boolean trackReasoning, Monitor monitor, Symboltable symboltable) {
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
            inferenceSteps.add(new InfRemoveComplementaries(previousClause, toIntArray()));}}






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
