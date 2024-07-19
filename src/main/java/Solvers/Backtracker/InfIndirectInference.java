package Solvers.Backtracker;

import Datastructures.Clause;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This inference step describes the indirect derivation of a new true literal from an old true literal and some clauses.
 * <br>
 * Typical usages:<br>
 * - Local search from a top-level selected literal derives via some clauses a new locally true literal.<br>
 *   If the top-level selected literal turns out to be globally true then the derived literal must also be globally true.<br>
 * - If the derived literal turns out to be globally false then the top-level selected literal must also be globally false.
 * <br>
 * Notice that model-based verification of this step might be too expensive if the clauses contain too many difference predicates.
 * However, the steps which intermediately derived locally true predicates from a single clause can be verified.
 * Therefor the indirect step should be correct as well.
 */
public class InfIndirectInference extends InferenceStep {
    @Override
    public String title() {
        return "Indirect Inference with clauses";}

    @Override
    public String rule() {
        return "true(literal1) and clauses => true(literal2)";}

    /** a literal which tuerned out to be globally true */
    private int oldTrueLiteral;

    /** the inference step that caused the global truth of oldTrueLiteral */
    private InferenceStep oldStep;

    /** a derived true literal */
    private int newTrueLiteral;

    /** the clauses used to derive this literal */
    private ArrayList<int[]> usedClauses;

    /**
     * This method initializes an instance of InfClauseInference.
     *
     * @param oldTrueLiteral The literal which turned out to be globally true.
     * @param oldStep   The inference step that caused the global truth of oldTrueLiteral.
     * @param newTrueLiteral The derived true literal.
     * @param usedClauses    The clauses used to derive this literal.
     */
    public InfIndirectInference(int oldTrueLiteral, InferenceStep oldStep, int newTrueLiteral, ArrayList<Clause> usedClauses) {
        this.oldTrueLiteral = oldTrueLiteral;
        this.oldStep = oldStep;
        this.newTrueLiteral = newTrueLiteral;
        if(usedClauses != null) {
            this.usedClauses = new ArrayList<>(usedClauses.size());
            for(Clause clause : usedClauses) {
                this.usedClauses.add(clause.simpleClone());}}}

    /** adds the new inference step to the list of steps and ids
     *
     * @param steps a list for collecting the inference steps.
     * @param ids  a list for collecting the input clause ids contributed to the step.
     */
    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        super.inferenceSteps(steps,ids);
        if(oldStep != null && !steps.contains(oldStep)) steps.add(oldStep);
        if(usedClauses != null) {
            for(int[] usedClause : usedClauses) {
                int id = usedClause[0];
                if(!ids.contains(id)) ids.add(id);}}}

    /**
     * Returns a string representation of the inference step.
     *
     * @param symboltable the symboltable used to map predicate names to integers
     * @return a string representation of the inference step.
     */
    @Override
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append(title()).append(": true(").append(Symboltable.toString(oldTrueLiteral,symboltable)).append(") ");
        if(usedClauses != null) {
            st.append("and clauses\n");
            for (int[] clause : usedClauses) {
                st.append(Clause.toString(clause, symboltable)).append("\n");}}
        st.append("causes true(").append(Symboltable.toString(newTrueLiteral,symboltable)).append(")");
        return st.toString();}
}
