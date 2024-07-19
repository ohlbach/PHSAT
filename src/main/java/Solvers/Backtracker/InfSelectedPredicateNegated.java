package Solvers.Backtracker;

import Datastructures.Clause;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This inference step describes the situation that a top-level selected predicate in the
 * backtracking search eventually causes a contradiction (usually a locally false clause).
 * The top-level selected predicate must therefor be globally false.
 * <br>
 * Notice that model-based verification of this step might be too expensive if the clauses contain too many difference predicates.
 * However, the steps which intermediately derived locally true predicates from a single clause can be verified.
 * Therefor the indirect step should be correct as well.
 */
public class InfSelectedPredicateNegated  extends InferenceStep {
    /**
     * Returns the title of the inference step
     *
     * @return the title of the inference step as a String
     */
    @Override
    public String title() {
        return "Negated Selected Predicate";}

    /**
     * Returns a string representation of the rule.
     *
     * @return The string representation of the rule.
     */
    @Override
    public String rule() {
        return "(selected predicate -> contradiction) => false(selected predicate)";}

    /** the negated top-level selected predicate which caused a contradiction */
    private int negatedPredicate;

    /** the clauses used to derive the contradiction */
    private ArrayList<int[]> usedClauses;

    /** creates the inference step which describes the situation that a top-level selected predicate in the
     * backtracking search eventually causes a contradiction (usually a locally false clause).
     *
     * @param negatedPredicate the negated top-level selected predicate which caused a contradiction
     * @param usedClauses the clauses used to derive the contradiction
     */
    public InfSelectedPredicateNegated(int negatedPredicate, ArrayList<Clause> usedClauses) {
        this.negatedPredicate = negatedPredicate;
        this.usedClauses = new ArrayList<>(usedClauses.size());
        for(Clause clause : usedClauses) { this.usedClauses.add(clause.simpleClone());}}

    /**
     * Returns a string representation of the inference step.
     *
     * @param symboltable the symbol table used to convert integers to symbolic names
     * @return a string representation of the inference step.
     */
    @Override
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append(title()).append("\n");
        for(int[] clause : usedClauses) st.append(Clause.toString(clause,symboltable)).append("\n");
        st.append(" -> ").append(Symboltable.toString(negatedPredicate,symboltable));
        return st.toString();}

    /** adds the new inference step to the list of steps and ids
     *
     * @param steps a list for collecting the inference steps.
     * @param ids  a list for collecting the input clause ids contributed to the step.
     */
    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        super.inferenceSteps(steps,ids);
        if(usedClauses != null) {
            for(int[] usedClause : usedClauses) {
                int id = usedClause[0];
                if(!ids.contains(id)) ids.add(id);}}}
}
