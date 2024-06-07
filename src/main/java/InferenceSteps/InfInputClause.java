package InferenceSteps;

import Datastructures.Clause;
import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import java.util.ArrayList;
import java.util.function.Consumer;

/** This not really an inference step.
 * Instead of this it documents that the clause comes from some input clauses.
 */
public class InfInputClause extends InferenceStep {

    /** the rul name */
    public static String title = "Input";

    /** the rule itself */
    public static String rule = "Input";

    /** the original input clause */
    public int[] inputClause;

    /** the clause as a simple clone. */
    public int[] clause;

    /**
     * Represents an inference step in which the clause is derived from input clauses.
     * <br>
     * The clause is turned into a simpleClone because it may be further simplified later on.
     *
     * @param inputClause the original input clause
     * @param clause either null or the transformed clause.
     */
    public InfInputClause(int[] inputClause, Clause clause) {
        this.inputClause = inputClause;
        if(clause != null) this.clause = clause.simpleClone();}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    /**
     * Verifies the transformation of InputClause to clause.
     *
     * @param monitor      the consumer for printing error messages
     * @param symboltable  the symbol table for mapping predicate names to integers
     * @return true if the verification is successful, false otherwise
     */
    public boolean verify(Consumer<String> monitor, Symboltable symboltable) {
        if(clause == null) return true;
        IntArrayList predicates = InputClauses.predicates(inputClause);
        int models = 1 << predicates.size();
        for(int model = 0; model < models; ++model) {
            if(InputClauses.isTrue(inputClause, model,predicates)) {
                int fModel = model; // final
                if(!Clause.isTrue(clause, (literal -> {
                    int position = predicates.indexOf(Math.abs(literal));
                    return ((literal > 0) ? ((fModel & (1 << position)) != 0) : ((fModel & (1 << position)) == 0));}))) {
                   monitor.accept("Verification failed for transformation of input clause: " +
                           InputClauses.toString(0,inputClause, symboltable) + " to " +
                           Clause.toString(clause,symboltable));
                return false;}}}
        return true;}


    @Override
    public String toString(Symboltable symboltable) {
        String cl = (clause != null) ? " -> " + Clause.toString(clause,symboltable) : "";
        return "Input: Clause " + InputClauses.toString(0,inputClause,symboltable) + cl;}


    /**
     * Collects the inference steps culminating in this in the list steps. Double occurrences are to be avoided.
     * Collects the inputClause ids of all clauses causing the current inference.
     *
     * @param steps a list for collecting the inference steps
     * @param ids   a list for collecting the inputClause ids
     */
    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        super.inferenceSteps(steps,ids);
        int id = inputClause[0];
        if(!ids.contains(id)) ids.add(id);}
}
