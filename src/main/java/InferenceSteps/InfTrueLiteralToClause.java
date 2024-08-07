package InferenceSteps;

import Datastructures.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.Consumer;

import static Utilities.Utilities.isTrue;

/**
 * The class represents an inference step where a true literal is applied to a clause.
 * It extends the NMInferenceStep class.
 */
public class InfTrueLiteralToClause extends InferenceStep {

    /** a true literal */
    int trueLiteral;

    /** the clause before application of the true literal (not yet simplified) */
    int[] clauseBefore;

    /** the clause after application of the true literal (not yet simplified) */
    int[] clauseAfter;

    /** the original inference steps of the clause */
    ArrayList<InferenceStep> inferenceSteps;

    /** The inference step that caused the literal to be true. */
    InferenceStep trueLiteralStep;

    /** Creates a new instance of NMISTrueLiteral.
     *
     * @param trueLiteral the true literal.
     * @param trueLiteralStep that cause the truth of the literal.
     * @param clauseBefore the clause to which the true literal is applied.
     * @param clauseAfter the clause after the application of the true literal.
     */
    public InfTrueLiteralToClause(int trueLiteral, InferenceStep trueLiteralStep, int[] clauseBefore, Clause clauseAfter) {
        this.trueLiteral   = trueLiteral;
        this.trueLiteralStep = trueLiteralStep;
        this.clauseBefore  = clauseBefore;
        this.clauseAfter   = clauseAfter.simpleClone();
        inferenceSteps = clauseAfter.inferenceSteps;}

    /**
     * Returns the title of the method.
     *
     * @return The title of the method.
     */
    @Override
    public String title() {
        return "True Literal Applied To Clause";}

    /**
     * Returns the rule for applying a true literal to a clause.
     *
     * @return The rule for applying a true literal to a clause.
     */
    public String rule() {
        return title() +": clauseBefore and true(literal) -> clauseAfter. "+
               "Example: p,q,r and true(-q) -> p,r";}

    /** Returns a string representation of the deduced clause and the steps involved in the inference.
     *
     * @param symboltable The symbol table to use for converting predicates to strings, or null.
     * @return A string representation of the deduced clause and the steps involved in the inference.
     */
    public String toString(Symboltable symboltable) {
        return title() + ": " + Clause.toString(clauseBefore, symboltable) + " and true(" +
                Symboltable.toString(trueLiteral,symboltable) +") => " + Clause.toString(clauseAfter, symboltable);}


    /** verifies the application of a true literal to a clause.
     *
     * @param monitor      the consumer for monitoring the verification process
     * @param symboltable  the symbol table for mapping predicate names to integers
     * @return true if the verification is successful.
     */
    @Override
    public boolean verify(Consumer<String> monitor, Symboltable symboltable) {
        IntArrayList predicates = Clause.predicates(clauseBefore);
        int nModels = 1 << predicates.size();
        for(int model = 0; model < nModels; ++model) {
            if(Datastructures.Clause.isTrue(clauseBefore, model) && isTrue(model,trueLiteral,predicates)) {
                if(!Datastructures.Clause.isTrue(clauseAfter, model,predicates)) {
                    monitor.accept("Error:\n" + toString(symboltable) + "\nVerification failed for model "+
                            Clause.modelString(model,predicates,symboltable));
                    return false;}}}
        return true;}

    /**
     * Collects the inference steps culminating in this in the list steps. Double occurrences are to be avoided.
     * Collects the inputClause ids of all clauses causing the current inference.
     *
     * @param steps a list for collecting the inference steps
     * @param ids   a list for collecting the inputClause ids
     * @param reasoners the list of reasoners
     */
    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids, ArrayList<String> reasoners) {
        if(steps.contains(this)) return;
        if(trueLiteralStep != null && !steps.contains(trueLiteralStep)) steps.add(trueLiteralStep);
        super.inferenceSteps(steps, ids, reasoners);
        int id = clauseBefore[0];
        if(!ids.contains(id)) ids.add(id);
        if(inferenceSteps != null) {
            for(InferenceStep step : inferenceSteps) {step.inferenceSteps(steps,ids,reasoners);}}
    }



    }
