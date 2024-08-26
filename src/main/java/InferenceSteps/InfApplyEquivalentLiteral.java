package InferenceSteps;

import Datastructures.Clause;
import Datastructures.Symboltable;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.Consumer;

/**
 * The InfApplyEquivalentLiteral class represents an inference step that applies the equivalence rule to replace a
 * literal in a clause with a new literal.
 */
public class InfApplyEquivalentLiteral extends InferenceStep {
    /** the new literal which replaces the old literal */
    int newLiteral;
    /** the old literal which is to be replaced by the new literal */
    int oldLiteral;
    /** the clause before the replacement */
    int[] clauseBefore;
    /** the clause right after the replacement (before simplifiction) */
    int[] clauseAfter;
    /** the inference step which caused the equivalence (usually an inputClause) */
    InferenceStep equivalenceStep;
    /** the original inference steps of the clause */
    ArrayList<InferenceStep> inferenceSteps;
    /**
     * Constructs a new InfApplyEquivalentLiteral object.
     *
     * @param newLiteral        the new literal which replaces the old literal
     * @param oldLiteral        the old literal which is to be replaced by the new literal
     * @param equivalenceStep   the inference step which caused the equivalence
     * @param clauseBefore      the clause before the replacement
     * @param clauseAfter       the clause right after the replacement (before simplification)
     */
    public InfApplyEquivalentLiteral(int newLiteral, int oldLiteral,
                                     InferenceStep equivalenceStep, int[] clauseBefore, Clause clauseAfter, String reasoner) {
        super(reasoner);
        this.newLiteral      = newLiteral;
        this.oldLiteral      = oldLiteral;
        this.equivalenceStep = equivalenceStep;
        this.clauseBefore    = clauseBefore;
        this.clauseAfter     = clauseAfter.simpleClone();
        inferenceSteps       = clauseAfter.inferenceSteps;}

    /**
     * Returns a string representation of the object. The string representation consists of the title
     * of the object, followed by the clause before the replacement, the old literal replaced by
     * the new literal, and the clause after the replacement.
     *
     * @param symboltable the symbol table for mapping predicate names to integers
     * @return a string representation of the object
     */
    public String toString(Symboltable symboltable) {
        return title() + ": " + Clause.toString(clauseBefore, symboltable) + " and literal " +
                Symboltable.toString(oldLiteral,symboltable) +
                " replaced by literal " + Symboltable.toString(newLiteral,symboltable) +
                " => " + Clause.toString(clauseAfter, symboltable);}

    /**
     * Verifies the rule.
     *
     * @param monitor      the consumer for monitoring the verification process
     * @param symboltable  the symbol table for mapping predicate names to integers
     * @return true if the verification is successful, false otherwise
     */
    @Override
    public boolean verify(Consumer<String> monitor, Symboltable symboltable) {
        IntArrayList predicates = Clause.predicates(clauseBefore);
        if(!predicates.contains(oldLiteral)) predicates.add(oldLiteral);
        if(!predicates.contains((newLiteral))) predicates.add(newLiteral);
        int nModels = 1 << predicates.size();
        for (int model = 0; model < nModels; model++) {
            if (Clause.isTrue(clauseBefore, model, predicates)
                && Utilities.isTrue(model,oldLiteral,predicates) == Utilities.isTrue(model,newLiteral,predicates)) {
                if(!Clause.isTrue(clauseAfter,model,predicates)) {
                    monitor.accept( "Error:\n"+ toString(symboltable)+"\nVerification failed for model "+
                            Clause.modelString(model, predicates, symboltable));
                    return false;}}}
        return true;}

    /**
     * Retrieves the title of the method.
     *
     * @return The title of the method, which is "Equivalence Replacement".
     */
    @Override
    public String title() {
        return "Equivalence Replacement";}

    /**
     * Returns the rule for the InferenceStep.
     *
     * @return The rule for the InferenceStep.
     */
    @Override
    public String rule() {
        return "Clause and new == old -> Clause/[old->new]"+
                "Example: p,-q,r and s == q -> p,-s,r";}

    /** collects the inference steps culminating in this in the list steps
     * Double occurrences are to be avoided.
     *  collects the inputClause ids of all clauses causing the current inference
     *
     * @param steps a list for collecting the inference steps.
     * @param ids   a list for collecting the inputClause ids
     * @param reasoners the list of reasoners
     */
    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids, ArrayList<String> reasoners) {
        if(steps.contains(this)) return;
        super.inferenceSteps(steps,ids,reasoners);
        if(!ids.contains(clauseBefore[0])) ids.add(clauseBefore[0]);
        if(!steps.contains(equivalenceStep)) {steps.add(equivalenceStep);}
        if(inferenceSteps != null) {
            for(InferenceStep step : inferenceSteps) {step.inferenceSteps(steps,ids,reasoners);}}}

}
