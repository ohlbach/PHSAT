package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Solvers.Normalizer.Clause;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.Consumer;

import static Utilities.Utilities.isTrue;

/**
 * The NMISTrueLiteral class represents an inference step where a true literal is applied to a clause.
 * It extends the NMInferenceStep class.
 */
public class InfTrueLiteralToClause extends NMInferenceStep{

    /** a true literal */
    int trueLiteral;

    /** the clause after application of the true literal (not yet simplified) */
    int[] clauseAfter;

    /** The inference step that caused the literal to be true. */
    InferenceStep inferenceStep;

    /** Creates a new instance of NMISTrueLiteral.
     *
     * @param trueLiteral the true literal.
     * @param inferenceStep that cause the truth of the literal.
     * @param clauseBefore the clause to which the true literal is applied.
     * @param clauseAfter the clause after the application of the true literal.
     */
    public InfTrueLiteralToClause(int trueLiteral, InferenceStep inferenceStep, int[] clauseBefore, Clause clauseAfter) {
        super("True Literal Applied To Clause ", clauseBefore);
        this.trueLiteral = trueLiteral;
        this.inferenceStep = inferenceStep;
        this.clauseAfter = clauseAfter.simpleClone();}

    public String rule() {
        return title() +": clauseBefore and true(literal) -> clauseAfter.\n"+
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
        IntArrayList predicates = Datastructures.Clause.predicates(clauseBefore);
        int nModels = 1 << predicates.size();
        for(int model = 0; model < nModels; ++model) {
            if(Datastructures.Clause.isTrue(clauseBefore, model) && isTrue(model,trueLiteral,predicates)) {
                if(!Datastructures.Clause.isTrue(clauseAfter, model)) {
                    monitor.accept(title()+ ": Clause " + Clause.toString(clauseAfter,symboltable) +
                            " is not true in model " + Clause.modelString(model,predicates,symboltable) +
                            " of clause " +  Clause.toString(clauseBefore,symboltable) + " and literal " +
                            Symboltable.toString(trueLiteral,symboltable));
                    return false;}}}
        return true;}

    /**
     * Collects the inference steps culminating in this in the list steps. Double occurrences are to be avoided.
     * Collects the inputClause ids of all clauses causing the current inference.
     *
     * @param steps a list for collecting the inference steps
     * @param ids   a list for collecting the inputClause ids
     */
    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        super.inferenceSteps(steps, ids);
        if(inferenceStep != null && !steps.contains(inferenceStep)) steps.add(inferenceStep);
        int id = clauseBefore[0];
        if(!ids.contains(id)) ids.add(id);
    }



    }
