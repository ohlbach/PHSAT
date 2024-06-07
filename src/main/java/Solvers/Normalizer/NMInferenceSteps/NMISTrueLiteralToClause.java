package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Solvers.Normalizer.Clause;
import it.unimi.dsi.fastutil.ints.IntArrayList;

/**
 * The NMISTrueLiteral class represents an inference step where a true literal is applied to a clause.
 * It extends the NMInferenceStep class.
 */
public class NMISTrueLiteralToClause extends NMInferenceStep{

    /** a true literal */
    int trueLiteral;
    /** The inference step that caused the literal to be true. */
    InferenceStep inferenceStep;

    /**Creates a new instance of NMISTrueLiteral.
     *
     * @param title the title of the inference step
     * @param trueLiteral the true literal
     * @param clause the clause to which the true literal is applied
     */
    public NMISTrueLiteralToClause(String title, int trueLiteral, int[] clause) {
        super(title, clause);
        this.trueLiteral = trueLiteral;}

    /** Returns a string representation of the deduced clause and the steps involved in the inference.
     *
     * @param deducedClause The deduced clause in the inference step.
     * @param symboltable The symbol table to use for converting predicates to strings, or null.
     * @return A string representation of the deduced clause and the steps involved in the inference.
     */
    public String toString(Clause deducedClause, Symboltable symboltable) {
        return title() + ": " + Clause.toString(clause, symboltable) + " and true(" +
                Symboltable.toString(trueLiteral,symboltable) +") => " + deducedClause.toString(symboltable,0);}

    /** Checks if a given literal is true in the model represented by the bits in the integer model.
     *
     * @param model          the bits represent a model of the predicates in the predicates array.
     * @param literal    a literal to be tested.
     * @param literals   a list of predicates.
     * @return true if the literal is true in the model, false otherwise.
     */
    public boolean isTrue(int model, int literal, IntArrayList literals) {
        if(literal ==  trueLiteral) return true;
        if(literal == -trueLiteral) return false;
        return super.isTrue(model,literal,literals);}

}
