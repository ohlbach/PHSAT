package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Solvers.Normalizer.Clause;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/**
 * NMInferenceStep class represents the top class of the inference steps in the Normalize package.
 */
public class NMInferenceStep extends InferenceStep {

    /** the title of the inference rule */
    public String title;

    /** the original clause of the inference */
    public int[] clause;

    /** returns the title of the inference rule */
    @Override
    public String title() {
        return title;}

    /** Creates the inference step with title and clause
     *
     * @param title   the title of the inference rule
     * @param clause  the original clause to which the inference rule is applied.
     */
    public NMInferenceStep(String title, int[] clause) {
        this.title = title;
        this.clause = clause;}

    /** turns the inference step into a string: title original clause =&gt; deduced clause.
     *
     *
     * @param deducedClause The deduced clause in the inference step.
     * @param symboltable The symbol table to use for converting predicates to strings, or null.
     * @return The string representation of the inference step.
     */
    public String toString(Clause deducedClause, Symboltable symboltable) {
        return title() + ": " + Clause.toString(clause,symboltable) + " => " + deducedClause.toString(symboltable,0);}

    /** Checks if the deduced clause is true in all models of the original clause.
     *
     * @param deducedClause The deduced clause to compare with.
     * @param symboltable   The symbol table to use for converting predicates to strings.
     * @param errors        The StringBuilder object to store any errors encountered during verification.
     * @return True if the clause is valid, false otherwise.
     */
    public boolean verify(Clause deducedClause, Symboltable symboltable, StringBuilder errors) {
        IntArrayList literals = Clause.predicates(clause);
        int limit = 1 << (literals.size()-1);
        for(int i = 0; i <= limit; ++i) {
            int ifinal = i;
            if(Clause.isTrue(clause, literal-> isTrue(ifinal,literal,literals))) {
                if(!deducedClause.isTrue(literal->isTrue(ifinal,literal,literals))) {
                    errors.append(title + ": " + Clause.toString(clause,symboltable) +
                            " is true in model: " +model(i,literals,symboltable) + "  but deduced clause " +
                            deducedClause.toString(symboltable, 0) + " is false.\n");
                    return false;}}}
        return true;}

    /** The bits in int integer i represent a model: position 0: first literal in predicates is true etc.
     *  The method checks if the literal is true in the given model.
     *
     * @param i          the bits represent a model of the predicates in the predicates array.
     * @param literal    a literal to be tested.
     * @param literals   a list of predicates.
     * @return           true if the literal is true in the model.
     */
    public boolean isTrue(int i, int literal, IntArrayList literals) {
        int index = literals.indexOf(Math.abs(literal));
        boolean truth = (i & (1 << index)) != 0;
        return literal > 0 ? truth : !truth;}

    /** turns the model into a string
     *
     * @param i           the bits represent a model
     * @param literals    a list of predicates
     * @param symboltable null or a symboltable
     * @return            the model as string.
     */
    public static String model(int i, IntArrayList literals, Symboltable symboltable) {
        String model = "";
        for (int j = 0; j < literals.size(); ++j) {
            if ((i & (1 << j)) != 0) model += Symboltable.toString(literals.getInt(j), symboltable) + ",";
        }
        return model;
    }



    @Override
    public String rule() {
        return null;
    }

    @Override
    public String toString(Symboltable symboltable) {
        return null;
    }


    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {

    }
}
