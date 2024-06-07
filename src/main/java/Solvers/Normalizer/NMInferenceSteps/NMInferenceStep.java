package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Solvers.Normalizer.Clause;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.isTrue;

/**
 * NMInferenceStep class represents the top class of the inference steps in the Normalize package.
 */
public class NMInferenceStep extends InferenceStep {

    /** the title of the inference rule */
    public String title;

    /** the original clause of the inference */
    public int[] clauseBefore;

    /** returns the title of the inference rule */
    @Override
    public String title() {
        return title;}

    /** Creates the inference step with title and clause
     *
     * @param title   the title of the inference rule
     * @param clauseBefore  the original clause to which the inference rule is applied.
     */
    public NMInferenceStep(String title, int[] clauseBefore) {
        this.title = title;
        this.clauseBefore = clauseBefore;}

    /** turns the inference step into a string: title original clause =&gt; deduced clause.
     *
     *
     * @param deducedClause The deduced clause in the inference step.
     * @param symboltable The symbol table to use for converting predicates to strings, or null.
     * @return The string representation of the inference step.
     */
    public String toString(Clause deducedClause, Symboltable symboltable) {
        return title() + ": " + Clause.toString(clauseBefore,symboltable) + " => " + deducedClause.toString(symboltable,0);}

    /** Checks if the deduced clause is true in all models of the original clause.
     *
     * @param deducedClause The deduced clause to compare with.
     * @param symboltable   The symbol table to use for converting predicates to strings.
     * @param errors        The StringBuilder object to store any errors encountered during verification.
     * @return True if the clause is valid, false otherwise.
     */
    public boolean verify(Clause deducedClause, Symboltable symboltable, StringBuilder errors) {
        IntArrayList literals = Clause.predicates(clauseBefore);
        int limit = 1 << (literals.size()-1);
        for(int i = 0; i <= limit; ++i) {
            int ifinal = i;
            if(Clause.isTrue(clauseBefore, literal-> isTrue(ifinal,literal,literals))) {
                if(!deducedClause.isTrue(literal->isTrue(ifinal,literal,literals))) {
                    errors.append(title + ": " + Clause.toString(clauseBefore,symboltable) +
                            " is true in model: " +model(i,literals,symboltable) + "  but deduced clause " +
                            deducedClause.toString(symboltable, 0) + " is false.\n");
                    return false;}}}
        return true;}


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
