package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Arrays;

/** The inference step represents the application of a true literal to an equivalence.
 * The equivalent literals must ge the same truth value as the true literal.
 */
public class NMISTrueLiteralToEquivalence extends InferenceStep {

    /** the original true literal */
    int oldTrueLiteral;
    /** the deduced true literal */
    int newTrueLiteral;
    /** the orignal equivalence clause */
    int[] equivalence;

    /**
     * Constructs an instance of NMISTrueLiteralToEquivalence.
     *
     * @param oldTrueLiteral  the original true literal
     * @param equivalence     the original equivalence clause
     * @param newTrueLiteral the deduced true literal
     */
    public NMISTrueLiteralToEquivalence(int oldTrueLiteral, int[] equivalence, int newTrueLiteral) {
        this.oldTrueLiteral = oldTrueLiteral;
        this.equivalence = equivalence;
        this.newTrueLiteral = newTrueLiteral;
    }

    /**Verifies the equivalence step.
     *
     * @param symboltable the symbol table containing the predicate names
     * @param errors the StringBuilder to store any error messages
     * @return true if there are no errors, false otherwise
     */
    public boolean verify(Symboltable symboltable, StringBuilder errors) {
        byte oldSign = 0;  byte newSign = 0;
        if (equivalence[1] != Quantifier.EQUIV.ordinal()) {
            errors.append(title() + ": Clause " + Arrays.toString(equivalence) + " is not an equivalence.\n");
            return false;}
        for(int i = Quantifier.EQUIV.firstLiteralIndex; i < equivalence.length; ++i) {
            if (equivalence[i] ==  oldTrueLiteral) {oldSign = 1;}
            if (equivalence[i] == -oldTrueLiteral) {oldSign = -1;}
            if (equivalence[i] ==  newTrueLiteral) {newSign = 1;}
            if (equivalence[i] == -newTrueLiteral) {newSign = -1;}}
        boolean error = false;
        if(oldSign == 0) {errors.append(title()+ ": Literal " + Symboltable.toString(oldTrueLiteral,symboltable) +
                " is not in the clause " + InputClauses.toString(0,equivalence,symboltable)+"\n"); error = true;}
        if(newSign == 0) {errors.append(title()+ ": Literal " + Symboltable.toString(newTrueLiteral,symboltable) +
                " is not in the clause " + InputClauses.toString(0,equivalence,symboltable)+"\n"); error = true;}
        if(oldSign != newSign) {errors.append(title() + ": Literals " + Symboltable.toString(oldTrueLiteral,symboltable) +
                " and " +Symboltable.toString(newTrueLiteral,symboltable) + " have different signs.\n"); error = true;}
        return !error;}

    @Override
    public String title() {
        return "True Equivalent Literals";
    }

    @Override
    public String rule() {
        return null;
    }

    /**
     * Returns a string representation of the inference step.
     *
     * @param symboltable null or a symbol table containing predicate names
     * @return a string representation of the inference step.
     */
    public String toString(Symboltable symboltable) {
        return title() +": true(" + symboltable.toString(oldTrueLiteral,symboltable) + ") and equivalence " +
                InputClauses.toString(0,equivalence,symboltable) + " => true(" +
                symboltable.toString(newTrueLiteral,symboltable) + ")";}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {

    }

}
