package Solvers.Resolution;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Utilities.BiIntConsumerWithUnsatisfiable;
import Utilities.IntToByteFunction;

import java.util.function.IntConsumer;

/** The class represents the equivalence of two literals: representative == literal
 * <br>
 *  The representative is always a positive literal with representative &lt; |literal|
 */
public class Equivalence {

    /** the representative literal for the equivalence class. It is always positive.*/
    int representative;

    /** the equivalent literal. */
    int literal;

    /** the inference step which caused the derivation of the equivalence.*/
    InferenceStep inferenceStep = null;

    /** constructs a new equivalence class representative == literal.
     * <br>
     * The representative is the smaller literal and always positive.
     *
     * @param literal1 a literal
     * @param literal2 a literal with literal1 == literal2
     * @param inferenceStep  null or the inference step for the equivalence.
     */
    public Equivalence(int literal1, int literal2, InferenceStep inferenceStep) {
        if(Math.abs(literal1) < Math.abs(literal2)) {
              representative = literal1; literal = literal2;}
        else {representative = literal2; literal = literal1;}
        if(representative < 0) {representative *= -1; literal *= -1;}
        this.inferenceStep = inferenceStep;}


    /** maps the literal to the representative in the equivalence class.
     *
     * @param literal any literal
     * @return  either the literal itself, or the (possibly negated) representative in the equivalence class.
     */
    int getRepresentative(int literal) {
        if(literal == representative || literal == -representative) return literal;
        if(literal == this.literal)  return  representative;
        if(literal == -this.literal) return -representative;
        return literal;}

    /** applies a true literal to the equivalence class.
         * <br>
         * If the true literal or its negation is in the class then all other literals are also made true/false.
         *
         * @param trueLiteral  to be applied to a literal: +1(true), -1(false), 0(undecided)
         * @param inferenceStep which caused the truth of the literal.
         * @param trueLiterals applied to the other true literal together with the corresponding inference step.
         * @return true if the equivalence class is now useless and can be removed.
         * @throws Unsatisfiable if a contradiction is encountered.
         */
    boolean applyTrueLiteral(IntToByteFunction trueLiteral, InferenceStep inferenceStep, BiIntConsumerWithUnsatisfiable<InferenceStep> trueLiterals) throws Unsatisfiable{
        int sign1 = trueLiteral.apply(representative); int sign2 = trueLiteral.apply(literal);
        if(sign1 != 0 && sign2 != 0) {
            if(sign1 == sign2) return true;
            else throw new UnsatEquivalence(representative,literal,InfList.makeInfList(this.inferenceStep,inferenceStep));}
        if(sign1 != 0) {
            trueLiterals.accept(sign1*literal,InfList.makeInfList(this.inferenceStep,inferenceStep));
            return true;}
        else {
            if(sign2 != 0) {
                trueLiterals.accept(sign2*representative,InfList.makeInfList(this.inferenceStep,inferenceStep));
                return true;}}
        return false;}


    /** completes a model for literals with undecided truth value.
     * <br>
     * The model must guaranty that the equivalences are true.<br>
     * There should not be an inconsistency of the existing model with the equivalences.<br>
     * If this happens, an exception is thrown. Something in the algorithm went wrong!
     *
     * @param modelStatus   returns +1 (true), -1 (false) or 0 (undecided).
     * @param makeTrue      for making an undecided literal true.
     * @throws UnsatEquivalence if an inconsistency true(p) == false(q) is found.
     */
    void completeModel(IntToByteFunction modelStatus, IntConsumer makeTrue) throws UnsatEquivalence {
        byte status1 = modelStatus.apply(representative);
        byte status2 = modelStatus.apply(literal);
        if(status1 != 0 && status2 != 0 && status1 != status2) {
            throw new UnsatEquivalence(representative,literal,inferenceStep);}
        if(status1 == 0) {
            if(status2 == 0) {makeTrue.accept(representative); makeTrue.accept(literal); return;}
            else             {makeTrue.accept(status2*representative); return;}}
        if(status2 == 0) makeTrue.accept(status1*literal);}


    /** turns the equivalence class into a string  representative == literal
     *
     * @return the equivalence class as a string.
     */
    public String toString() {
        return toString(null);}

    /** turns the equivalence class into a string representative == literal
     *
     * @param symboltable null or a symboltable for the predicates.
     * @return the equivalence class as a string.
     */
    public String toString(Symboltable symboltable) {
        return Symboltable.toString(representative,symboltable) + " == "+Symboltable.toString(literal,symboltable);}
}
