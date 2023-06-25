package Solvers.Resolution;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Utilities.BiIntConsumerWithUnsatisfiable;
import Utilities.IntToByteFunction;

import java.util.function.IntConsumer;
import java.util.function.IntFunction;

/** The class represents a conditioned equivalence class: triggerLiteral -&gt; representative == literal1 == literal2 == ...
 * <br>
 *  If the triggerLiteral is 0 then there is no condition for the equivalence.<br>
 *  The representative is always a positive literal.
 */
public class Equivalence {

    /** the representative literal for the equivalence class. It is always positive.*/
    int representative;

    /** the equivalent literal. */
    int literal;

    /** the inference steps which caused the derivation of the equivalence.
     * infereceSteps[i] corresponds to the equivalence representative == literals[i]. */
    InferenceStep inferenceStep = null;

    /** constructs a new equivalence class.
     *
     * @param literal1 a literal
     * @param literal2 a literal with literal1 == literal2
     * @param inferenceStep  null or the inference step for the equivalence.
     */
    public Equivalence(int literal1, int literal2, InferenceStep inferenceStep) {
        if(Math.abs(literal1) < Math.abs(literal2)) {
              representative = literal1; literal = literal2;}
        else {representative = literal2; literal = literal1;}
        if(representative < 0) {representative *= -1; literal = -1;}
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
         * @param trueLiterals applied to the other true literals together with the corresponding inference step.
         * @return true if the equivalence class is now useless and can be removed.
         * @throws Unsatisfiable if a contradiction is encountered.
         */
    boolean applyTrueLiteral(IntToByteFunction trueLiteral, InferenceStep inferenceStep, BiIntConsumerWithUnsatisfiable<InferenceStep> trueLiterals) throws Unsatisfiable{
        int sign;
        if((sign = trueLiteral.apply(representative)) != 0) {
            trueLiterals.accept(sign*literal,InfList.makeInfList(inferenceStep,inferenceStep));
            return true;}
        else {
            if((sign = trueLiteral.apply(literal)) != 0) {
                trueLiterals.accept(sign*representative,InfList.makeInfList(inferenceStep,inferenceStep));
                return true;}}
        return false;}


    /** completes a model for literals with undecided truth value.
     * <br>
     * The model must guaranty that the equivalences are true.<br>
     * Triggered equivalences t &gt;- p == q == ...<br>
     * are even true if false(t) and true(p) == false(q) holds.<br>
     * There should not be an inconsistency of the existing model with the equivalences.
     * If this happens, an exception is thrown. Something in the algorithm went wrong!
     *
     * @param modelStatus   returns +1 (true), -1 (false) or 0 (undecided).
     * @param makeTrue      for making an undecided literal true.
     * @throws UnsatEquivalence if an inconsistency true(p) == false(q) is found.
     */
    void completeModel(IntFunction<Integer> modelStatus, IntConsumer makeTrue) throws UnsatEquivalence {
        int status1 = modelStatus.apply(representative);
        int status2 = modelStatus.apply(literal);
        if(status1 != 0 && status2 != 0 && status1 != status2) {
            throw new UnsatEquivalence(representative,literal,inferenceStep);}
        if(status1 == 0) {
            if(status2 == 0) {makeTrue.accept(representative); makeTrue.accept(literal); return;}
            else             {makeTrue.accept(status2*representative); return;}}
        if(status2 == 0) makeTrue.accept(status1*literal);}


    /** turns the equivalence class into a string triggerLiteral -&gt; representative == literal1 == ...
     *
     * @return the equivalence class as a string.
     */
    public String toString() {
        return toString(null);}

    /** turns the equivalence class into a string triggerLiteral -&gt; representative == literal1 == ...
     *
     * @param symboltable null or a symboltable for the predicates.
     * @return the equivalence class as a string.
     */
    public String toString(Symboltable symboltable) {
        return Symboltable.toString(representative,symboltable) + " == "+Symboltable.toString(literal,symboltable);}
}
