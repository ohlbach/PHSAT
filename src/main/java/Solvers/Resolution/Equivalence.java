package Solvers.Resolution;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Utilities.IntToByteFunction;

import java.util.function.BiConsumer;
import java.util.function.IntConsumer;
import java.util.function.IntFunction;

/** The class represents a conditioned equivalence class: triggerLiteral -&gt; representative == literal1 == literal2 == ...
 * <br>
 *  If the triggerLiteral is 0 then there is no condition for the equivalence.<br>
 *  The representative is always a positive literal.
 */
public class Equivalence {

    /** The condition for the literal. If it is 0 then there is no condition. */
    int triggerLiteral;

    /** the equivalent literals.*/
    int representative, literal;


    /** the inference step which caused the derivation of the equivalence.*/
    InferenceStep inferenceStep;

    /** constructs a new equivalence class.
     *
     * @param triggerLiteral 0 or the condition for the equivalence.
     * @param literal1  a literal
     * @param literal2  a literal with literal1 == literal2.
     * @param inferenceStep  null or the inference step for the equivalence.
     */
    public Equivalence(int triggerLiteral, int literal1, int literal2, InferenceStep inferenceStep) {
        assert representative > 0;
        this.triggerLiteral = triggerLiteral;
        if(Math.abs(literal1) < Math.abs(literal2)) {
            representative = literal1; literal = literal2;}
        else {representative = literal2; literal = literal1;}
        if(representative < 0) {representative *= -1; literal *= -1;}
        this.inferenceStep = inferenceStep;}


    /** sets the triggerLiteral to 0 and adds the inferenceStep to the individual inference steps.
     *
     * @param inferenceStep which caused the trigger literal to be true.
     */
    void triggerLiteralZero(InferenceStep inferenceStep) {
        triggerLiteral = 0;
        if(inferenceStep != null) this.inferenceStep = InfList.makeInfList(this.inferenceStep,inferenceStep);}

    /** maps the literal to the representative in the equivalence class.
     *
     * @param literal any literal
     * @return  either the literal itself, or the (possibly negated) representative in the equivalence class.
     */
    int getRepresentative(int literal) {
        if(literal ==  this.literal) return  representative;
        if(literal == -this.literal) return -representative;
        return literal;}


    /** applies a true literal to the equivalence class.
     * <br>
     * If the true literal or its negation is in the class then all other literals are also made true/false.
     *
     * @param modelStatus  maps a literal to +1(true),-1(false) or 0(undecided).
     * @param inferenceStep null or a function that maps a literal to null or to the inference step that caused the truth of the literal.
     * @param trueLiterals applied to the other true literals together with the corresponding inference step.
     * @return true if the literals in the equivalence class are true now (and the class can be removed).
     */
    boolean applyTrueLiteral(IntToByteFunction modelStatus, IntFunction<InferenceStep> inferenceStep,
                             BiConsumer<Integer,InferenceStep> trueLiterals) throws Unsatisfiable{
        if(triggerLiteral != 0) {
            switch(modelStatus.apply(triggerLiteral)) {
                case -1: return true;   // the entire class became useless
                case 0: return false;}} // the truth of literals cannot yet be decided.
        // from here the triggerLiteral is either 0 or true.

        byte signRep = modelStatus.apply(representative);
        byte signLit = modelStatus.apply(literal);
        InferenceStep stepRep = inferenceStep == null ? null : inferenceStep.apply(representative);
        InferenceStep stepLit = inferenceStep == null ? null : inferenceStep.apply(literal);
        if(signRep != 0) {
            if(signRep == -signLit) throw new UnsatEquivalence(representative,literal,this.inferenceStep,stepRep,stepLit);
            if(signRep == signLit) return true;
            if(signLit == 0) {trueLiterals.accept(literal,InfList.makeInfList(this.inferenceStep,stepRep)); return true;}}
        if(signLit != 0) {
            trueLiterals.accept(representative,InfList.makeInfList(this.inferenceStep,stepLit)); return true;}
        return false;}


    /** checks if the literal or its negation is in the equivalence class.
     *
     * @param literal any literal
     * @return true if the literal or its negation is in the equivalence class.
     */
    boolean contains(int literal) {
        return (literal == representative || literal == -representative || literal == this.literal || literal == -this.literal);}

    /** completes a model for literals with undecided truth value.
     * <br>
     * The model must guarantee that the equivalences are true.<br>
     * Triggered equivalences t &gt;- p == q == ...<br>
     * are even true if false(t) and true(p) == false(q) holds.<br>
     * There should not be an inconsistency of the existing model with the equivalences.
     * If this happens, an exception is thrown. Something in the algorithm went wrong!
     *
     * @param modelStatus   returns +1 (true), -1 (false) or 0 (undecided).
     * @param makeTrue      for making an undecided literal true.
     * @throws UnsatEquivalence if an inconsistency true(p) == false(q) is found.
     */
    void completeModel(IntToByteFunction modelStatus, IntConsumer makeTrue) throws UnsatEquivalence {
        byte statusTrigger = triggerLiteral == 0 ? 0 : modelStatus.apply(triggerLiteral);
        if(triggerLiteral != 0 && statusTrigger == -1) return; // equivalence can be false.
        byte statusRep = modelStatus.apply(representative);
        byte statusLit = modelStatus.apply(literal);
        if(statusRep != 0) {
            if(statusRep == statusLit) return; // equivalence is true
            if(statusRep == -statusLit) { // equivalence is false. Trigger literal must be false.
                if(triggerLiteral == 0 || statusTrigger == 1) {throw new UnsatEquivalence(representative,literal,inferenceStep);}// should never happen.
                makeTrue.accept(-triggerLiteral); return;}}
        makeTrue.accept(statusLit*representative);} // alternative: make the triggerLiteral false.



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
        String string = triggerLiteral != 0 ? Symboltable.toString(triggerLiteral,symboltable) + " -> " : "";
        string += Symboltable.toString(representative,symboltable) + " == "+Symboltable.toString(literal,symboltable);
        return string;}
}
