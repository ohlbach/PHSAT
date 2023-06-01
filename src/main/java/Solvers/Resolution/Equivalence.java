package Solvers.Resolution;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.BiConsumer;

/** The class represents a conditioned equivalence class: triggerLiteral -&gt; representative == literal1 == literal2 == ...
 * <br>
 *  If the triggerLiteral is 0 then there is no condition for the equivalence.<br>
 *  The representative is always a positive literal.
 */
public class Equivalence {

    /** The condition for the literal. If it is 0 then there is no condition. */
    int triggerLiteral;

    /** the representative literal for the equivalence class. It is always positive.*/
    int representative;

    /** the literals which are equivalent to each other and the representative. */
    IntArrayList literals = new IntArrayList();

    /** the inference steps which caused the derivation of the equivalence.
     * infereceSteps[i] corresponds to the equivalence representative == literals[i]. */
    ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();

    /** constructs a new equivalence class.
     *
     * @param triggerLiteral 0 or the condition for the equivalence.
     * @param representative the representative of the equivalence representative == literal.
     * @param literal        the equivalent literal.
     * @param inferenceStep  null or the inference step for the equivalence.
     */
    public Equivalence(int triggerLiteral, int representative, int literal, InferenceStep inferenceStep) {
        assert representative > 0;
        this.triggerLiteral = triggerLiteral;
        this.representative = representative;
        literals.add(literal);
        inferenceSteps.add(inferenceStep);}

    /** adds a new literal which is equivalent to the other literals in the class.
     *
     * @param literal        a new equivalent literal.
     * @param inferenceStep  null or the inference step which caused the equivalence.
     * @throws Unsatisfiable if the new literal causes a contradiction: literal == -literal.
     */
    void add(int literal, InferenceStep inferenceStep) throws Unsatisfiable {
        for(int i = 0; i < literals.size(); ++i) {
            int lit = literals.getInt(i);
            if(lit == literal) return;
            if(lit == -literal) throw new UnsatEquivalence(lit,literal,inferenceSteps.get(i), inferenceStep);
            literals.add(literal);
            inferenceSteps.add(inferenceStep);}}

    /** maps the literal to the representative in the equivalence class.
     *
     * @param literal any literal
     * @return  either the literal itself, or the (possibly negated) representative in the equivalence class.
     */
    int getRepresentative(int literal) {
        if(literal == representative || literal == -representative) return literal;
        for(int lit: literals) {
            if(lit == literal)  return representative;
            if(lit == -literal) return -representative;}
        return literal;}

    /** If the literal is equivalent to the representative in the equivalence class (or its negation)
     * then the corresponding inference step is returned, otherwise null is returned.
     *
     * @param literal any literal
     * @return  null or the inference step for the equivalence of the literal with the representative.
     */
    InferenceStep getInferenceStep(int literal) {
        if(literal == representative || literal == -representative) return null;
        for(int i = 0; i < literals.size(); ++i) {
            int lit = literals.getInt(i);
            if(lit == literal || lit == -literal) return inferenceSteps.get(i);}
        return null;}

    /** joins two overlapping equivalence classes.
     *
     * @param sign          +1 or -1. It controls if the literals or their negations are joined in the class.
     * @param equivalence   another equivalence class which is to be joined with the class.
     * @param inferenceStep null or the inference step which caused the overlap with the class.
     */
    void join(int sign, Equivalence equivalence, InferenceStep inferenceStep) {
        literals.add(sign*equivalence.representative);
        inferenceSteps.add(inferenceStep);
        for(int i = 0; i < equivalence.literals.size(); ++i) {
            literals.add(sign*equivalence.literals.getInt(i));
            inferenceSteps.add(InfList.makeInfList(inferenceStep,equivalence.inferenceSteps.get(i)));}}

    /** applies a true literal to the equivalence class.
     * <br>
     * If the true literal or its negation is in the class then all other literals are also made true/false.
     *
     * @param trueLiteral  a literal which is supposed to be true.
     * @param inferenceStep which caused the truth of the literal.
     * @param trueLiterals applied to the other true literals together with the corresponding inference step.
     * @return true if the literals in the equivalence class are true now (and the class can be removed).
     */
    boolean applyTrueLiteral(int trueLiteral, InferenceStep inferenceStep, BiConsumer<Integer,InferenceStep> trueLiterals) {
        int sign = 0;
        if(trueLiteral == representative)        sign = 1;
        else {if(trueLiteral == -representative) sign = -1;}
        if(sign != 0) {
            for(int i = 0; i < literals.size(); ++i) {
                trueLiterals.accept(sign*literals.getInt(i),InfList.makeInfList(inferenceStep,inferenceSteps.get(i)));}
            return true;}

        for(int i = 0; i < literals.size(); ++i) {
            int literal = literals.getInt(i);
            if(literal == trueLiteral) {
                trueLiterals.accept(representative,InfList.makeInfList(inferenceStep,inferenceSteps.get(i)));
                for(int j = 0; j < literals.size(); ++j) {
                    int lit = literals.getInt(j);
                    if(lit != trueLiteral) trueLiterals.accept(lit, InfList.makeInfList(inferenceStep,inferenceSteps.get(i),inferenceSteps.get(j)));}
                return true;}
            if(literal == -trueLiteral) {
                trueLiterals.accept(-representative,InfList.makeInfList(inferenceStep,inferenceSteps.get(i)));
                for(int j = 0; j < literals.size(); ++j) {
                    int lit = literals.getInt(j);
                    if(lit != -trueLiteral) trueLiterals.accept(-lit,InfList.makeInfList(inferenceStep,inferenceSteps.get(i),inferenceSteps.get(j)));}
                return true;}}
        return false;}


    /** checks if the literal or its negation is in the equivalence class.
     *
     * @param literal any literal
     * @return true if the literal or its negation is in the equivalence class.
     */
    boolean contains(int literal) {
        if(literal == representative || literal == -representative) return true;
        for(int lit : literals) {if(literal == lit || literal == -lit) return true;}
        return false;}


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
        string += Symboltable.toString(representative,symboltable);
        for(int literal : literals) string += " == "+Symboltable.toString(literal,symboltable);
        return string;}
}
