package Solvers.Resolution;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
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
            if(lit == -literal) throw new UnsatEquivalence(lit,literal,inferenceSteps.get(i), inferenceStep);}
        literals.add(literal);
        inferenceSteps.add(inferenceStep);}

    /** sets the triggerLiteral to 0 and adds the inferenceStep to the individual inference steps.
     *
     * @param inferenceStep which caused the trigger literal to be true.
     */
    void triggerLiteralZero(InferenceStep inferenceStep) {
        triggerLiteral = 0;
        if(inferenceStep != null) {
            for(int i = 0; i < inferenceSteps.size(); ++i) {
                inferenceSteps.set(i,InfList.makeInfList(inferenceSteps.get(i),inferenceStep));}}}

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
        if(triggerLiteral != 0) {
            int status = modelStatus.apply(triggerLiteral);
            switch (status) {
                case -1: return;
                case  0: if(literalsAreInconsistent(modelStatus)) {makeTrue.accept(-triggerLiteral); return;}
                case +1: completeModelForLiterals(modelStatus,makeTrue); return;}}
        else {completeModelForLiterals(modelStatus,makeTrue);}}

    /** completes a model for literals with undecided truth value and where the triggerLiteral is 0 or is true.
     * <br>
     * The model must guaranty that the equivalences are true.<br>
     * There should not be an inconsistency of the existing model with the equivalences.
     * If this happens, an exception is thrown. Something in the algorithm went wrong!
     *
     * @param modelStatus   returns +1 (true), -1 (false) or 0 (undecided).
     * @param makeTrue      for making an undecided literal true.
     * @throws UnsatEquivalence if an inconsistency true(p) == false(q) is found.
     */
    void completeModelForLiterals(IntFunction<Integer> modelStatus, IntConsumer makeTrue) throws UnsatEquivalence {
        findInconsistency(modelStatus);
        int status = modelStatus.apply(representative);
        if (status == 0) {
            for (int literal : literals) {
                int statusLiteral = modelStatus.apply(literal);
                if (statusLiteral != 0) {status = statusLiteral; break;}}}
        if(status == 0) status = 1; // undecided equivalences are made true by making all literals true.
        if(modelStatus.apply(representative) == 0) makeTrue.accept(status*representative);
        for (int literal : literals) {
            if(modelStatus.apply(literal) == 0) makeTrue.accept(status*literal);}}

    /** tries to find an inconsistency true(p) == false(q).
     *
     * @param modelStatus   returns +1 (true), -1 (false) or 0 (undecided)
     * @returns true if an inconsistency true(p) == false(q) is found.
     */
    boolean literalsAreInconsistent(IntFunction<Integer> modelStatus) {
        int status = modelStatus.apply(representative);
        for(int literal : literals) {
            int statusLiteral = modelStatus.apply(literal);
            if(status != 0 && statusLiteral != 0 && status != statusLiteral) return true;
            if(statusLiteral != 0) status = statusLiteral;}
        return false;}

    /** tries to find an inconsistency true(p) == false(q) and generates an UnsatEquivalence exception.
     * <br>
     * Hopefully this method will never be called.
     *
     * @param modelStatus   returns +1 (true), -1 (false) or 0 (undecided)
     * @throws UnsatEquivalence if an inconsistency true(p) == false(q) is found.
     */
    void findInconsistency(IntFunction<Integer> modelStatus) throws UnsatEquivalence{
        int status = modelStatus.apply(representative);
        int trueLiteral  = (status == 1) ? representative : 0;
        int falseLiteral = (status == -1) ? representative : 0;
        for(int i = 0; i < literals.size(); ++i) {
            int literal = literals.getInt(i);
            int statusLiteral = modelStatus.apply(literal);
            if(status != 0 && statusLiteral != 0 && status != statusLiteral) {
                int conflictingLiteral = 0;
                if(statusLiteral ==  1 && falseLiteral != 0)       conflictingLiteral = falseLiteral;
                else {if(statusLiteral == -1 && trueLiteral != 0)  conflictingLiteral = trueLiteral;}
                InferenceStep step = inferenceSteps.get(i);
                if(conflictingLiteral != representative)
                    step = InfList.makeInfList(inferenceSteps.get(Utilities.Utilities.indexOf(literals,conflictingLiteral)), step);
                throw new UnsatEquivalence(conflictingLiteral,literal,step);}
            if(statusLiteral != 0) {
                status = statusLiteral;
                if(status == 1) {trueLiteral = literal; continue;}
                if(status == -1) falseLiteral = literal;}}}


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
