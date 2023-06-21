package Solvers.Resolution;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Utilities.BiIntConsumerWithUnsatisfiable;
import Utilities.IntToByteFunction;

import java.util.ArrayList;
import java.util.function.IntConsumer;
import java.util.function.IntFunction;

/** This class represents possibly conditioned equivalence classes.
 * <br>
 * Such a class is logically like: triggerLiteral -&gt; representative == literal1 == literal2 == ...<br>
 * If the triggerLiteral is 0 then there is no condition. <br>
 * For each trigger literal there may be different (of course disjoint) equivalence classes.
  */
public class Equivalences {

    /** maps triggerLiterals to a list of equivalence classes */
    ArrayList<Equivalence> equivalences = new ArrayList<>();

    ArrayList<Equivalence> processedEquivalences = new ArrayList<>();

    /** adds a new equivalence to the set of equivalence classes.
     * <br>
     * If the new equivalence overlaps with an existing class, it is added to the existing class.<br>
     * If the new equivalence overlaps with two existing classes, the two classes are joined.<br>
     * The representative of a class is positive and the literal with the smallest number. <br>
     *
     * @param triggerLiteral 0 or a condition literal for the equivalence
     * @param literal1       a literal
     * @param literal2       a literal
     * @param inferenceStep  null or the inference step that caused the equivalence
     * @throws Unsatisfiable if a equivalence p == -p is encountered.
     */
    Equivalence add(int triggerLiteral, int literal1, int literal2, InferenceStep inferenceStep) throws Unsatisfiable {
        if(Math.abs(literal1) > Math.abs(literal2)) {
            int dummy = literal1;
            literal1 = literal2;
            literal2 = dummy;}
        if(literal1 < 0) {literal1 *= -1; literal2 *= -1;}

        Equivalence equivalence1 = getEquivalence(triggerLiteral,literal1);
        Equivalence equivalence2 = getEquivalence(triggerLiteral,literal2);

        Equivalence equivalence = null;

        int representative1 = literal1; int representative2 = literal2;
        InferenceStep step1 = null; InferenceStep step2 = null;
        if(equivalence1 != null) {
            representative1 = equivalence1.getRepresentative(literal1);
            step1           = equivalence1.getInferenceStep(literal1);}
        if(equivalence2 != null) {
            representative2 = equivalence2.getRepresentative(literal2);
            step2           = equivalence2.getInferenceStep(literal2);}

        if(representative1 == representative2) return null;
        if(representative1 == -representative2)
            throw new UnsatEquivalence(literal1, literal2, step1, step2, inferenceStep);

        if(equivalence1 == null && equivalence2 == null) {
            equivalences.add(equivalence = new Equivalence(triggerLiteral,representative1,representative2,inferenceStep));
            return equivalence;}
        if(equivalence2 == null) { // equivalence1 != null
            equivalence1.add(representative2,InfList.makeInfList(step1 ,inferenceStep));
            return equivalence1;}
        if(equivalence1 == null) { // equivalence2 != null
            equivalence2.add(representative1,InfList.makeInfList(step2 ,inferenceStep));
            return equivalence2;}
        if(Math.abs(representative1) < Math.abs(representative2)) {
            equivalence1.join((representative1 == equivalence1.representative ? 1:-1) *
                               (representative2 == equivalence2.representative ? 1:-1),
                equivalence2,inferenceStep);
            equivalences.remove(equivalence2);
            return equivalence1;}
        else {equivalence2.join((representative1 == equivalence1.representative ? 1:-1) *
                                    (representative2 == equivalence2.representative ? 1:-1),
                equivalence1,inferenceStep);
            equivalences.remove(equivalence1);
            return equivalence2;}}


    private final Unsatisfiable[] unsatisfiable = {null};

    /** applies a true literals to all equivalence classes.
     * <br>
     * If a trigger literal is true then it is set to 0 (condition removed).<br>
     * If the trigger literal is false then all the classes are removed.<br>
     * If a literal in an equivalence class is true then all other literals in this class are made true.
     *
     * @param trueLiteral  to be applied to a literal: +1(true), -1(false), 0(undecided)
     * @param inferenceStep which caused the truth of the literal.
     * @param trueLiterals applied to the other true literals together with the corresponding inference step.
     * @return the list of equivalence classes whose trigger literal has become true.
     * @throws Unsatisfiable if a contradiction is encountered.
     * */
    ArrayList<Equivalence> applyTrueLiteral(IntToByteFunction trueLiteral, InferenceStep inferenceStep,
                             BiIntConsumerWithUnsatisfiable<InferenceStep> trueLiterals) throws Unsatisfiable {
        unsatisfiable[0] = null;
        ArrayList<Equivalence> untriggered = new ArrayList<>();
        equivalences.removeIf(equivalence -> {
                    try{int triggerLiteral = equivalence.triggerLiteral;
                        boolean remove = equivalence.applyTrueLiteral(trueLiteral,inferenceStep, trueLiterals);
                        if(!remove && triggerLiteral != 0 && equivalence.triggerLiteral == 0) untriggered.add(equivalence);
                        return remove;}
                    catch(Unsatisfiable unsat) {unsatisfiable[0] = unsat; return false;}});
        if(unsatisfiable[0] != null) throw unsatisfiable[0];
        return untriggered;}

    boolean isEmpty() {
        return equivalences.isEmpty();}

    /** clears all data (to be used anew).
     */
    void clear() {
        equivalences.clear();
        processedEquivalences.clear();}

    /** returns the equivalence class of the given triggerLiteral containing the literal.
     *
     * @param triggerLiteral 0 or a literal
     * @param literal  any literal
     * @return null or the equivalence class containing the literal.
     */
    Equivalence getEquivalence(int triggerLiteral, int literal) {
        for(Equivalence equivalence : equivalences) {
            if(equivalence.triggerLiteral == triggerLiteral && equivalence.contains(literal)) return equivalence;}
        return null;}



    /** returns the inference step of the literal in the class with the triggerLiteral.
     *
     * @param triggerLiteral a trigger literal.
     * @param literal        any literal.
     * @return null or the inference step that caused the equivalence of the literal with the representative.
     */
    InferenceStep getInferenceStep(int triggerLiteral,int literal) {
        for(Equivalence equivalence : equivalences) {
            if(equivalence.triggerLiteral == triggerLiteral) {
                for(int i = 0; i < equivalence.literals.size(); ++i) {
                    if(equivalence.literals.getInt(i) == literal) return equivalence.inferenceSteps.get(i);}}}
        return null;}

    /** puts the equivalence int the backup array and clears the equivalences.
     */
    void backupEquivalence(Equivalence equivalence) {
        processedEquivalences.add(equivalence);
        equivalences.remove(equivalence);}

    /** completes a model after Resolution has finished and pretended that the clause set is satisfiable.
     * <br>
     * Literals without truth values get a truth value such that the equivalences become true.  <br>
     * There should be no contradiction true(p) == false(q), but who knows.<br>
     * A triggered equivalence with  t &gt;= true(p) == false(q) is not a contradiction if t can be made false.
     *
     * @param modelStatus returns +1 (true), -1 (false) or 0 (undecided).
     * @param makeTrue    makes an undecided literal true.
     * @return            null (hopefully) or UnsatEquivalence if unexpectedly a contradiction was found.
     */
    UnsatEquivalence completeModel(IntFunction<Integer> modelStatus, IntConsumer makeTrue) {
        try{
            for(Equivalence equivalence : equivalences)          equivalence.completeModel(modelStatus,makeTrue);
            for(Equivalence equivalence : processedEquivalences) equivalence.completeModel(modelStatus,makeTrue);}
        catch(UnsatEquivalence unsatisfiable) {return unsatisfiable;}
        return null;}


    /** collects all equivalence classes in a string.
     *
     * @return all equivalence classes as a string.
     */
    public String toString() {
        return toString(null);}

    /** collects all equivalence classes in a string.
     *
     * @param symboltable null or a symboltable.
     * @return all equivalence classes as a string.
     */
    public String toString(Symboltable symboltable) {
        if(equivalences.isEmpty() && processedEquivalences.isEmpty()) return "";
        StringBuilder st = new StringBuilder();
        if(!equivalences.isEmpty()) {
            st.append("Unprocessed Equivalences:\n");
            st.append(equivalences.get(0).toString(symboltable));
            for(int i = 1; i < equivalences.size(); ++i)
                st.append("\n").append(equivalences.get(i).toString(symboltable));}
        if(!processedEquivalences.isEmpty()) {
            st.append("Processed Equivalences:\n");
            st.append(processedEquivalences.get(0).toString(symboltable));
            for(int i = 1; i < processedEquivalences.size(); ++i)
                st.append("\n").append(processedEquivalences.get(i).toString(symboltable));}
        return st.toString();}
}
