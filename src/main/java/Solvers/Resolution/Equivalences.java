package Solvers.Resolution;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.function.BiConsumer;

/** This class represents possibly conditioned equivalence classes.
 * <br>
 * Such a class is logically like: triggerLiteral -&gt; representative == literal1 == literal2 == ...<br>
 * If the triggerLiteral is 0 then there is no condition. <br>
 * For each trigger literal there may be different (of course disjoint) equivalence classes.
  */
public class Equivalences {

    /** maps triggerLiterals to a list of equivalence classes */
    HashMap<Integer,ArrayList<Equivalence>> equivalences = new HashMap<>();

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
    void add(int triggerLiteral, int literal1, int literal2, InferenceStep inferenceStep) throws Unsatisfiable {
        if(Math.abs(literal1) > Math.abs(literal2)) {
            int dummy = literal1;
            literal1 = literal2;
            literal2 = dummy;}
        if(literal1 < 0) {literal1 *= -1; literal2 *= -1;}

        Equivalence equivalence1 = getEquivalence(triggerLiteral,literal1);
        Equivalence equivalence2 = getEquivalence(triggerLiteral,literal2);

        int representative1 = literal1; int representative2 = literal2;
        InferenceStep step1 = null; InferenceStep step2 = null;
        if(equivalence1 != null) {
            representative1 = equivalence1.getRepresentative(literal1);
            step1           = equivalence1.getInferenceStep(literal1);}
        if(equivalence2 != null) {
            representative2 = equivalence2.getRepresentative(literal2);
            step2           = equivalence2.getInferenceStep(literal2);}

        if(representative1 == representative2) return;
        if(representative1 == -representative2)
            throw new UnsatEquivalence(literal1, literal2, step1, step2, inferenceStep);

        if(equivalence1 == null && equivalence2 == null) {
            ArrayList<Equivalence> equivalenceList = equivalences.computeIfAbsent(triggerLiteral, k -> new ArrayList<>());
            equivalenceList.add(new Equivalence(triggerLiteral,representative1,representative2,inferenceStep));
            return;}
        if(equivalence2 == null) { // equivalence1 != null
            equivalence1.add(representative2,InfList.makeInfList(step1 ,inferenceStep));
            return;}
        if(equivalence1 == null) { // equivalence2 != null
            equivalence2.add(representative1,InfList.makeInfList(step2 ,inferenceStep));
            return;}
        if(Math.abs(representative1) < Math.abs(representative2)) {
            equivalence1.join((representative1 == equivalence1.representative ? 1:-1) *
                               (representative2 == equivalence2.representative ? 1:-1),
                equivalence2,inferenceStep);
            equivalences.get(triggerLiteral).remove(equivalence2);}
        else {equivalence2.join((representative1 == equivalence1.representative ? 1:-1) *
                                    (representative2 == equivalence2.representative ? 1:-1),
                equivalence1,inferenceStep);
            equivalences.get(triggerLiteral).remove(equivalence1);}}

    /** auxiliary list */
    private final IntArrayList removed = new IntArrayList();

    /** applies a true literal to all equivalence classes.
     * <br>
     * If a trigger literal is true then it is set to 0 (condition removed).<br>
     * If the trigger literal is false then all the classes are removed.<br>
     * If a literal in an equivalence class is true then all other literals in this class are made true.
     *
     * @param literal        a true literal
     * @param inferenceStep  which caused the truth of the literal
     * @param trueLiterals  to be applied to (new true literal, inference step)
     */
    void applyTrueLiteral(int literal, InferenceStep inferenceStep, BiConsumer<Integer,InferenceStep> trueLiterals) {
        ArrayList<Equivalence> equivalenceList = equivalences.get(literal);
        if(equivalenceList != null) {
            for(Equivalence equivalence : equivalenceList) equivalence.triggerLiteralZero(inferenceStep);
            ArrayList<Equivalence> equivalenceZero = equivalences.get(0);
            if(equivalenceZero == null) equivalences.put(0,equivalenceList);
            else equivalenceZero.addAll(equivalenceList);
            equivalences.remove(literal);
            return;}
        equivalenceList = equivalences.get(-literal);
        if(equivalenceList != null) {equivalences.remove(-literal); return;}
        removed.clear();
        equivalences.forEach((triggerLiteral,equivList) -> {
            for(int i = 0; i < equivList.size(); ++i) {
                Equivalence equivalence = equivList.get(i);
                if(equivalence.applyTrueLiteral(literal,inferenceStep, trueLiterals)) {
                    equivList.remove(i);
                    break;}}
            if(equivList.isEmpty()) removed.add((int)triggerLiteral);});
        if(!removed.isEmpty()) {equivalences.remove(removed.getInt(0));}}

    boolean isEmpty() {
        return equivalences.isEmpty();}

    /** returns the equivalence class of the given triggerLiteral containing the literal.
     *
     * @param triggerLiteral 0 or a literal
     * @param literal  any literal
     * @return null or the equivalence class containing the literal.
     */
    Equivalence getEquivalence(int triggerLiteral, int literal) {
        ArrayList<Equivalence> equivalenceList = equivalences.get(triggerLiteral);
        if(equivalenceList == null) return null;
        for(Equivalence equivalence : equivalenceList) {
            if(equivalence.contains(literal)) return equivalence;}
        return null;}

    /** returns the representative of the literal in the class with the triggerLiteral.
     *
     * @param triggerLiteral a trigger literal.
     * @param literal        any literal.
     * @return the literal itself or its representative in the corresponding class.
     */
    int getRepresentative(int triggerLiteral, int literal) {
        Equivalence equivalence = getEquivalence(triggerLiteral,literal);
        if(equivalence != null) return equivalence.getRepresentative(literal);
        return literal;}

    /** returns the inference stwp of the literal in the class with the triggerLiteral.
     *
     * @param triggerLiteral a trigger literal.
     * @param literal        any literal.
     * @return null or the inference step that caused the equivalence of the literal with the representative.
     */
    InferenceStep getInferenceStep(int triggerLiteral,int literal) {
        Equivalence equivalence = getEquivalence(triggerLiteral,literal);
        if(equivalence != null) return equivalence.getInferenceStep(literal);
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
        StringBuilder st = new StringBuilder();
        equivalences.forEach((Integer triggerLiteral,ArrayList<Equivalence> equivalenceList) -> {
            if(!equivalenceList.isEmpty()) {
                st.append(equivalenceList.get(0).toString(symboltable));
                for(int i = 1; i < equivalenceList.size(); ++i) {
                    st.append("\n").append(equivalenceList.get(i).toString(symboltable));}}});
        return st.toString();}
}
