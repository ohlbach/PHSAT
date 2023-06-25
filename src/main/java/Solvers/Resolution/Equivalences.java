package Solvers.Resolution;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Utilities.BiIntConsumerWithUnsatisfiable;
import Utilities.IntToByteFunction;

import java.util.ArrayList;
import java.util.function.IntConsumer;

/** This class represents a list of equivalence classes: representative == literal.
 * <br>
 * Once an  equivalence is detected, it is applied immediately to all clauses.
 * Therefor there is no need for maintaining overlapping equivalence relations.
  */
public class Equivalences {

    /** a list of equivalence classes */
    ArrayList<Equivalence> equivalences = new ArrayList<>();

    /** adds a new equivalence to the set of equivalence classes.
     *
     * @param equivalence an equivalence object.
     */
    void add(Equivalence equivalence)  {
        equivalences.add(equivalence);}


    /** to be used in applyTrueLiteral */
    private final Unsatisfiable[] unsatisfiable = {null};

    /** applies true literals to all equivalence classes.
     * <br>
     * If a literal in an equivalence class is true then the other literal in this class is made true.<br>
     * Equivalence classes which are true now are removed from the list.
     *
     * @param trueLiteral  to be applied to a literal: +1(true), -1(false), 0(undecided)
     * @param inferenceStep which caused the truth of the literal.
     * @param trueLiterals applied to the other true literals together with the corresponding inference step.
     * @throws Unsatisfiable if a contradiction is encountered.
     * */
    void applyTrueLiteral(IntToByteFunction trueLiteral, InferenceStep inferenceStep,
                             BiIntConsumerWithUnsatisfiable<InferenceStep> trueLiterals) throws Unsatisfiable {
        unsatisfiable[0] = null;
        equivalences.removeIf(equivalence -> {
                    try{return equivalence.applyTrueLiteral(trueLiteral,inferenceStep, trueLiterals);}
                    catch(Unsatisfiable unsat) {unsatisfiable[0] = unsat; return false;}});
        if(unsatisfiable[0] != null) throw unsatisfiable[0];}

    /** checks if the list of equivalence classes is empty
     *
     * @return true if there is no equivalence class.
     */
    boolean isEmpty() {return equivalences.isEmpty();}

    /** clears all data (to be used anew).*/
    void clear() {equivalences.clear();}


    /** completes a model after Resolution has finished and pretended that the clause set is satisfiable.
     * <br>
     * Literals without truth values get a truth value such that the equivalence becomes true.  <br>
     * There should be no contradiction true(p) == false(q), but who knows.<br>
     *
     * @param modelStatus returns +1 (true), -1 (false) or 0 (undecided).
     * @param makeTrue    makes an undecided literal true.
     * @return            null (hopefully) or UnsatEquivalence if unexpectedly a contradiction was found.
     */
    UnsatEquivalence completeModel(IntToByteFunction modelStatus, IntConsumer makeTrue) {
        try{for(Equivalence equivalence : equivalences) equivalence.completeModel(modelStatus,makeTrue);}
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
        if(equivalences.isEmpty()) return "";
        StringBuilder st = new StringBuilder();
        st.append("Equivalences:\n");
        st.append(equivalences.get(0).toString(symboltable));
        for(int i = 1; i < equivalences.size(); ++i)
            st.append("\n").append(equivalences.get(i).toString(symboltable));
        return st.toString();}
}
