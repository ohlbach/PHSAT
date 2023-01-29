package Datastructures.Theory;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Results.UnsatisfiableLiteral;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.BiConsumer;


/** This class represents a propositional model, i.e. a set of literals which are supposed to be true.<br>
 *
 * When a literal is added to the model, and its negation is already in the model then
 * an Unsatisfiable exception is thrown. This usually terminates the search.
 * <br>
 * Besides the mapping of literals to true/false, this class provides the following services to the system:
 * <ul>
 *     <li>Each literal in the model is accompanied by the inference step that caused the literal to be true.<br>
 *     This allows one to reconstruct the results of the search for a model or the unsatisfiability of the clauses.</li>
 *     <li>One can install observers. These are functions which get automatically called when a new
 *     true literal is added to the model.<br>
 *     The observers distribute the information about the literal to the other reasoners.</li>
 * </ul>
 * Critical methods in the model are synchronized such that they can be called from different threats.
 */
public class Model {
    /** the maximum number of predicates. */
    public int predicates;

    /** the current model. It collects the true literals.*/
    public IntArrayList model;

    /** the inference steps which caused the truth of the literals. The positions in the array are the same
     * as the positions of the true literal in the model-array.*/
    public ArrayList<InferenceStep> inferenceSteps;

    /** maps predicates in the model to +1 (true), -1 (false) or 0 (undefined).
     * It provides the fastest lookup of a literals status in the model.*/
    private byte[] status;

    /** functions to be called when a new true literal is inserted. */
    private final ArrayList<BiConsumer<Integer, InferenceStep>> observers = new ArrayList<>();


    /** creates a model with a maximum number of predicates.
     *
     * @param predicates the maximum number of predicates.
     */
    public Model(int predicates) {
        assert predicates > 0;
        this.predicates  = predicates;
        model          = new IntArrayList(predicates);
        inferenceSteps = new ArrayList<>(predicates);
        status         = new byte[predicates+1];}


    /** adds a new observer which gets called when a new true literal is inserted.
     * An observer is a function to be called for the true literal and the corresponding inference step.
     *
     * @param observer a function (literal,inference-step)
     */
    public synchronized void addObserver(BiConsumer<Integer, InferenceStep> observer) {
        observers.add(observer);}

    /** adds a literal to the model with null inference step.
     *
     * @param literals some literals.
     * @throws Unsatisfiable if a contradiction is found.
     */
    public synchronized void add(int... literals) throws Unsatisfiable {
        for(int literal : literals) add(literal,null);}

    /** adds a literal to the model and checks if the literal is already in the model.
     * If the literal is new to the model then all observers are called.
     * If the literal is already false in the model then an UnsatisfiableLiteral exception is thrown
     * with the literal, the old inference step and the new inference step.
     *
     * @param literal the literal for the model.
     * @param inferenceStep  the inference step that caused the truth of the model.
     * @throws UnsatisfiableLiteral if a contradiction with an earlier entry in the model occurs.
     */
    public synchronized void add(int literal, InferenceStep inferenceStep) throws Unsatisfiable {
        int predicate = Math.abs(literal);
        assert predicate > 0 && predicate <= predicates;
        if(isTrue(literal)) {return;}
        if(isFalse(literal)) {throw new UnsatisfiableLiteral(literal,getInferenceStep(literal),inferenceStep);}
        inferenceSteps.add(inferenceStep);
        model.add(literal);
        status[predicate] = literal > 0 ? (byte)1: (byte)-1;

        for(BiConsumer<Integer, InferenceStep> observer : observers) {
            observer.accept(literal,inferenceStep);}}


    /** adds the literals immediately without any checks and inference step.
     * No observers are called.
     * This method is useful for algorithms which work with candidate models and backtracking.
     *
     * @param literals a literal
     */
    public void addImmediately(int... literals) {
        for(int literal : literals) {
            model.add(literal);
            status[Math.abs(literal)] = literal > 0 ? (byte)1: (byte)-1;}}

    /** checks if the literal is true in the model.
     *
     * @param literal the literal to be checked.
     * @return true if the literal is true in the model.
     */
    public synchronized boolean isTrue(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        short status = this.status[predicate];
        if(status == 0) {return false;}
        return literal > 0 ? status == 1: status == -1;}

    /** checks if the literal is false in the model.
     *
     * @param literal the literal to be checked.
     * @return true if the literal is false in the model.
     */
    public synchronized boolean isFalse(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        short status = this.status[predicate];
        if(status == 0) {return false;}
        return literal > 0 ? status == -1: status == 1;}

    /** returns the status of the literal in the model.
     *
     * @param literal the literal to be checked.
     * @return +1 if the literal is true in the model, -1 if it is false in the model, otherwise 0.
     */
    public synchronized int status(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        short status = this.status[predicate];
        return literal > 0 ? status : -status;}

    /** returns null or the inference step which caused the truth of the literal in the model.
     *
     * @param literal a literal.
     * @return null or the inference step which caused the truth of the literal in the model.
     */
    public synchronized InferenceStep getInferenceStep(int literal) {
        if(inferenceSteps == null) {return null;}
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        byte status = this.status[predicate];
        if(status == 0) {return null;}
        int position = model.indexOf((status > 0) ? predicate : -predicate);
        if(position >= inferenceSteps.size()) return null;
        return inferenceSteps.get(position);}


    /** turns the status value into a string.
     *
     * @param status -1,0,+1.
     * @return the corresponding name.
     */
    public synchronized static String toString(int status) {
        switch(status) {
            case -1 : return "false";
            case  0:  return "unknown";
            case +1 : return "true";}
        return "error";}


    // unklar

    /** clones the model (without observers)
     *
     * @return a clone of the model.
     */
    public Model clone() {
        Model newModel = new Model(predicates);
        newModel.inferenceSteps = new ArrayList<>();
        newModel.inferenceSteps.addAll(inferenceSteps);
        newModel.status = status.clone();
        newModel.model = model.clone();
        return newModel;}

    /** checks if the literal is in the model.
     *
     * @param literal a literal.
     * @return true if the model contains the literal.
     */
    public synchronized boolean contains(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        return status[predicate] != 0;}

    /** returns the current size of the model.
     *
     * @return the current size of the model.
     */
    public synchronized int size() {return model.size();}

    /** checks if the model is empty.
     *
     * @return true if the model is empty.
     */
    public synchronized boolean isEmpty() {return model.isEmpty();}

    /** checks if all predicates have a truth value.
     *
     * @return true if all predicates have a truth value.
     */
    public synchronized boolean isComplete() {return model.size() == predicates;}


    /** clears the data */
    public void clear() {
        model.clear();
        inferenceSteps.clear();
        for(int i = 0; i <= predicates; ++i) status[i] = 0;}

    /** returns the model as a comma separated string of numbers (sorted).
     *
     * @return the model as a comma separated string of numbers (sorted).
     */
    public String toString() {
        return toString(null);}

    /** returns the model as a comma separated string of names or numbers (sorted).
     *
     * @param symboltable null or a symboltable.
     * @return the model as a comma separated string of names or numbers (sorted).
     */
    public String toString(Symboltable symboltable) {
        IntArrayList sortedModel = new IntArrayList(model.size());
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            byte state = status[predicate];
            if(state != 0) {sortedModel.add(state == 1 ? predicate : -predicate);}}
        if(sortedModel.isEmpty()) return "";
        StringBuilder st = new StringBuilder();
        for(int i = 0; i < sortedModel.size()-1; ++i)
            st.append(Symboltable.toString(sortedModel.getInt(i),symboltable)).append(",");
        st.append(Symboltable.toString(sortedModel.getInt(sortedModel.size()-1),symboltable));
        return st.toString();}

}
