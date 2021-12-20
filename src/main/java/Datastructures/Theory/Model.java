package Datastructures.Theory;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Results.UnsatisfiableLiteral;
import Datastructures.Symboltable;
import InferenceSteps.ContradictoryLiterals;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import javafx.util.Pair;

import java.util.ArrayList;
import java.util.function.BiConsumer;

import static Utilities.Utilities.sortIntArray;


/** This class represents a propositional model, i.e. a set of literals which are supposed to be true.<br>
 * Each literal in the model is accompanied by the origins, i.e. the list of basic clause ids which
 * caused the derivation of the truth of the literal.
 * <br>
 * When a literal is added to the model, and its negation is already in the model then
 * an Unsatisfiable exception is thrown.
 * This may terminate the search process. <br>
 * Adding a non-contradictory literal to the model causes all observers to be called.
 * They distribute the information about the literal to the other reasoners.
 *
 * Created by ohlbach on 12.09.2018.
 */
public class Model {
    /** the maximum number of predicates */
    public int predicates;

    /** the symboltable for the literals */
    public Symboltable symboltable;

    /** the current model */
    private IntArrayList model;

    /** The inference steps which caused the truth of the literals */
    private ArrayList<InferenceStep> inferenceSteps;

    /** maps predicates in the model to +1 (true), -1 (false) or 0 (undefined) */
    private byte[] status;

    /** observers to be called when a new true literal is inserted */
    private final ArrayList<BiConsumer<Integer, InferenceStep>> observers = new ArrayList<>();


    /** creates a model with a maximum number of predicates, together with a means of tracking the origins
     *
     * @param predicates the maximum number of predicates
     * @param symboltable the symboltable for the literals (mostly for error messages).
     */
    public Model(int predicates, Symboltable symboltable) {
        assert predicates > 0;
        this.predicates  = predicates;
        this.symboltable = symboltable;
        model   = new IntArrayList(predicates);
        inferenceSteps = new ArrayList<>(predicates);
        status  = new byte[predicates+1];}


    /** add a new observer which gets called when a new true literal is inserted.
     * Submitting a thread causes the observers to be called only if this thread
     * is different to the thread that submitted the literal.
     * This avoids a ping pong effect.
     *
     * @param observer a function (literal,origins)
     */
    public synchronized void addObserver(BiConsumer<Integer, InferenceStep> observer) {
        observers.add(observer);}

    /** adds a literal to the model with null inference step and null thread
     *
     * @param literals some literals
     * @throws Unsatisfiable if a contradiction is found.
     */
    public synchronized void add(int... literals) throws Unsatisfiable {
        for(int literal : literals) {
            add(literal,null);}}

    /** pushes a literal onto the model and checks if the literal is already in the model.
     * If the literal is new to the model then the observers from a thread different to the
     * submitting thread are called.
     *
     * @param literal the literal for the model.
     * @param inferenceStep the ids of the basic clauses causing this truth
     * @throws Unsatisfiable if a contradiction with an earlier entry in the model occurs.
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


    /** adds a literal immediately without any checks and transfers
     *
     * @param literal a literal
     */
    public void addImmediately(int literal) {
        model.add(literal);
        status[Math.abs(literal)] = literal > 0 ? (byte)1: (byte)-1;}

        /** returns the entire model, i.e. the list of true literals.
         * Access to the list, however is not synchronized.
         *
         * @return the model
         */
    public IntArrayList getModel() {return model;}

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
     * @param literal the literal to be checked
     * @return +1 if the literal is true in the model, -1 if it is false in the model, otherwise 0.
     */
    public synchronized int status(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        short status = this.status[predicate];
        return literal > 0 ? status : -status;}

    /** returns the origins of a model entry
     *
     * @param literal a literal
     * @return null or the origins of the literal entry
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


    /** turns the status value into a string
     *
     * @param status -1,0,+1
     * @return the corresponding name
     */
    public synchronized static String toString(int status) {
        switch(status) {
            case -1 : return "false";
            case  0:  return "unknown";
            case +1 : return "true";}
        return "error";}


    //  Unklar

    /** sets the logical status of the literal.
     *
     * @param literal a literal
     * @param status +1 (for true) and -1 (for false)
     */
    public synchronized void setStatus(int literal, int status) {
        if(literal < 0) {literal = -literal; status = (byte)-status;}
        assert this.status[literal] == 0 || this.status[literal] == status;
        if(this.status[literal] == 0) {
            this.status[literal] = (byte)status;
            model.add(status > 0 ? literal : -literal);}}


    // unklar

    /** clones the model (without observers)
     *
     * @return a clone of the model
     */
    public Model clone() {
        Model newModel = new Model(predicates, symboltable);
        newModel.inferenceSteps = new ArrayList<>();
        newModel.inferenceSteps.addAll(inferenceSteps);
        newModel.status = status.clone();
        newModel.model = model.clone();
        return newModel;}

    /** checks if the literal is in the model.
     *
     * @param literal a literal
     * @return true if the model contains the literal.
     */
    public synchronized boolean contains(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        return status[predicate] != 0;}

    /** returns the current size of the model.
     *
     * @return the current size of the model
     */
    public synchronized int size() {return model.size();}

    /** checks if the model is empty.
     *
     * @return true if the model is empty
     */
    public synchronized boolean isEmpty() {return model.isEmpty();}

    /** checks if the model is full.
     *
     * @return true if the model is full
     */
    public synchronized boolean isFull() {return model.size() == predicates;}

    /** clears the data */
    public void clear() {
        model.clear();
        inferenceSteps.clear();
        for(int i = 0; i <= predicates; ++i) status[i] = 0;}

    /** returns the model as a comma separated string  (sorted)
     *
     * @return the model as a comma separated string  (sorted)
     */
    public String toString() {
        return toString(symboltable);}

    /** returns the model as a comma separated string of numbers (sorted)
     *
     * @return the model as a comma separated string of numbers (sorted)
     */
    public String toNumbers() {
        return toString(null);}

    /** returns the model as a comma separated string of names (sorted)
     *
     * @return the model as a comma separated string of names (sorted)
     */
    public String toString(Symboltable symboltable) {
        IntArrayList model = new IntArrayList();
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            int status = status(predicate);
            if(status != 0) model.add(status*predicate);}
        if(model.isEmpty()) return "";
        StringBuilder st = new StringBuilder();
        st.append("Model:\n");
        for(int i = 0; i < model.size()-1; ++i)
            st.append(Symboltable.toString(model.getInt(i),symboltable)).append(",");
        st.append(Symboltable.toString(model.getInt(model.size()-1),symboltable));
        return st.toString();}


    /** turns the model and the inference steps into a string of names or numbers
     *
     * @return the model together with the inference steps as string.
     */
    public String infoString(boolean withSymboltable) {
        Symboltable symboltable = withSymboltable ? this.symboltable : null;
        if(inferenceSteps.isEmpty())  return toString(symboltable);

        IntArrayList model = new IntArrayList();
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            int status = status(predicate);
            if(status != 0) model.add(status*predicate);}
        if(model.isEmpty()) return "";

        StringBuilder st = new StringBuilder();
        st.append("Model:\n");
        int size = model.size()-1;
        for(int i = 0; i <= size; ++i) {
            int literal = model.getInt(i);
            st.append(Symboltable.toString(literal,symboltable));
            InferenceStep step = getInferenceStep(literal);
            if(step != null) {
                st.append("\n").append(step.toString(symboltable));
                IntArrayList origins = step.origins();
                if(origins != null) st.append("\nOrigins: ").append(sortIntArray(origins).toString());
                ArrayList<InferenceStep> steps = new ArrayList<>();
                step.inferenceSteps(steps);
                if(!steps.isEmpty()) {
                    st.append("\nInference Steps: ");
                    for(InferenceStep stp : steps) st.append(stp.title()).append(",");}}
            if(i < size) st.append("\n");}
        return st.toString();}


}
