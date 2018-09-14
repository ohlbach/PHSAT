package Datastructures;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Created by Ohlbach on 25.08.2018.
 */
public class LocalModel {
    public int predicates;    // the maximum number of predicates
    private int actualSize; // the current number of literals in the model
    private int[] model;    // the current model (as a stack)
    private short[] status;  // maps predicates in the model to +1 (true), -1 (false) or 0 (undefined)
    private ArrayList<Consumer<Integer>> pushObserver = null;
    private ArrayList<BiConsumer<String,Integer>> satisfiableObserver = null;
    private ArrayList<BiConsumer<String,Integer>> unsatisfiableObserver = null;

    public LocalModel() {}

    /** creates a model with a maximum number of predicates
     *
     * @param predicates the maximum number of predicates
     */
    public LocalModel(int predicates) {
        assert predicates > 0;
        this.predicates = predicates;
        model = new int[predicates];
        status = new short[predicates+1];}

    /** creates a copy of the model.
     *  Observers are not copied.
     *
     * @return a copy of the model.
     */
    public LocalModel copy() {
        LocalModel newmodel = new LocalModel();
        newmodel.predicates = predicates;
        newmodel.actualSize = actualSize;
        newmodel.model = Arrays.copyOf(model,predicates);
        newmodel.status = Arrays.copyOf(status,predicates+1);
        return newmodel;}

    /** adds a pushObserver, a Consumer function to be applied to a literal which just became true.
     *
     * @param observer a Consumer function to be applied to a literal.
     */
    public synchronized void addPushObserver(Consumer<Integer> observer) {
        if(pushObserver == null) {pushObserver = new ArrayList<>();}
        pushObserver.add(observer);}

    /** adds a satisfiableObserver, a Consumer function to be applied either when all predicates became assigned,
     * or when the clause list became empty.
     *
     * @param observer a Consumer function to be applied to a string ("full" or "empty") and the last literal.
     */
    public synchronized void addSatisfiableObserver(BiConsumer<String,Integer> observer) {
        if(satisfiableObserver == null) {satisfiableObserver = new ArrayList<>();}
        satisfiableObserver.add(observer);}

    /** adds an unsatisfiableObserver, a Consumer function to be applied when an inconsistency has been detected
     *
     * @param observer a Consumer function to be applied to a string ("literal" or "clause") and either an inconsistent
     *                 literal or the empty clause number.
     */
    public synchronized void addUnsatisfiableObserver(BiConsumer<String,Integer> observer) {
        if(unsatisfiableObserver == null) {unsatisfiableObserver = new ArrayList<>();}
        unsatisfiableObserver.add(observer);}

    /** pushes a literal onto the model and checks if the literal is already in the model.
     *
     * @param literal the literal for the model.
     * @return +1 if the literal was already in the model, -1 if the negated literal was in the model, otherwise 0.
     */
    public synchronized short push(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        assert actualSize <= predicates;
        short tr = status[predicate];
        if(tr == 0){
            model[actualSize++] = literal;
            status[predicate] = literal > 0 ? (short)1: (short)-1;
            if(pushObserver != null) {
                for(Consumer<Integer> observer : pushObserver) {observer.accept(literal);}}
            if(actualSize == predicates && satisfiableObserver != null) {
                for(BiConsumer<String,Integer> observer : satisfiableObserver) {observer.accept("full",literal);}}
            return 0;}
        else {return (Integer.signum(literal) == (int)tr) ? (short)1 : (short)-1;}}

    /** flips the truth value of the literal
     *
     * @param literal a literal
     */
    public void flip(int literal) {
        int predicate = Math.abs(literal);
        status[predicate] = (short)-status[predicate];}

    /** pops the last literal from the model.
     *
     *  @return the popped literal, or 0 if the model is empty.
     */
    public int pop() {
        if(actualSize == 0) {return 0;}
        int literal = model[--actualSize];
        status[Math.abs(literal)] = 0;
        return literal;}

    /** checks if the literal is true in the model.
     *
     * @param literal the literal to be checked.
     * @return true if the literal is true in the model.
     */
    public boolean isTrue(int literal) {
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
    public boolean isFalse(int literal) {
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
    public short status(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        short status = this.status[predicate];
        return (short)(literal > 0 ? status : -status);}

    /** checks if the literal is in the model.
     *
     * @param literal
     * @return true if the model contains the literal.
     */
    public boolean contains(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        return status[predicate] != 0;}

    /** returns the current size of the model.
     *
     * @return the current size of the model
     */
    public int size() {return actualSize;}

    /** checks if the model is empty.
     *
     * @return true if the model is empty
     */
    public boolean isEmpty() {return actualSize == 0;}

    /** checks if the model is full.
     *
     * @return true if the model is empty
     */
    public boolean isFull() {return actualSize == predicates;}

    public void signalContradiction(int literal) {
        if(unsatisfiableObserver != null) {
            for(BiConsumer<String,Integer> consumer : unsatisfiableObserver) {
                consumer.accept("literal",literal);}}}

    public void signalEmptyClause(int clause) {
        if(unsatisfiableObserver != null) {
            for(BiConsumer<String,Integer> consumer : unsatisfiableObserver) {
                consumer.accept("clause",clause);}}}


    public void signalEmptyClauseList() {
        if(satisfiableObserver != null) {
            for(BiConsumer<String,Integer> consumer : satisfiableObserver) {
                consumer.accept("empty",null);}}}
    /**
     * @return the model as a comma separated string.
     */
    public String toString() {
        StringBuffer st = new StringBuffer();
        for(int i = 0; i < actualSize; ++i) {
            st.append(Integer.toString(model[i])).append(",");}
        return st.toString();}

}
