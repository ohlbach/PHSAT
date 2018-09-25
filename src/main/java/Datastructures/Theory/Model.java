package Datastructures.Theory;

import java.util.ArrayList;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Consumer;

/** This class represents a propositional model, i.e. a set of literals which are supposed to be true.<br/>
 * The access is synchronized with read- and write-locks. <br/>
 * A writer blocks all readers, but several readers may read in parallel. <br/>
 *
 * Created by ohlbach on 12.09.2018.
 */
public class Model {
    public final int predicates;    // the maximum number of predicates
    private final ArrayList<Integer> model;    // the current model (as a stack)
    private final short[] status;  // maps predicates in the model to +1 (true), -1 (false) or 0 (undefined)
    private final ReentrantReadWriteLock rwl = new ReentrantReadWriteLock();
    private final Lock readLock = rwl.readLock();
    private final Lock writeLock = rwl.writeLock();

    /** creates a model with a maximum number of predicates
     *
     * @param predicates the maximum number of predicates
     */
    public Model(int predicates) {
        assert predicates > 0;
        this.predicates = predicates;
        model = new ArrayList<>();
        status = new short[predicates+1];}

    public void readLock() {readLock.lock();}
    public void readUnLock() {readLock.unlock();}

    private ArrayList<Consumer<Integer>> newTruthObserver = new ArrayList<>();

    public synchronized void addNewTruthObserver(Consumer<Integer> observer) {
        newTruthObserver.add(observer);}

    private void reportNewTruth(int literal) {
        for(Consumer<Integer> observer : newTruthObserver) {observer.accept(literal);}}

    /** pushes a literal onto the model and checks if the literal is already in the model.
     *
     * @param literal the literal for the model.
     * @return +1 if the literal was already in the model, -1 if the negated literal was in the model, otherwise 0.
     */
    public short add(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        writeLock.lock();
        try{
            short tr = status[predicate];
            if(tr == 0){
                model.add(literal);
                status[predicate] = literal > 0 ? (short)1: (short)-1;
                reportNewTruth(literal);
                return 0;}
            else {return (Integer.signum(literal) == (int)tr) ? (short)1 : (short)-1;}}
        finally{writeLock.unlock();}}

    /** returns the entire model, i.e. the list of true literals.
     * Access to the list, however is not synchronized.
     *
     * @return the (unsynchronized) model
     */
    public ArrayList<Integer> getModel() {return model;}

    /** checks if the literal is true in the model.
     *
     * @param literal the literal to be checked.
     * @return true if the literal is true in the model.
     */
    public boolean isTrue(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        readLock.lock();
        try{
            short status = this.status[predicate];
            if(status == 0) {return false;}
            return literal > 0 ? status == 1: status == -1;}
        finally{readLock.unlock();}}

    /** checks if the literal is false in the model.
     *
     * @param literal the literal to be checked.
     * @return true if the literal is false in the model.
     */
    public boolean isFalse(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        readLock.lock();
        try{
            short status = this.status[predicate];
            if(status == 0) {return false;}
            return literal > 0 ? status == -1: status == 1;}
        finally{readLock.unlock();}}

    /** returns the status of the literal in the model.
     *
     * @param literal the literal to be checked
     * @return +1 if the literal is true in the model, -1 if it is false in the model, otherwise 0.
     */
    public short status(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        readLock.lock();
        try{
            short status = this.status[predicate];
            return (short)(literal > 0 ? status : -status);}
        finally{readLock.unlock();}}

    /** returns the current size of the model.
     *
     * @return the current size of the model
     */
    public short[] cloneStatus() {
        readLock.lock();
        try{return status.clone();}
        finally{readLock.unlock();}}

    /** checks if the literal is in the model.
     *
     * @param literal
     * @return true if the model contains the literal.
     */
    public boolean contains(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        readLock.lock();
        try{return status[predicate] != 0;}
        finally{readLock.unlock();}}

    /** returns the current size of the model.
     *
     * @return the current size of the model
     */
    public int size() {
        readLock.lock();
        try{return model.size();}
        finally{readLock.unlock();}}

    /** checks if the model is empty.
     *
     * @return true if the model is empty
     */
    public boolean isEmpty() {
        readLock.lock();
        try{return model.isEmpty();}
        finally{readLock.unlock();}}

    /** checks if the model is full.
     *
     * @return true if the model is empty
     */
    public boolean isFull() {
        readLock.lock();
        try{return model.size() == predicates;}
        finally{readLock.unlock();}}


    /**
     * @return the model as a comma separated string.
     */
    public String toString() {return model.toString();}
}
