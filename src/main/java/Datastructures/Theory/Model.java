package Datastructures.Theory;

import Datastructures.Symboltable;

import java.util.ArrayList;
import java.util.function.Consumer;

/** This class represents a propositional model, i.e. a set of literals which are supposed to be true.<br>
 *
 * Created by ohlbach on 12.09.2018.
 */
public class Model {
    /** the maximum number of predicates */
    public int predicates = 0;
    /** the current model */
    private ArrayList<Integer> model = null;
    /** maps predicates in the model to +1 (true), -1 (false) or 0 (undefined) */
    private byte[] status = null;

    /** creates a model with a maximum number of predicates
     *
     * @param predicates the maximum number of predicates
     */
    public Model(int predicates) {
        assert predicates > 0;
        this.predicates = predicates;
        model = new ArrayList<>();
        status = new byte[predicates+1];}

    /* Observers
       ********* */

    /** the observers are called when a new literal is inserted in the model */
    private ArrayList<Consumer<Integer>> trueLiteralObservers = new ArrayList<>();

    /** adds a new observer. The method is synchronized
     *
     * @param observer the new observer
     */
    public synchronized void addTrueLiteralObserver(Consumer<Integer> observer) {
        trueLiteralObservers.add(observer);}

    /** removes an observer. The method is synchronized
     *
     * @param observer the new observer
     */
    public synchronized void removeTrueLiteralObserver(Consumer<Integer> observer) {
        trueLiteralObservers.remove(observer);}


    /** calls all observers when a new literal has been inserted.
     *
     * @param literal a new literal in the model.
     */
    private void reportNewTruth(int literal) {
        for(Consumer<Integer> observer : trueLiteralObservers) {observer.accept(literal);}}

    /** pushes a literal onto the model and checks if the literal is already in the model.
     *
     * @param literal the literal for the model.
     * @return +1 if the literal was already in the model, -1 if the negated literal was in the model, otherwise 0.
     */
    public int add(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        short tr = status[predicate];
        if(tr == 0){
            model.add(literal);
            status[predicate] = literal > 0 ? (byte)1: (byte)-1;
            reportNewTruth(literal);
            return 0;}
        else {return (Integer.signum(literal) == (int)tr) ? 1 : -1;}}

    /** returns the entire model, i.e. the list of true literals.
     * Access to the list, however is not synchronized.
     *
     * @return the model
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
    public int status(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        short status = this.status[predicate];
        return literal > 0 ? status : -status;}

    /** sets the logical status of the literal.
     *
     * @param literal a literal
     * @param status +1 (for true) and -1 (for false)
     */
    public void setStatus(int literal, int status) {
        if(literal < 0) {literal = -literal; status = (byte)-status;}
        assert this.status[literal] == 0 || this.status[literal] == status;
        if(this.status[literal] == 0) {
            this.status[literal] = (byte)status;
            model.add(status > 0 ? literal : -literal);}}



    /** clones the model (without observers)
     *
     * @return a clone of the model
     */
    public Model clone() {
        Model newModel = new Model(predicates);
        newModel.status = status.clone();
        newModel.model = (ArrayList<Integer>)model.clone();
        return newModel;}

    /** returns a clone of the current status of the model.
     *
     * @return a clone of the current status of the model
     */
    public byte[] cloneStatus() {return status.clone();}

    /** checks if the literal is in the model.
     *
     * @param literal a literal
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
    public int size() {return model.size();}

    /** checks if the model is empty.
     *
     * @return true if the model is empty
     */
    public boolean isEmpty() {return model.isEmpty();}

    /** checks if the model is full.
     *
     * @return true if the model is empty
     */
    public boolean isFull() {return model.size() == predicates;}


    /**
     * @return the model as a comma separated string.
     */
    public String toString() {return model.toString();}

    /** returns the model as a comma separated string of names
     *
     * @param symboltable a symboltable
     * @return the model as a comma separated string of names
     */
    public String toString(Symboltable symboltable) {
        if(symboltable == null) {return model.toString();}
        StringBuilder st = new StringBuilder();
        for(int i = 0; i < model.size()-1; ++i) {
            st.append(symboltable.getLiteralName(model.get(i))).append(",");}
        st.append(symboltable.getLiteralName(model.get(model.size()-1)));
        return st.toString();}


}
