package Coordinator;

import java.util.ArrayList;

/**
 * Created by ohlbach on 12.09.2018.
 */
public class GlobalModel {
    public int predicates;    // the maximum number of predicates
    private ArrayList<Integer> model;    // the current model (as a stack)
    private short[] status;  // maps predicates in the model to +1 (true), -1 (false) or 0 (undefined)


    /** creates a model with a maximum number of predicates
     *
     * @param predicates the maximum number of predicates
     */
    public GlobalModel(int predicates) {
        assert predicates > 0;
        this.predicates = predicates;
        model = new ArrayList<>();
        status = new short[predicates+1];}

    /** pushes a literal onto the model and checks if the literal is already in the model.
     *
     * @param literal the literal for the model.
     * @return +1 if the literal was already in the model, -1 if the negated literal was in the model, otherwise 0.
     */
    public short add(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= predicates;
        short tr = status[predicate];
        if(tr == 0){
            model.add(literal);
            status[predicate] = literal > 0 ? (short)1: (short)-1;
            return 0;}
        else {return (Integer.signum(literal) == (int)tr) ? (short)1 : (short)-1;}}

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
}
