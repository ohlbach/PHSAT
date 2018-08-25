package Datastructures;

/**
 * Created by Ohlbach on 25.08.2018.
 */
public class Model {
    private int maxSize;    // the maximum number of predicates
    private int actualSize; // the current number of literals in the model
    private int[] model;    // the current model (as a stack)
    private short[] status;  // maps predicates in the model to +1 (true), -1 (false) or 0 (undefined)

    /** creates a model with a maximum number of predicates
     *
     * @param size the maximum number of predicates
     */
    public Model(int size) {
        assert size > 0;
        maxSize = size;
        model = new int[size];
        status = new short[size+1];}

    /** pushes a literal onto the model and checks if the literal is already in the model.
     *
     * @param literal the literal for the model.
     * @return +1 if the literal was already in the model, -1 if the negated literal was in the model, otherwise 0.
     */
    public short push(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= maxSize;
        assert actualSize <= maxSize;
        short tr = status[predicate];
        if(tr == 0){
            model[actualSize++] = literal;
            status[predicate] = literal > 0 ? (short)1: (short)-1;
            return 0;}
        else {return (Integer.signum(literal) == (int)tr) ? (short)1 : (short)-1;}}

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
        assert predicate <= maxSize;
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
        assert predicate <= maxSize;
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
        assert predicate <= maxSize;
        short status = this.status[predicate];
        return (short)(literal > 0 ? status : -status);}

    /** checks if the literal is in the model.
     *
     * @param literal
     * @return true if the model contains the literal.
     */
    public boolean contains(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= maxSize;
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
    public boolean isFull() {return actualSize == maxSize;}

    /**
     * @return the model as a comma separated string.
     */
    public String toString() {
        StringBuffer st = new StringBuffer();
        for(int i = 0; i < actualSize; ++i) {
            st.append(Integer.toString(model[i])).append(",");}
        return st.toString();}

}
