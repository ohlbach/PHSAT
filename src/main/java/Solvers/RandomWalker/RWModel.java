package Solvers.RandomWalker;

import Datastructures.Theory.Model;

/** Represents a simple boolean model,
 * where the truth values are represented by signs in an array (+1 = true, -1 = false)
 * Created by ohlbach on 07.05.2019.
 */
public class RWModel {
    /** status of a predicate: -1: false, +1: true, */
    public byte[] status = null;

    /** constructs an unassigned model with the given number of predicates
     *
     * @param predicates the number of predicates: 1...predicates
     */
    public RWModel(int predicates) {
        status = new byte[predicates+1];}

    /** constructs an RWModel from a given Model.
     *
     * @param model a model
     */
    public RWModel(Model model) {status = model.cloneStatus();}

    /** checks if the truth value is not yet assigned for the literal
     *
     * @param literal a literal
     * @return true if there is not yet a truth value.
     */
    public boolean isUnassigned(int literal) {return status[Math.abs(literal)] == 0;}

    /** checks if the literal is true in the model
     *
     * @param literal a literal
     * @return true if the literal is true in the model.
     */
    public boolean isTrue(int literal) {
        short status = this.status[Math.abs(literal)];
        return literal > 0 ? status == 1: status == -1;}

    /** checks if the literal is false in the model
     *
     * @param literal a literal
     * @return true if the literal is false in the model.
     */
    public boolean isFalse(int literal) {
        short status = this.status[Math.abs(literal)];
        return literal > 0 ? status == -1: status == 1;}

    /** makes the literal true in the model
     *
     * @param literal a literal to be made true.
     */
    public void makeTrue(int literal) {
        if(literal > 0) {status[literal] = 1;}
        else {status[-literal] = -1;}}

    /** makes the literal false in the model
     *
     * @param literal a literal to be made false.
     */
    public void makeFalse(int literal) {
        if(literal > 0) {status[literal] = -1;}
        else {status[-literal] = 1;}}

    /** flips the truth value for the literal
     *
     * @param literal a literal
     */
    public void flip(int literal) {
        status[Math.abs(literal)] *= -1;}

    /** returns the number of predicates in the model
     *
     * @return the number of predicates in the model
     */
    public int predicates() {return status.length-1;}

    /** trurn the model into a string of positive/negative integers.
     *
     * @return the model as a string.
     */
    public String toString(){
        StringBuffer st = new StringBuffer();
        for(int predicate = 1; predicate < status.length; ++predicate){
            if(status[predicate] == 0) {continue;}
            if(status[predicate]<0){st.append("-");}
            st.append(Integer.toString(predicate)).append(",");}
        return st.toString();}


}
