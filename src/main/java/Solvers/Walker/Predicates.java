package Solvers.Walker;

import Datastructures.Symboltable;

/** This class is for representing a list of predicates with positive flip scores.
 * The bidirectional list allows one to add and remove predicates in constant time.
 */
public class Predicates {

    /** contains all Predicate objects, one for each predicate.*/
    public Predicate[] predicates;

    /** the first predicate object in a bidirectional list. */
    public Predicate firstPredicate;
    /** the last predicate object in a bidirectional list. */
    public Predicate lastPredicate;

    /** constructs the predicate objects, one for each predicate.
     *
     * @param predicates the total number of predicates.
     */
    public Predicates(int predicates) {
        this.predicates = new Predicate[predicates+1];
        for(int i = 1; i <= predicates; ++i) {this.predicates[i] = new Predicate(0);}}

    /** adds a predicate to the front of the list.
     *
     * @param predicate the predicate to be added to the list.
     */
    public void addToFront(int predicate) {
        assert predicate > 0;
        Predicate predicateObject = predicates[predicate];
        predicateObject.predicate = predicate;
        predicateObject.previousPredicate = null; predicateObject.nextPredicate = null;
        if(firstPredicate == null) {firstPredicate = predicateObject; lastPredicate = predicateObject; return;}
        if(firstPredicate.nextPredicate == null) {lastPredicate = firstPredicate;}
        predicateObject.nextPredicate = firstPredicate;
        firstPredicate.previousPredicate = predicateObject;
        firstPredicate = predicateObject;}

    /** adds a predicate to the back of the list.
     *
     * @param predicate the predicate to be added to the list.
     */
    public void addToBack(int predicate) {
        assert predicate > 0;
        Predicate predicateObject = predicates[predicate];
        predicateObject.predicate = predicate;
        predicateObject.previousPredicate = null; predicateObject.nextPredicate = null;
        if(firstPredicate == null) {firstPredicate = predicateObject; lastPredicate = predicateObject; return;}
        if(firstPredicate.nextPredicate == null) {lastPredicate = firstPredicate;}
        predicateObject.previousPredicate = lastPredicate;
        lastPredicate.nextPredicate = predicateObject;
        lastPredicate = predicateObject;}

    /** removes the predicate from the list.
     *  The predicate of the corresponding predicate object is set to 0.
     *
     * @param predicate the predicate to be removed.
     */
    public void remove(int predicate) {
        assert predicate > 0;
        Predicate predicateObject = predicates[predicate];
        predicateObject.predicate = 0;
        if(lastPredicate == firstPredicate) {lastPredicate = null; firstPredicate = null; return;}
        if(predicateObject.previousPredicate == null) {
            firstPredicate = predicateObject.nextPredicate;
            return;}
        if(predicateObject.nextPredicate == null) {
            lastPredicate = predicateObject.previousPredicate;
            lastPredicate.nextPredicate = null;
            return;}
        Predicate pred = predicateObject.previousPredicate;
        pred.nextPredicate = predicateObject.nextPredicate;
        predicateObject.nextPredicate.previousPredicate = pred;}

    /** returns a string representation of the list.
     *
     * @return a string representation of the list.
     */
    public String toString() {
        return toString(null);}

    /** returns a string representation of the list, with symboltable.
     *
     * @param symboltable null or a symboltable.
     * @return a string representation of the list.
     */
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        if(firstPredicate == null) return "";
        Predicate predicateObject = firstPredicate;
        while(predicateObject != null) {
            int predicate = predicateObject.predicate;
            st.append(predicate == 0 ? "0" : Symboltable.toString(predicate,symboltable)).append(",");
            predicateObject = predicateObject.nextPredicate;}
        return st.toString();}
}
