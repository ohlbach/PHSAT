package Solvers.Walker;

import Datastructures.Symboltable;

/** This class can be used to represent predicates with positive flip scores.
 * Together with the class Predicates it allows to represent lists of
 * predicates with positive flip scores and to add and remove predicates in constant time.
 *
 */
public class Predicate {

    /** the predicate itself.
     * predicate = 0 indicates that the predicate is not in the list.
     */
    public int predicate;
    /** the next predicate object in a bidirectional list */
    public Predicate nextPredicate;
    /** the previous object in a bidirectional list. */
    public Predicate previousPredicate;

    /** constructs a new predicate object.
     *
     * @param predicate the predicate itself.
     */
    public Predicate(int predicate) {
        this.predicate = predicate;}

    /**returns a string representation: predicate:-previousPredicate+nextPredicate.
     *
     * @return a string representation: predicate:-previousPredicate+nextPredicate.
     * */
    public String toString() {
        return toString(null);}

    /** returns a string representation: predicate:-previousPredicate+nextPredicate with symboltable.
     *
     * @param symboltable null or a symboltable
     * @return a string representation: predicate:-previousPredicate+nextPredicate.
     */
    public String toString(Symboltable symboltable) {
        String st = "";
        if(previousPredicate != null && previousPredicate.predicate != 0) {
            st += "-"+Symboltable.toString(previousPredicate.predicate,symboltable);}
        if(nextPredicate != null && nextPredicate.predicate != 0) {
            st += "+"+Symboltable.toString(nextPredicate.predicate,symboltable);}
        return ((predicate == 0) ? "0" : Symboltable.toString(predicate,symboltable)) +
                ((st.length() > 0) ? ":" : "") + st.toString();}


}
