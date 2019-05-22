package Solvers.RandomWalker;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;

import java.util.ArrayList;
import java.util.HashSet;

/** A rwModel is initialized as follows:</br>
 *  - a predicate occurring positively in more or equally many clauses than negatively, is made true.  </br>
 *  - Otherwise it is made false.
 *
 * Created by ohlbach on 07.05.2019.
 */
public class InitializerIsolated {
    private ClauseList clauses;

    public InitializerIsolated(ClauseList clauses){
        this.clauses = clauses;}


    /** generates a candidate rwModel for the clauses.
     * A predicate becomes true if it occurs in more (&ge;) clauses than its negation.
     */
    public void initializeModel(RWModel rwModel) {
        int predicates = rwModel.predicates();
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(rwModel.status[predicate] == 0) {
                int sizep = clauses.getLiterals(predicate).size();
                int sizen = clauses.getLiterals(-predicate).size();
                if(sizep == 0 && sizen == 0) {continue;}
                rwModel.status[predicate] = (byte)(sizep >= sizen ? 1 : -1);}}}

    /** initializes the flipScores, the false Clauses and the affected predicates
     * A flipScore for a predicate, say 5, is a number n if flipping its truth value makes n clauses more true than false
     *
     * @param rwModel       the current model
     * @param flipScores    an array with 0-entries
     * @param falseClauses  an empty array. It becomes the list of clauses which are false in the model
     * @param affectedPredicates the predicates in the false clauses.
     */
    public void initializeScores(RWModel rwModel, int[] flipScores, ArrayList<Clause> falseClauses, HashSet<Integer> affectedPredicates) {
        for(Clause clause : clauses.getClauses(0)) {
            int trueLiteral = 0;
            boolean remainsTrue = false;
            for(CLiteral lit : clause.cliterals) {
                int literal = lit.literal;
                if(rwModel.isTrue(literal)) {
                    if(trueLiteral != 0) {remainsTrue = true; break;} // at least two true literals: flipping changes nothing.
                    trueLiteral = literal;}}
            if(remainsTrue) {continue;}
            if(trueLiteral == 0) { // clause is false
                falseClauses.add(clause);
                for(CLiteral lit : clause.cliterals) {
                    int predicate = Math.abs(lit.literal);
                    ++flipScores[predicate];
                    affectedPredicates.add(predicate);}}
            else {  // clause is true and becomes false
                --flipScores[Math.abs(trueLiteral)];}}}

}
