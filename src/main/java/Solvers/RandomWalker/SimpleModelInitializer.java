package Solvers.RandomWalker;

import Datastructures.Clauses.ClauseList;

/** A rwModel is initialized as follows:</br>
 *  - a predicate occurring positively in more or equally many clauses than negatively, is made true.  </br>
 *  - Otherwise it is made false.
 *
 * Created by ohlbach on 07.05.2019.
 */
public class SimpleModelInitializer {
    private ClauseList clauses;

    public SimpleModelInitializer(ClauseList clauses){
        this.clauses = clauses;}


    /** generates a candidate rwModel for the clauses.
     * A predicate becomes true if it occurs in more clauses than its negation.
     */
    public void initializeModel(RWModel rwModel) {
        int predicates = rwModel.predicates();
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(rwModel.status[predicate] == 0) {
                int sizep = clauses.getLiterals(predicate).size();
                int sizen = clauses.getLiterals(-predicate).size();
                if(sizep == 0 && sizen == 0) {continue;}
                rwModel.status[predicate] = (byte)(sizep >= sizen ? 1 : -1);}}}
}
