package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Theory.ImplicationDAG;
import Solvers.RandomWalker.RWModel;

/** This class is for initializing a model by taking into account the implication dag.
 * A predicate becomes true if it itself together with its implied literals occur more often in the clauses than its negation.
 * Created by ohlbach on 07.05.2019.
 */
public class IDModelInitializer {

    private ClauseList clauses;
    private ImplicationDAG implicationDAG;
    private int timestamp = 0;

    /** creates an initializer
     *
     * @param clauses        the clauses
     * @param implicationDAG the implication dag
     */
    public IDModelInitializer(ClauseList clauses, ImplicationDAG implicationDAG) {
        this.clauses = clauses;
        this.implicationDAG = implicationDAG;}

    /** generates a candidate rwModel for the clauses.
     * A predicate becomes true if it itself together with its implied literals occur more often in the clauses than its negation.
     *
     * @param rwModel   the model to be initialized
     * @param timestamp a timestamp
     * @return          the changed timestamp
     */
    public int initializeModel(RWModel rwModel, int timestamp) {
        int predicates = rwModel.predicates();
        this.timestamp = timestamp;

        implicationDAG.applyToRoots(literal -> {
            if(rwModel.isUnassigned(literal)) {
                literal = getOccurrences(literal) >= getOccurrences(-literal) ? literal : -literal;
                implicationDAG.apply(literal,true,(lit-> {
                    rwModel.status[ Math.abs(lit)] = (byte)(lit > 0 ? 1 : -1);}));}});

        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(rwModel.isUnassigned(predicate)) {
                int sizep = clauses.getLiterals(predicate).size();
                int sizen = clauses.getLiterals(-predicate).size();
                if(sizep == 0 && sizen == 0) {continue;}
                rwModel.status[predicate] = (byte)(sizep >= sizen ? 1 : -1);}}
        return this.timestamp;}


    private int[] counter = new int[]{0};

    /** counts the clauses containing the literal and its implied literals
     *
     * @param literal a literal
     * @return the number of clauses containing the literal and its implied literals.
     */
    public int getOccurrences(int literal) {
        ++timestamp;
        counter[0] = 0;
        implicationDAG.apply(literal,true,(lit-> {
            for(CLiteral cLiteral : clauses.getLiterals(lit)){
                Clause clause = cLiteral.clause;
                if(clause.timestamp != timestamp) {
                    clause.timestamp = timestamp;
                    ++counter[0];}}}));
        return counter[0];}
}
