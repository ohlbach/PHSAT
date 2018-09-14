package Datastructures.Clauses;

import Datastructures.Literals.CGLiteral;

/**
 * Created by ohlbach on 29.08.2018.
 */
public class ClauseCG extends Clause {

    public ClauseCG(int number, int maxSize) {
        super(number,maxSize);
    }

    /** adds a literal to the clause.
     * The clause and the literal position within the clause are updated in the literal datastructure.
     * Double literals and tautologies are checked.
     *
     * @param cliteral the literal to be added.
     * @return 1 if the literal was already in the clause, -1 if the clause becomes a tautology, 0 otherwise.
     */
    public int addLiteral(CGLiteral cliteral) {
        int status = super.addLiteral(cliteral);
        return status;

    }
}
