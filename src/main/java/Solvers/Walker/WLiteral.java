package Solvers.Walker;

import Datastructures.Clauses.ClauseOld;
import Datastructures.Literals.CLiteralOld;

/**
 * Created by ohlbach on 27.01.2020.
 */
public class WLiteral extends CLiteralOld {

    public int score = 0;

    /** creates a CLiteral without a clause
     *
     * @param literal the literal
     */
    public WLiteral(int literal) {
        super(literal);}



    /** creates a CLiteral and sets the clause
     *
     * @param literal    the literal
     * @param clause     the clause containing the literal
     * @param position   the clausePosition of the literal within the clause
     */
    public WLiteral(int literal, ClauseOld clause, int position) {
        super(literal,clause,position);}

}
