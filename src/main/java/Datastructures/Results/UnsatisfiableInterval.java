package Datastructures.Results;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;

/** This class documents a contradiction arsing from two clauses with the same literals, but disjoint intervals
 */
public class UnsatisfiableInterval extends Unsatisfiable{
    private final Clause clause1;
    private final Clause clause2;

    /** constructs an Unsatisfiability from two clauses with the same literals, but disjoint intervals
     *
     * @param clause1 a clause
     * @param clause2 a clause
     */
    public UnsatisfiableInterval(String problemId, String solverId, Clause clause1, Clause clause2) {
        super(problemId,solverId);
        this.clause1 = clause1;
        this.clause2 = clause2;}

    @Override
    public String description(Symboltable symboltable) {
        int width = Math.max(clause1.getName().length(),clause2.getName().length());
        return "Two clauses with the same literals have disjoint intervals:\n" +
                clause1.toString(width,symboltable) + "\n"+
                clause2.toString(width,symboltable);}


}
