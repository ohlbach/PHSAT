package Datastructures.Results;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;

/** This class reports about an unsatisfiable clause, for example an empty clause.*/

public class UnsatisfiableClause extends Unsatisfiable {
    private Clause clause = null;
    private int[] inputClause = null;

    /** constructs an Unsatisfiability from an unsatisfiable clause
     *
     * @param clause an unsatisfiable clause
     */
    public UnsatisfiableClause(String problemId, String solverId, Clause clause) {
        super(problemId,solverId);
        this.clause = clause;}

    public UnsatisfiableClause(String problemId, String solverId, int[] inputClause) {
        super(problemId,solverId);
        this.inputClause = inputClause;}


    @Override
    public String description(Symboltable symboltable) {
        return "Unsatisfiable clause " +
                ((clause == null) ? InputClauses.toString(0,inputClause,symboltable) :
                clause.toString(0,symboltable)+"\n");}


}
