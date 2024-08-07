package Datastructures.Results;

import Datastructures.Clause;
import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.function.Consumer;

/** This class describes an Unsatisfiablitiy caused by a clause.
 * Example: atleast 3 p,q
 */
public class UnsatClause extends Unsatisfiable {


    /** an unsatisfiable input clause */
    private int[] inputClause = null;

    /** an unsatifiable clause */
    private Clause clause = null;

    /** constructs an Unsatisfiability from an unsatisfiable input clause
     *
     * @param problemId the promblem's id
     * @param solverId the solvers's id
     * @param inputClause an unsatisfiable clause
     */
    public UnsatClause(String problemId, String solverId, int[] inputClause) {
        super(problemId, solverId);
        this.inputClause = inputClause;
        message = (symboltable -> "Unsatisfiable Clause: " + InputClauses.toString(0,inputClause,symboltable));
    }

    /** constructs an Unsatisfiability from an unsatisfiable  clause
     *
     * @param problemId the promblem's id
     * @param solverId the solvers's id
     * @param clause an unsatisfiable clause
     */
    public UnsatClause(String problemId, String solverId, Clause clause) {
        super(problemId,solverId);
        this.clause = clause;
        message = (symboltable -> "Unsatisfiable Clause: " + clause.toString(symboltable,0));}

    /**
     * Verifies the unsatisfiability of a clause. If the clause is satisfiable, a monitoring message is
     * sent to the provided monitor.
     *
     * @param monitor     the consumer of the monitoring messages
     * @param symboltable the symbol table used for predicate name mapping
     * @return true if the clause is unsatisfiable, false if it is satisfiable
     */
    public boolean verify(Consumer<String>monitor, Symboltable symboltable) {
        IntArrayList predicates = (inputClause != null) ? InputClauses.predicates(inputClause) : clause.predicates();
        int nModels = 1 << predicates.size();
        for(int model = 0; model < nModels; ++model) {
            if(clause.isTrue(model,predicates)) {
                String cl = (inputClause != null) ? InputClauses.toString(0,inputClause,symboltable) : clause.toString(symboltable,0);
                monitor.accept("Clause " + cl + " which is supposed to be unsatisfiable is satisfied by the model "+
                        clause.modelString(model,predicates, symboltable));
                return false;}}
        return true;}


}
