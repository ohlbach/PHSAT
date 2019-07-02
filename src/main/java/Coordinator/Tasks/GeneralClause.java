package Coordinator.Tasks;

import Datastructures.Results.Result;
import Management.Monitor;

import java.util.ArrayList;
import java.util.function.Function;
import java.util.function.Predicate;

/** This task treats general clauses to be simplified
 * Created by ohlbach on 02.07.2019.
 */
public class GeneralClause<Clause>  extends Task {
    /** a clause which may trigger further simplifications */
    private Clause clause;
    private Function<Clause,Result> handler;
    private Predicate<Clause> removedTest;

    /** constructs a task for using the clause to simplify other clauses
     *
     * @param clause       a clause with at least 3 literals
     * @param handler     which must process the clause
     * @param removedTest a predicate which can check if the clause has been removed.
     */
    public GeneralClause(Clause clause, Monitor monitor, String sourceId, Function<Clause,Result> handler, Predicate<Clause> removedTest) {
        super(5,monitor,sourceId);
        this.clause = clause;
        this.handler = handler;
        this.removedTest = removedTest;}

    /** If a literal in the clause became true or false, it has already been changed.
     *  If the clause has been removed, the task becomes obsolete.
     *
     * @param trueLiteral  ignored
     * @param tasks   for adding new tasks
     * @return false
     */
    public boolean makeTrue(int trueLiteral, ArrayList<Task> tasks) {
        if(removedTest != null && removedTest.test(clause)) {ignore = true;}
        return false;}

    /** calls the processor to use the clause for further simplifications
     *
     * @return  Un/Satisfiable if this has been detected, otherwise null
     */
    public Result execute() {super.execute();  return handler.apply(clause);}

    public String toString() {
        return "Task: General Clause " + clause.toString();}
}

