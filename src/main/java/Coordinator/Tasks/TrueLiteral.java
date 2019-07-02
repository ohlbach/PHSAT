package Coordinator.Tasks;

import Datastructures.Results.Result;
import Management.Monitor;

import java.util.ArrayList;
import java.util.function.Function;

/** This task treats the processing of new unit literals.
 * They get priority 1, directly behind Satisfiability and Unsatisfiability.
 * Created by ohlbach on 02.07.2019.
 */
public class TrueLiteral  extends Task {
    /** the unit literal */
    private int literal;
    Function<Integer,Result> handler;

    /** constructs a unit-literal task.
     * It gets listPosition 1
     *
     * @param literal   the unit literal
     * @param handler which processes the unit literal
     */
    public TrueLiteral(int literal, Monitor monitor, String sourceId, Function<Integer,Result> handler) {
        super(1,monitor,sourceId);
        this.literal = literal;
        this.handler = handler;}

    public int trueLiteral() {return literal;}

    /** the unit literal may have become true or false in the meantime
     *
     * @param trueLiteral a true literal
     * @param tasks   for adding new tasks
     * @return true if a contradiction was detected
     */
    public boolean makeTrue(int trueLiteral, ArrayList<Task> tasks) {
        if(literal == trueLiteral) {ignore = true; return false;}
        if(literal == -trueLiteral) {return true;}
        return false;}

    /** calls the processor's processOneLiteralClause method
     *
     * @return Un/Satisfiable if this has ben detected, otherwise null
     */
    public Result execute() {super.execute();  return handler.apply(literal);}

    public String toString() {
        return "Task: New true Literal " + literal;}}

