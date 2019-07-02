package Coordinator.Tasks;

import Datastructures.Results.Result;
import Management.Monitor;

import java.util.ArrayList;
import java.util.function.Function;

/** The task treats purity removals.
 * Created by ohlbach on 02.07.2019.
 */
public class Purity extends Task {
    /** a pure literal */
    private int literal;
    private Function<Integer,Result> handler;

    /** constructs a task for purity removal
     *
     * @param literal a pure literal
     * @param handler which must process the purity removal
     */
    public Purity(int literal, Monitor monitor, String sourceId, Function<Integer,Result> handler) {
        super(2,monitor,sourceId);
        this.literal = literal;
        this.handler = handler;}

    public Result execute() {super.execute();  return handler.apply(literal);}

    public String toString() {
        return "Task Purity: " + literal;}

    /** The pure literal may already have become true or false.
     * A pure literal may become true, but there may also be models where the literal is false.
     * Therefore a pure literal which has become false is not a contradiction.
     * Nevertheless if the literal has become true/false, the task becomes obsolete.
     *
     * @param trueLiteral  a true literal
     * @param tasks   for adding new tasks
     * @return false
     */
    public boolean makeTrue(int trueLiteral, ArrayList<Task> tasks) {
        if(literal == trueLiteral || literal == -trueLiteral ) {ignore = true;}
        return false;}}
