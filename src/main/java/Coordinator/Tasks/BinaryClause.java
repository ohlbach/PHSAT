package Coordinator.Tasks;

import Coordinator.Processor;
import Datastructures.Results.Result;
import Management.Monitor;

import java.util.ArrayList;
import java.util.function.BiFunction;
import java.util.function.Function;

/** This task treats binary clauses, to be inserted into the implication dag.
 * Created by ohlbach on 02.07.2019.
 */
public class BinaryClause extends Task {
    /** the two literals of the clause */
    private  int literal1,literal2;
    private BiFunction<Integer,Integer,Result> handler;
    private Function<Integer,Result> trueLiteralHandler;

    /** constructs a two-literal task
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @param handler which has to process the task
     * @param trueLiteralHandler which has to process unit clauses derived from the binary clause
     */
    public BinaryClause(int literal1, int literal2, Monitor monitor, String sourceId, BiFunction<Integer, Integer, Result> handler, Function<Integer,Result> trueLiteralHandler ) {
        super(4,monitor,sourceId);
        this.literal1 = literal1;
        this.literal2 = literal2;
        this.handler = handler;
        this.trueLiteralHandler = trueLiteralHandler;}

    /** one or both literals may have become true/false in the meantime.
     * In this case one-literal task are generated.
     *
     * @param trueLiteral a true literal
     * @param tasks   for adding new tasks
     * @return false
     */
    public boolean makeTrue(int trueLiteral, ArrayList<Task> tasks) {
        if(trueLiteral == literal1 || trueLiteral == literal2) {ignore = true; return false;}
        if(trueLiteral == -literal1) {
            tasks.add(new TrueLiteral(literal2,trueLiteralHandler));
            ignore = true;
            return false;}
        if(trueLiteral == -literal2) {
            tasks.add(new TrueLiteral(literal1,trueLiteralHandler));
            ignore = true;
            return false;}
        return false;}

    /** calls the processor to process the two-literal clause
     *
     * @return  Un/Satisfiable if this has been detected, otherwise null
     */
    public Result execute() {
        super.execute();
        return handler.apply(literal1,literal2);}

    public String toString() {
        return "Task: Binary Clause " + literal1 + "," + literal2;}
}
