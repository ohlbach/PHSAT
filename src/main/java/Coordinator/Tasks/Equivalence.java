package Coordinator.Tasks;

import Datastructures.Results.Result;
import Management.Monitor;
import Utilities.Utilities;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.function.Function;

/** This task class  deals with newly derived equivalences.
 * Created by ohlbach on 02.07.2019.
 */
public class Equivalence  extends Task {
    /** the equivalence class. The first literal servs as representative */
    private int[] equivalences;

    private Function<int[],Result> handler;
    private Function<Integer,Result> trueLiteralHandler;

    /** constructs an Equivalence Task
     *
     * @param equivalences  an equivalence class
     * @param handler which executes the equivalence processing
     * @param trueLiteralHandler which treats true literals if the equivalence collapses to true literals.
     */
    public Equivalence(int[] equivalences, Monitor monitor, String sourceId, Function<int[],Result> handler, Function<Integer,Result> trueLiteralHandler) {
        super(3,monitor,sourceId);
        this.equivalences = equivalences;
        this.handler = handler;
        this.trueLiteralHandler = trueLiteralHandler;}

    /** Some of the literals in the equivalence class may have become true or false in the meantime.
     * In this case the other literals in the class become also true/false.
     *
     * @param trueLiteral    a true literal
     * @param tasks   for adding new tasks
     * @return false
     */
    public boolean makeTrue(int trueLiteral, ArrayList<Task> tasks) {
        int position = Utilities.contains(equivalences,trueLiteral);
        if(position >= 0) {
            for(int i = 0; i < equivalences.length; ++i) {
                if(i != position) {tasks.add(new TrueLiteral(equivalences[i],trueLiteralHandler));}}
            ignore = true;
            return false;}
        position = Utilities.contains(equivalences,-trueLiteral);
        if(position >= 0) {
            for(int i = 0; i < equivalences.length; ++i) {
                if(i != position) {tasks.add(new TrueLiteral(-equivalences[i],trueLiteralHandler));}}
            ignore = true;
            return false;}
        return false;}

    /** asks the processor to deal with the equivalence
     *
     * @return Un/Satisfiable if this has been detected, otherwise null
     */
    public Result execute() {super.execute();  return handler.apply(equivalences);}

    public String toString() {
        return "Task: Equivalences " + Arrays.toString(equivalences);}
}

