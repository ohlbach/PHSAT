package Datastructures.Results;

import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.Function;

/** This is the superclass of the various possible results the solvers can generate.
 * The most important subclasses represent the satisfiability and unsatisfiability of a clause set.
 */
public class Result extends Exception {
    /** to name the result */
    public String result = "unknown";
    /** the solver which found the result. */
    public String solverId = null;
    /** the problem identifier which has the result. */
    public String problemId = null;

     /** the statistics of the solver. */
    public Statistic statistic = null;

    /** the nanotime at which the problem was ended*/
    public long endTime = 0L;

    /** an optional message */
    public Function<Symboltable,String> message = null;

    public ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();
    /** the constructor. It sets the endTime.
     *
     * @param problemId the problem identifier which has the result.
     * @param solverId the solver which found the result
     * @param result a name of the result (eg. satisfiable, aborted etc.)
     */
    public Result(String problemId, String solverId, String result) {
        super();
        this.problemId = problemId;
        this.solverId  = solverId;
        this.result    = result;
        endTime        = System.nanoTime();}

    /** completes the information about the result
     *
     * @param problemId the problem identifier which has the result.
     * @param solverId the solver which found the result
     */
    public void complete(String problemId, String solverId) {
        this.problemId = problemId;
        this.solverId = solverId;}

    /** Collects the inference steps, the input clause ids and the reasoners which contributed to the result.
     * <p>
     * Double occurrences are removed.
     * @param steps for collecting the inference steps
     * @param ids for collecting the input clause ids
     * @param reasoners for collecting the names of the reasoners.
     */
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids, ArrayList<String> reasoners) {
        for(InferenceStep step: inferenceSteps) {
            step.inferenceSteps(steps,ids, reasoners);}
        ids.sort(Integer::compare);}


    /** describes the result.
     *
     * @return a description of the result.
     */
    public String toString() {return toString(null,0L);};

    /** describes the result.
     *
     * @param symboltable null or a symboltable
     * @return a description of the result.
     */
    public String toString(Symboltable symboltable, long startTime) {
        StringBuilder st = new StringBuilder();
        st.append("Result: ").append(result).append( " for problem ").append(problemId).append(" found by ").append(solverId);
        if(startTime != 0L) st.append(", elapsed time: ").append(Utilities.duration(endTime-startTime));
        st.append("\n");
        if(message != null) {st.append(message.apply(symboltable)).append("\n");}
        if(!inferenceSteps.isEmpty()) {
            ArrayList<InferenceStep> steps = new ArrayList<>();
            IntArrayList ids = new IntArrayList();
            ArrayList<String> reasoners = new ArrayList<>();
            inferenceSteps(steps,ids,reasoners);
            if(!reasoners.isEmpty()) {
                st.append("Reasoners contributing to the result:\n  ").append(reasoners).append("\n");}
            if(!ids.isEmpty()) {
                st.append("Input Clauses Ids contributing to the result:  \n").append(ids).append("\n");}
            if(!steps.isEmpty()) {
                st.append("Inference Steps contributing to the result:  \n");
                    for(InferenceStep infStep : steps) st.append(infStep.toString(symboltable)).append("\n");}}
        return st.toString();}


}
