package Datastructures.Results;

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

    /** the nanotime at which the problem was ended*/
    public String duration = null;

    /** an optional message */
    public Function<Symboltable,String> message = null;

    /** the inference steps contributing to the result */
    public ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();
    /** the constructor. It sets the endTime.
     *
     * @param problemId the problem identifier which has the result.
     * @param solverId the solver which found the result
     * @param result a name of the result (eg. satisfiable, aborted etc.)
     * @param startTime the time when the reasoning started.
     */
    public Result(String problemId, String solverId, String result, long startTime) {
        super();
        this.problemId = problemId;
        this.solverId  = solverId;
        this.result    = result;
        this.duration = startTime == 0L ? null: Utilities.duration(System.nanoTime()-startTime);}

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
        if(!reasoners.contains(solverId)) {reasoners.add(solverId);}
        for(InferenceStep step: inferenceSteps) {step.inferenceSteps(steps,ids, reasoners);}
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
    public String toString(Symboltable symboltable, long stmeartTi) {
        StringBuilder st = new StringBuilder();
        st.append("Result: ").append(result).append( " for problem ").append(problemId).append(" found by ").append(solverId);
        if(duration != null) st.append(", elapsed time: ").append(duration);
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
