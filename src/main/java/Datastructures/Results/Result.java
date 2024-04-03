package Datastructures.Results;

import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This is the superclass of the various possible results the solvers can generate.
 * The most important subclasses represent the satisfiability and unsatisfiability of a clause set.
 */
public class Result extends Exception {
    /** the solver which found the result. */
    public String solverId = null;
    /** the problem identifier which has the result. */
    public String problemId = null;
    /** the last inference steps which generated the result. */
    public ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();
    /** the statistics of the solver. */
    public Statistic statistic = null;
    /** the nanotime at which the result has been generated */
    public long endTime;

    /** the nanotime at which the search was started. */
    public long startTime;

    /** an optional message */
    public String message = null;


    /** the constructor. It sets the endTime.
     *
     * @param problemId the problem identifier which has the result.
     * @param solverId the solver which found the result.
     */
    public Result(String problemId, String solverId) {
        super();
        this.problemId = problemId;
        this.solverId = solverId;
        endTime = System.nanoTime();
    }

    /** returns the duration (endTime - startTime) in nanoseconds.
     *
     * @return 0 or the duration in nanoseconds.
     */
    public long getDuration() {
        return (startTime != 0 && endTime != 0) ? endTime - startTime : 0;}


    /** returns the inference steps which produced the result.
     * Double occurrences are removed.
     *
     * @return null or the list of inference steps which produced the result.
     */
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(inferenceSteps == null || inferenceSteps.isEmpty()) return;
        for(InferenceStep step : inferenceSteps) step.inferenceSteps(steps,ids);
        ids.sort((x,y) -> Integer.compare(x,y));
    }

    public static void printInferenceSteps(InferenceStep step) {
        ArrayList<InferenceStep> steps = new ArrayList<>();
        IntArrayList ids = new IntArrayList();
        step.inferenceSteps(steps,ids);
        System.out.println("Input Ids: " + ids.toString());
        for(InferenceStep infStep : steps) System.out.println(infStep.toString(null));}



    public String toString(Symboltable symboltable, boolean trackReasoning) {return "";};

    public String toString(Symboltable symboltable) {return "";};
}
