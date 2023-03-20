package Datastructures.Results;

import Datastructures.Statistics.Statistic;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.sortIntArray;

/** This is the superclass of the various possible results the solvers can generate.
 */
public class Result extends Exception {
    /** the solver which found the result. */
    public String solverId = null;
    /** the problem identifier which has the result. */
    public String problemId = null;
    /** the last inference step which generated the result. */
    public ArrayList<InferenceStep> inferenceSteps = new ArrayList<>();
    /** the statistics of the solver. */
    public Statistic statistic = null;
    /** the nanotime at which the result has been generated */
    public long endTime;

    public long startTime;

    /** an optional message */
    public String message = null;


    /** the constructor. It sets the endTime */
    public Result(String problemId, String solverId) {
        super();
        this.problemId = problemId;
        this.solverId = solverId;
        endTime = System.nanoTime();}

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
    public ArrayList<InferenceStep> inferenceSteps() {
        if(inferenceSteps == null || inferenceSteps.isEmpty()) return null;
        ArrayList<InferenceStep> steps1 = new ArrayList<>();
        ArrayList<InferenceStep> steps2 = new ArrayList<>();
        ArrayList<InferenceStep> steps3 = new ArrayList<>();
            for(InferenceStep step : inferenceSteps) step.inferenceSteps(steps1);
        for(int i = 0; i < steps1.size(); ++i) {
            InferenceStep step = steps1.get(i);
            if(!steps2.contains(step)) steps2.add(step);}
        for(int i = 0; i < steps2.size(); ++i) {
            InferenceStep step2 = steps2.get(i);
            steps3.clear();
            step2.inferenceSteps(steps3);
            for(InferenceStep step3 : steps3) {if(!steps2.contains(step3)) steps2.add(step3);}}
        return steps2;}

    /** returns the list of input clause Ids which are responsible for the result.
     * Double occurrences are removed.
     *
     * @return the list of input clause Ids which are responsible for the result.
     */
    public IntArrayList inputClauseIds() {
        if(inferenceSteps.isEmpty()) return null;
        IntArrayList ids = new IntArrayList();
        for(InferenceStep step : inferenceSteps())
            for(int id : step.inputClauseIds()) {
                if(!ids.contains(id)) ids.add(id);}
        return sortIntArray(ids);}
}
