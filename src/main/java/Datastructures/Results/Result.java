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
    public Class  solver       = null;
    /** the problem identifier which has the result. */
    public String problemId    = null;
    /** the last inference step which generated the result. */
    public InferenceStep inferenceStep = null;
    /** the statistics of the solver. */
    public Statistic statistic = null;
    /** the nanotime at which the result has been generated */
    public long endTime;


    /** the constructor. It sets the endTime */
    public Result() {
        super();
        endTime = System.nanoTime();}

    /** gets the solverId.
     *
     * @return the solverId.
     */
    public String getSolverId() {
        try{
            return (String)solver.getMethod("getSolverId").invoke(solver);}
        catch(Exception ex) {ex.printStackTrace(); System.exit(1); }
        return "";}

    /** returns the inference steps which produced the result.
     * Double occurrences are removed.
     *
     * @return null or the list of inference steps which produced the result.
     */
    public ArrayList<InferenceStep> inferenceSteps() {
        if(inferenceStep == null) return null;
        ArrayList<InferenceStep> steps = new ArrayList<>();
        inferenceStep.inferenceSteps(steps);
        for(int i = 0; i < steps.size(); ++i) {
            InferenceStep step = steps.get(i);
            for(int j = i+1; j < steps.size(); ++j) {
                if(steps.get(j) == step) steps.remove(j);}}
        return steps;}

    /** returns the list of input clause Ids which are responsible for the result.
     * Double occurrences are removed.
     *
     * @return the list of input clause Ids which are responsible for the result.
     */
    public IntArrayList inputClauseIds() {
        if(inferenceStep == null) return null;
        IntArrayList ids = inferenceStep.inputClauseIds();
        for(int i = 0; i < ids.size(); ++i) {
            int id = ids.getInt(i);
            for(int j = i+1; j < ids.size(); ++j) {
                if(ids.getInt(j) == id) ids.removeInt(j);}}
        return sortIntArray(ids);}
}
