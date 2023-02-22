package Datastructures.Results;

import Datastructures.Statistics.Statistic;
import InferenceSteps.InferenceStep;

/**
 * Created by ohlbach on 14.09.2018.
 */
public class Result extends Exception {
    public InferenceStep inferenceStep = null;
    public Class solverClass = null;
    public String solverId   = null;
    public String problemId  = null;
    public Statistic statistic;


    public boolean isOkay() {return true;}

    public Result() {
        super(); }
}
