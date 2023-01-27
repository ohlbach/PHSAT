package Datastructures.Results;

import InferenceSteps.InferenceStep;

/**
 * Created by ohlbach on 14.09.2018.
 */
public class Result extends Exception {
    public InferenceStep inferenceStep = null;

    public boolean isOkay() {return true;}

    public Result() {
        super(); }
}
