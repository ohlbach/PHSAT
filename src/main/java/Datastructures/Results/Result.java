package Datastructures.Results;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/**
 * Created by ohlbach on 14.09.2018.
 */
public class Result extends Exception {
    public InferenceStep inferenceStep = null;

    public boolean isOkay() {return true;}

    public Result() {
        super(); }
}
