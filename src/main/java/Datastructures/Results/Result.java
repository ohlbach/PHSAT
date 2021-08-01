package Datastructures.Results;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Theory.Model;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/**
 * Created by ohlbach on 14.09.2018.
 */
public class Result extends Exception {
    public IntArrayList origins = null;

    public boolean isOkay() {return true;}

    public Result() {
        super(); }
}
