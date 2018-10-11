package Datastructures.Statistics;

import java.lang.reflect.Field;

/**
 * Created by ohlbach on 11.10.2018.
 */
public class ProblemStatistics extends Statistic {
    public int disjunctions = 0;
    public int conjunctions = 0;
    public int xors         = 0;
    public int disjoints    = 0;
    public int equivalences = 0;
    public int solvers      = 0;
    public int aborted      = 0;
    public int erraneous    = 0;

    public void incAborted() {++aborted;}
    public void incErraneous() {++erraneous;}

    public String toString() {
        return Statistic.toString(0,this);}

    public String toString(int size) {
        return Statistic.toString(size,this);}


    public static void main (String[] args) throws Exception {
        ProblemStatistics s = new ProblemStatistics();
        s.disjunctions = 10;
        s.aborted = 2;
        System.out.println(s.toString());
    }

}
