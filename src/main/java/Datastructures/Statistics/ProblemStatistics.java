package Datastructures.Statistics;

import Coordinator.Processor;
import com.sun.org.glassfish.gmbal.Description;

import java.lang.reflect.Field;

/**
 * Created by ohlbach on 11.10.2018.
 */
public class ProblemStatistics extends Statistic {
    static {Statistic.statisticsClasses.add(ProblemStatistics.class);}

    @Description("number of disjunctions in the input clauses")
    public int disjunctions = 0;
    @Description("number of conjunctions in the input clauses")
    public int conjunctions = 0;
    @Description("number of xors in the input clauses")
    public int xors         = 0;
    @Description("number of disjointness classes in the input clauses")
    public int disjoints    = 0;
    @Description("number of equivalence classes in the input clauses")
    public int equivalences = 0;
    @Description("number of solvers applied to the problem")
    public int solvers      = 0;
    @Description("number of solvers which got aborted")
    public int aborted      = 0;
    @Description("number of solvers which produced wrong models")
    public int erraneous    = 0;

    public void incAborted() {++aborted;}
    public void incErraneous() {++erraneous;}

    public ProblemStatistics(String id) {
        super(id);}



    public static void main (String[] args) throws Exception {
        ProblemStatistics s = new ProblemStatistics("AA");
        s.disjunctions = 10;
        s.aborted = 2;
        System.out.println(s.toString());
        System.out.println(Statistic.descriptions());
    }

}
