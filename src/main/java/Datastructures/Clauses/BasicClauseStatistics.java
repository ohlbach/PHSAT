package Datastructures.Clauses;

import Datastructures.Statistics.Statistic;
import com.sun.org.glassfish.gmbal.Description;

/**
 * Created by ohlbach on 30.11.2019.
 */
public class BasicClauseStatistics  extends Statistic{
    static {
        Statistic.statisticsClasses.add(BasicClauseStatistics.class);}

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

    public BasicClauseStatistics(String id) {
        super(id);}

}
