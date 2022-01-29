package Solvers.RecursiveSearch;

import Datastructures.Statistics.Statistic;
import com.sun.org.glassfish.gmbal.Description;

public class RSStatistics  extends Statistic {

    static {Statistic.statisticsClasses.add(RSStatistics.class);}

    public RSStatistics(String id) {
        super(id);}

    @Description("Number of backtrackings")
    public int backtrackings = 0;

    @Description("Number of subsumptions")
    public int subsumptions = 0;

}
