package Datastructures.Theory;

import Datastructures.Statistics.Statistic;
import com.sun.org.glassfish.gmbal.Description;

public class EquivalenceStatistics extends Statistic {

    static {Statistic.statisticsClasses.add(DisjointnessStatistics.class);}

    public EquivalenceStatistics(String id) {
        super(id);}

    @Description("number of basic clauses")
    public int basicClauses = 0;

    @Description("number of derived equivalence classes")
    public int derivedClasses = 0;

}
