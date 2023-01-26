package Management;

import Datastructures.Statistics.Statistic;
import org.glassfish.gmbal.Description;

/**
 * Created by ohlbach on 30.11.2019.
 */
public class SupervisorStatistics extends Statistic{

    static {Statistic.statisticsClasses.add(SupervisorStatistics.class);}

    public SupervisorStatistics(String id) {
        super(id);}

    @Description("number of solvers applied to the problem")
    public int solvers = 0;

    @Description("number of aborted solvers")
    public int aborted = 0;

    @Description("number of erraneous solvers")
    public int erraneous = 0;




}
