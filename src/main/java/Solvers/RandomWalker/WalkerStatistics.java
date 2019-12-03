package Solvers.RandomWalker;

import Datastructures.Statistics.Statistic;
import com.sun.org.glassfish.gmbal.Description;

/**
 * Created by ohlbach on 17.10.2018.
 */
public class WalkerStatistics extends DataStatistics {

    static {Statistic.statisticsClasses.add(WalkerStatistics.class);}

    @Description("Number of flips")
    public int RW_flips  = 0;

    WalkerStatistics(Processor processor) {
        super(processor);
    }

}
