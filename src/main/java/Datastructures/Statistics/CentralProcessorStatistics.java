package Datastructures.Statistics;

import Coordinator.Processor;
import com.sun.org.glassfish.gmbal.Description;


/** This class extends the DataStatistic with information received by the central processor.<br>
 * Since the CentralProcessor takes over all clauses from the PreProcessor,
 * it also takes over the observers which fill the DataStatistic part of the statistics.
 * <br>
 * Created by ohlbach on 17.10.2018.
 */
public class CentralProcessorStatistics extends DataStatistics {
    static {Statistic.statisticsClasses.add(CentralProcessorStatistics.class);}

    @Description("number of unit clauses received by the central processor")
    public int CP_UnitClausesReceived       = 0;

    @Description("number of implications received by the central processor")
    public int CP_ImplicationsReceived      = 0;

    @Description("number of shortened input clauses received by the central processor")
    public int CP_ShortenedClausesReceived = 0;

    /** creates a new CentralProcessorStatistics object
     *
     * @param processor for which the statistics is collected.
     */
    public CentralProcessorStatistics(Processor processor) {
        super(processor);
    }

}
