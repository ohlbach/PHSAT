package Datastructures.Statistics;

import Coordinator.PreProcessor;
import com.sun.org.glassfish.gmbal.Description;

/**
 * Created by ohlbach on 10.10.2018.
 */
public class PreProcessorStatistics extends DataStatistics {

    static{Statistic.statisticsClasses.add(PreProcessorStatistics.class);}
    @Description("number of clauses removed during preprocessing")
    public int BCL_RedundantClauses       = 0;
    @Description("number of literals removed during preprocessing")
    public int BCL_RedundantLiterals      = 0;
    @Description("number of replacement resolutions during preprocessing")
    public int BCL_ReplacementResolutions = 0;

    public PreProcessorStatistics(PreProcessor preProcessor) {
        super(preProcessor);}



}
