package Solvers.Resolution;

import Coordinator.Processor;
import Datastructures.Statistics.DataStatistics;
import Datastructures.Statistics.Statistic;
import com.sun.org.glassfish.gmbal.Description;

/** This class extends the DataStatistics with the information about resolutions
 * <br>
 * Created by ohlbach on 24.10.2018.
 */
public class ResolutionStatistics extends DataStatistics {
    static {Statistic.statisticsClasses.add(ResolutionStatistics.class);}

    @Description("number of resolvents in the Resolution processor.")
    public int resolvents = 0;

    /** construction a statics for the Resolution processor
     *
     * @param processor the Resolution processor.
     */
    public ResolutionStatistics(Processor processor) {
        super(processor);}


}
