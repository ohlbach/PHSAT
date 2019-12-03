package Solvers.Resolution;

import Datastructures.Statistics.Statistic;
import com.sun.org.glassfish.gmbal.Description;

/** This class extends the DataStatistics with the information about resolutions
 * <br>
 * Created by ohlbach on 24.10.2018.
 */
public class ResolutionStatistics extends Statistic {
    static {Statistic.statisticsClasses.add(ResolutionStatistics.class);}


    /** construction a statics for the Resolution processor
     *
     * @param id an identifier.
     */
    public ResolutionStatistics(String id) {
        super(id);}


    @Description("number of unit clauses generated")
    public int unitClauses = 0;

    @Description("number of backward subsumptions")
    public int backwardSubsumptions = 0;

    @Description("number of forward subsumptions")
    public int forwardSubsumptions = 0;

    @Description("number of replacement resolutions into the new clause")
    public int backwardReplacementResolutions = 0;

    @Description("number of replacement resolutions from the new clause")
    public int forwardReplacementResolutions = 0;

    @Description("number of generated resolvents.")
    public int resolvents = 0;



}
