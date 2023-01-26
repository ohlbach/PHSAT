package Solvers.Resolution;

import Datastructures.Statistics.Statistic;
import org.glassfish.gmbal.Description;

/**
 * Created by ohlbach on 12.02.2020.
 */
public class PreparerStatistics extends Statistic {
    static {
        Statistic.statisticsClasses.add(ResolutionStatistics.class);}


    /** construction a statics for the Resolution processor
     *
     * @param id an identifier.
     */
    public PreparerStatistics(String id) {
        super(id);}

    @Description("number of derived unit clauses")
    public int derivedUnitClauses = 0;

    @Description("number of backward subsumptions")
    public int backwardSubsumptions = 0;

    @Description("number of forward subsumptions")
    public int forwardSubsumptions = 0;

    @Description("number of replacement resolutions into the new clause")
    public int backwardReplacementResolutions = 0;

    @Description("number of replacement resolutions from the new clause")
    public int forwardReplacementResolutions = 0;

    @Description("number of derived equivalences.")
    public int equivalences = 0;

    @Description("number of derived disjunctions.")
    public int disjunctions = 0;


    @Description("reductions.")
    public int reductions = 0;

    @Description("removePureLiterals")
    public int purities = 0;

    @Description("eliminations")
    public int eliminations = 0;

    @Description("positive clauses")
    public int positiveClauses = 0;

    @Description("negative clauses")
    public int negativeClauses = 0;

    @Description("clauses")
    public int clauses = 0;

}
