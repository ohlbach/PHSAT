package Solvers.ResolutionOld;

import Datastructures.Statistics.Statistic;
import Utilities.Description;

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


    @Description("number of derived unit clauses")
    public int derivedUnitClauses = 0;

    @Description("number of imported unit clauses")
    public int importedUnitClauses = 0;

    @Description("number of imported binary clauses")
    public int importedBinaryClauses = 0;

    @Description("number of exported binary clauses")
    public int exportedBinaryClauses = 0;

    @Description("number of imported other clauses")
    public int importedOtherClauses = 0;

    @Description("number of exported other clauses")
    public int exportedOtherClauses = 0;


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

    @Description("number of equivalences.")
    public int equivalences = 0;

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
