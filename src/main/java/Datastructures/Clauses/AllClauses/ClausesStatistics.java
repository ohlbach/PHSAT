package Datastructures.Clauses.AllClauses;

import Datastructures.Clauses.Connective;
import Datastructures.Statistics.Statistic;
import Solvers.Resolution.ResolutionStatistics;
import org.glassfish.gmbal.Description;

public class ClausesStatistics extends Statistic {
    static {
        Statistic.statisticsClasses.add(ResolutionStatistics.class);}


    /** construction a statics for the Resolution processor
     *
     * @param id an identifier.
     */
    public ClausesStatistics(String id) {
        super(id);}

    @Description("number of derived unit clauses")
    public int derivedUnitClauses = 0;

    @Description("number of backward subsumptions")
    public int backwardSubsumptions = 0;

    @Description("number of forward subsumptions")
    public int forwardSubsumptions = 0;

    @Description("number of replacement resolutions into the new clause")
    public int replacementResolutionBackwards = 0;

    @Description("number of replacement resolutions from the new clause")
    public int replacementResolutionForward = 0;

    @Description("number of derived equivalences.")
    public int equivalences = 0;

    @Description("number of equivalence replacements.")
    public int equivalenceReplacements = 0;

    @Description("number of derived disjunctions.")
    public int disjunctions = 0;

    @Description("number of unit resolutions.")
    public int unitResolutions = 0;

    @Description("removePureLiterals")
    public int purities = 0;

    @Description("eliminations")
    public int eliminations = 0;

    @Description("positive clauses")
    public int positiveClauses = 0;

    @Description("negative clauses")
    public int negativeClauses = 0;

    @Description("clauses")
    public int[] clauses = new int[Connective.size()];

    @Description("tautologies")
    public int tautologies = 0;


}
