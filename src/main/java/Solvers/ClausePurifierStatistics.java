package Solvers;

import Datastructures.Statistics.Statistic;
import org.glassfish.gmbal.Description;

public class ClausePurifierStatistics extends Statistic {
    public ClausePurifierStatistics(Solver solver) {
        super(solver);
    }

    public ClausePurifierStatistics(String id) {
        super(id);
    }
    @Description("number changed clauses")
    public int changedClauses = 0;
    @Description("number derived true literals")
    public int derivedTrueLiterals = 0;

    @Description("number of deleted complementary literals")
    public int complentaryLiterals = 0;

    @Description("number of deleted multiplicities")
    public int multiplicities = 0;

}
