package Solvers.Simplifier;

import Datastructures.Statistics.Statistic;
import org.glassfish.gmbal.Description;

public class SimplifierStatistics extends Statistic {
    public SimplifierStatistics(String id) {
        super(id);
    }

    @Description("number of not internalized input clauses ")
    public int notInternalizedInputClauses = 0;

    @Description("number of derived unit clauses")
    public int derivedUnitClauses = 0;




}
