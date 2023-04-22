package Solvers.Backtracker;

import Datastructures.Statistics.Statistic;
import Utilities.Description;

public class BacktrackerStatistics extends Statistic {
    public BacktrackerStatistics(String id) {
        super(id);
    }

    @Description("number of contradictions")
    public int contradictions = 0;

    @Description("number of complementary literal pairs.")
    public int complementaryLiterals = 0;

}
