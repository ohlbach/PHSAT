package Solvers.Backtracker;

import Datastructures.Statistics.Statistic;
import Utilities.Description;

public class BacktrackerStatistics extends Statistic {
    public BacktrackerStatistics(String id) {
        super(id);
    }

    @Description("number of backtrackings")
    public int backtrackings = 0;

    public String toString() {
        return "Backtracker Statistics:\n  backtrackings:  " + backtrackings;}

}
