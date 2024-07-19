package Solvers.Backtracker;

import Datastructures.Statistics.Statistic;
import Utilities.Description;

public class StatisticsBacktracker extends Statistic {
    public StatisticsBacktracker(String id) {
        super(id);
    }

    @Description("number of backtrackings")
    public int backtrackings = 0;

    @Description("Backjumps")
    public int backjumps = 0;

    @Description("Incopration of changes in the ClauseList")
    public int incoporations = 0;

    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Backtracker Statistics:");
        st.append("\n  backtrackings:     ").append(backtrackings);
        st.append("\n  backjumps:         ").append(backjumps);
        st.append("\n  incorporations:    ").append(incoporations);
        return st.toString();}

}
