package Solvers.Backtracker;

import Datastructures.Statistics.Statistic;
import Utilities.Description;

public class BacktrackerStatistics extends Statistic {
    public BacktrackerStatistics(String id) {
        super(id);
    }

    @Description("number of backtrackings")
    public int backtrackings = 0;

    @Description("Merge Resolutions")
    public int mergeResolutions = 0;

    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Backtracker Statistics:");
        if(backtrackings    != 0) st.append("\n  backtrackings:     ").append(backtrackings);
        if(mergeResolutions != 0) st.append("\n  merge resolutions: ").append(mergeResolutions);
        return st.toString();}

}
