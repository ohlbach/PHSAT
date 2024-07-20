package Solvers.Backtracker;

import Datastructures.Statistics.Statistic;
import Solvers.Solver;
import Utilities.Description;

import java.util.ArrayList;

import static Utilities.Utilities.duration;

/**
 * StatisticsBacktracker is a subclass of Statistic that keeps track of various statistics related to backtracking operations.
 */
public class StatisticsBacktracker extends Statistic {


    public StatisticsBacktracker(String id) {
        super(id);
    }

    @Description("number of backtrackings")
    public int backtrackings = 0;

    @Description("Backjumps")
    public int backjumps = 0;

    @Description("Selected Literals")
    public int selectedLiterals = 0;

    @Description("Maximal Recursion Depth")
    public int recursionDepth = 0;

    @Description("Incopration of changes in the ClauseList")
    public int incoporations = 0;

    @Description("Propagator Jobs")
    public int propagatorJobs = 0;

    @Description("Elapsed Time")
    public long elapsedTime = 0;


    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Backtracker Statistics:");
        st.append("\n  selected literals: ").append(selectedLiterals);
        st.append("\n  recusion depth:    ").append(recursionDepth);
        st.append("\n  backtrackings:     ").append(backtrackings);
        st.append("\n  backjumps:         ").append(backjumps);
        st.append("\n  incorporations:    ").append(incoporations);
        st.append("\n  propagator jobs:   ").append(propagatorJobs);
        st.append("\n  elapsed time:      ").append(duration(elapsedTime));
        return st.toString();}

}
