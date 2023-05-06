package Solvers.Backtracker;

import Datastructures.Statistics.Statistic;
import Utilities.Description;
import it.unimi.dsi.fastutil.ints.IntArrayList;

public class BacktrackerStatistics extends Statistic {
    public BacktrackerStatistics(String id) {
        super(id);
    }

    @Description("number of backtrackings")
    public int backtrackings = 0;

    @Description("Merge Resolutions")
    public int mergeResolutions = 0;

    @Description("Backjumps")
    public int backjumps = 0;

    @Description("Derived Literal Lengths")
    public IntArrayList derivedLiteralLength = new IntArrayList(10);
    public void addDerivedLiteralLength(int length) {
        if(derivedLiteralLength.size() < length+1)
            for(int i = derivedLiteralLength.size(); i <= length+1; ++i) derivedLiteralLength.add(0);
        derivedLiteralLength.set(length,derivedLiteralLength.getInt(length)+1);
    }


    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Backtracker Statistics:");
        st.append("\n  backtrackings:     ").append(backtrackings);
        st.append("\n  backjumps:         ").append(backjumps);
        st.append("\n  merge resolutions: ").append(mergeResolutions);
        st.append("\n  derived literal lengths: \n   ");
        for(int i = 0; i < derivedLiteralLength.size(); ++i) {
            if(derivedLiteralLength.getInt(i) > 0)
                st.append(i + ": " + derivedLiteralLength.getInt(i) + ", ");}
        return st.toString();}

}
