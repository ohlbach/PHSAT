package Datastructures;

import Datastructures.Statistics.Statistic;
import Utilities.Description;

public class ClauseListStatistics extends Statistic {

    public ClauseListStatistics() {
        super("ClauseList");
    }

    @Description("Subsumed Clauses")
    public int subsumedClauses = 0;


    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Clause List Statistics:");
        if(subsumedClauses > 0)       st.append("\n  Subsumed Clauses:      ").append(subsumedClauses);

        return st.toString();}

    }
