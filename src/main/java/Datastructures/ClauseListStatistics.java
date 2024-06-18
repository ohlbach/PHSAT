package Datastructures;

import Datastructures.Statistics.Statistic;
import Utilities.Description;

public class ClauseListStatistics extends Statistic {

    public ClauseListStatistics() {
        super("ClauseList");
    }

    @Description("Subsumed Clauses")
    public int subsumedClauses = 0;


    @Description("Pure Literals")
    public int pureLiterals = 0;

    @Description("Singleton Literals")
    public int singletonLiterals = 0;


    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Clause List Statistics:");
        if(subsumedClauses > 0)    st.append("\n  Subsumed Clauses:   ").append(subsumedClauses);
        if(pureLiterals > 0)       st.append("\n  Pure Literals:      ").append(pureLiterals);
        if(singletonLiterals > 0)  st.append("\n  Singleton Literals: ").append(singletonLiterals);
        return st.toString();}

    }
