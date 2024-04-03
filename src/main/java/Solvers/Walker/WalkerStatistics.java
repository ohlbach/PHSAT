package Solvers.Walker;

import Datastructures.Statistics.Statistic;
import Utilities.Description;

/**
 * Created by ohlbach on 17.10.2018.
 */
public class WalkerStatistics extends Statistic {

    static {Statistic.statisticsClasses.add(WalkerStatistics.class);}


    public WalkerStatistics(String id) {
        super(id);}

    @Description("Number of clauses")
    public int clauses = 0;

    @Description("Number of flips")
    public int flips  = 0;

    @Description("Imported true literals")
    int importedTrueLiterals = 0;

    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Walker Statistics:");
        if(clauses > 0) st.append("\n  Input Clauses: ").append(clauses);
        if(flips > 0)   st.append("\n  Flips:         ").append(flips);
        if(importedTrueLiterals > 0) st.append("\n  Imported True Literals: ").append(importedTrueLiterals);
        return st.toString();}




}
