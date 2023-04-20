package Datastructures.TwoLiteral;

import Datastructures.Statistics.Statistic;
import Utilities.Description;

import java.util.ArrayList;

public class TwoLitStatistics extends Statistic {


    public static ArrayList<TwoLitStatistics> statistics = new ArrayList<>();

    static {Statistic.statisticsClasses.add(TwoLitStatistics.class);}

    public TwoLitStatistics(String id) {
        super(id);
        statistics.add(this);
    }

    @Description("number of two-literal clauses")
    public int twoLitlauses = 0;

    @Description("number of resolvents generated")
    public int resolvents = 0;

    @Description("number of unit clauses generated")
    public int unitClauses = 0;

    @Description("number of equivalences found.")
    public int equivalences = 0;

    @Description("number of disjointnesses found.")
    public int disjointnesses = 0;

}

