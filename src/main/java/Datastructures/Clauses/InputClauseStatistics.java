package Datastructures.Clauses;

import Datastructures.Statistics.Statistic;
import org.glassfish.gmbal.Description;

/**
 * Created by ohlbach on 30.11.2019.
 */
public class InputClauseStatistics extends Statistic{
    static {
        Statistic.statisticsClasses.add(InputClauseStatistics.class);}

    @Description("number of disjunctions in the input clauses")
    public int disjunctions = 0;

    @Description("number of conjunctions in the input clauses")
    public int conjunctions = 0;

    @Description("number of equivalence classes in the input clauses")
    public int equivalences = 0;

    @Description("number of atlest clauses in the input clauses")
    public int atleasts         = 0;

    @Description("number of atmost clauses in the input clauses")
    public int atmosts    = 0;

    @Description("number of exactly clauses in the input clauses")
    public int exactlys    = 0;

    @Description("number of interval clauses in the input clauses")
    public int intervals    = 0;

    public InputClauseStatistics(String id) {
        super(id);}

}
