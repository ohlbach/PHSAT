package Solvers.Walker;

import Datastructures.Statistics.Statistic;
import org.glassfish.gmbal.Description;

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

}
