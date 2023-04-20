package Solvers.Walker;

import Datastructures.Statistics.Statistic;
import Utilities.Description;

/**
 * Created by ohlbach on 17.10.2018.
 */
public class Statistics extends Statistic {

    static {Statistic.statisticsClasses.add(Statistics.class);}


    public Statistics(String id) {
        super(id);}

    @Description("Number of clauses")
    public int clauses = 0;

    @Description("Number of flips")
    public int flips  = 0;

    @Description("Imported true literals")
    int importedTrueLiterals = 0;

    @Description("Imported equivalences")
    int importedEquivalentLiterals = 0;


}
