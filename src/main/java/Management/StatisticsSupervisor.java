package Management;

import Datastructures.Statistics.Statistic;
import Utilities.Description;

/**
 * Created by ohlbach on 30.11.2019.
 */
public class StatisticsSupervisor extends Statistic{

    static {Statistic.statisticsClasses.add(StatisticsSupervisor.class);}

    public StatisticsSupervisor(String id) {
        super(id);}

    @Description("number of solvers applied to the problem")
    public int solvers = 0;

    @Description("number of aborted solvers")
    public int aborted = 0;

    @Description("number of erraneous solvers")
    public int erraneous = 0;

    @Description("elapsed time for solving the problem")
    public long elapsedTime = 0;

    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Statistics ").append(id).append("\n");
                           st.append("Solvers:      ").append(solvers).append("\n");
        if(aborted != 0)   st.append("Aborted:      ").append(aborted).append("\n");
        if(erraneous != 0) st.append("Erraneous:    ").append(erraneous).append("\n");
                           st.append("Elapsed Time: ").append(elapsedTime).append("\n");
        return st.toString();
    }




}
