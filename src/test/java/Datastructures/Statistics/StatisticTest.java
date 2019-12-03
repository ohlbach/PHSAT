package Datastructures.Statistics;

import org.junit.Test;

import java.util.ArrayList;

/**
 * Created by ohlbach on 16.10.2018.
 */
public class StatisticTest {


    @Test
    public void combineSameStatistics() throws Exception {
        PreProcessorStatistics p1 = new PreProcessorStatistics(null);
        p1.DIS_TrueLiterals = 10;
        p1.CLS_Purities = 100;

        PreProcessorStatistics p2 = new PreProcessorStatistics(null);
        p2.DIS_TrueLiterals = 200;
        p2.CLS_Purities = 10;
        p2.BCL_RedundantClauses = 100;

        Statistic[] stt = new Statistic[]{p1,p2};
        ArrayList<Object[]> ar = Statistic.combineSameStatistics(stt,false);

        System.out.println(Statistic.statisticToString(ar));
    }

    @Test
    public void combineDifferentStatistics() throws Exception {
        System.out.println("Combine different strategies");
        PreProcessorStatistics p1 = new PreProcessorStatistics(null);
        p1.DIS_TrueLiterals = 10;
        p1.CLS_Purities = 100;

        PreProcessorStatistics p2 = new PreProcessorStatistics(null);
        p2.DIS_TrueLiterals = 200;
        p2.CLS_Purities = 10;
        p2.BCL_RedundantClauses = 100;

        ProblemStatistics p3 = new ProblemStatistics("pro");
        p3.solvers = 5;
        p3.disjunctions = 500;

        Statistic[] stt = new Statistic[]{p1,p2,p3};
        ArrayList<Object[]> ar = Statistic.combineDifferentStatistics(stt,false);
        String[] ids = new String[]{"Prob 1", "Prob 2", "Prob 3"};
        System.out.println(Statistic.statisticToString(ar));

    }

    @Test
    public void statisticToString() throws Exception {
        System.out.println("toString");
        String[] ids = new String[]{"Prol.","Problem2"};
        ArrayList<Object[]> stds = new ArrayList<>();
        stds.add(ids);
        stds.add(new Object[]{"subsumptions",3000, null, 13,6500.3f});
        stds.add(new Object[]{"taut",2,4.5,6,3});
        System.out.println(Statistic.statisticToString(stds));

    }

    @Test
    public void statisticToCSV() throws Exception {
        System.out.println("toCSV");
        String[] ids = new String[]{"Prol.","Problem2"};
        ArrayList<Object[]> stds = new ArrayList<>();
        stds.add(ids);
        stds.add(new Object[]{"subsumptions",3000, null, 13,6500.3f});
        stds.add(new Object[]{"taut",2,4.5,6,3});
        System.out.println(Statistic.statisticToCSV(" ; ",stds));
    }

    @Test
    public void singleToString() throws Exception {
        System.out.println("toString single");
        PreProcessorStatistics p1 = new PreProcessorStatistics(null);
        p1.DIS_TrueLiterals = 10;
        p1.CLS_Purities = 100;
        System.out.println(Statistic.statisticToString("Prob1", p1.extractStatistic(false)));

        System.out.println(Statistic.statisticToCSV("Prob1"," ; ", p1.extractStatistic(true)));

        System.out.println(p1.toString());


    }

}