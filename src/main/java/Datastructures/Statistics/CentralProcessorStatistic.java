package Datastructures.Statistics;

import Coordinator.CentralProcessor;
import Coordinator.PreProcessor;
import Coordinator.Processor;

import java.util.function.Consumer;

/**
 * Created by ohlbach on 17.10.2018.
 */
public class CentralProcessorStatistic extends Statistic {
    public int CP_UnitClausesReceived       = 0;
    public int CP_UnitClausesSent           = 0;
    public int CP_ImplicationsReceived      = 0;
    public int CP_ImplicationsSent          = 0;
    public int CP_LongClausesReceived       = 0;

    public CentralProcessorStatistic(Processor processor) {
        super(processor);
    }



}
