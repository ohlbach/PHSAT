package Coordinator;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.CentralProcessorStatistic;
import Management.ProblemSupervisor;

import java.util.HashMap;

/**
 * Created by ohlbach on 10.10.2018.
 */
public class CentralProcessor extends Processor {

    public CentralProcessor(PreProcessor preProcessor) {
        super(preProcessor.supervisor,preProcessor.globalParameters,preProcessor.applicationParameters,preProcessor.basicClauseList);
        clauses        = preProcessor.clauses;
        model          = preProcessor.model;
        implicationDAG = preProcessor.implicationDAG;
        equivalences   = preProcessor.equivalences;
        disjointnesses = preProcessor.disjointnesses;
        statistics     = new CentralProcessorStatistic(this);
    }

}
