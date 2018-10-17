package Datastructures.Statistics;

import Coordinator.CentralProcessor;
import Coordinator.PreProcessor;

/**
 * Created by ohlbach on 17.10.2018.
 */
public class CentralProcessorStatistic extends Statistic {
    private CentralProcessor centralProcessor = null;
    public int BCL_RedundantClauses       = 0;
    public int BCL_RedundantLiterals      = 0;
    public int BCL_ReplacementResolutions = 0;
    public int CLS_LiteralRemovals        = 0;
    public int CLS_LiteralReplacements    = 0;
    public int CLS_ClauseRemovals         = 0;
    public int CLS_Purities               = 0;
    public int IDG_TrueLiterals           = 0;
    public int IDG_Implications           = 0;
    public int IDG_Equivalences           = 0;
    public int DIS_TrueLiterals           = 0;
    public int DIS_Unsatisifiablities     = 0;
    public int DIS_Disjointnesses         = 0;
    public int EQV_TrueLiterals           = 0;
    public int EQV_Unsatisfiabilities     = 0;

    public CentralProcessorStatistic(String id) {
        super(id);
    }
}
