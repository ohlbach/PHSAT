package Datastructures.Statistics;

import Coordinator.PreProcessor;
import Coordinator.Task;
import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;

import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Created by ohlbach on 10.10.2018.
 */
public class PreProcessorStatistics extends Statistic {
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

    public PreProcessorStatistics(PreProcessor preProcessor) {
        super(preProcessor);}

    private Consumer<CLiteral> CLS_literalRemovalObserver               = (cLiteral -> ++CLS_LiteralRemovals);
    private BiConsumer<CLiteral,Boolean> CLS_literalReplacementObserver = ((cLiteral, b) -> ++CLS_LiteralReplacements);
    private Consumer<Clause>  CLS_clauseRemovalObserver                 = (cLiteral -> ++CLS_ClauseRemovals);
    private Consumer<Integer> IDG_trueLiteralObserver                   = (cLiteral -> ++IDG_TrueLiterals);
    private BiConsumer<Integer,Integer> IDG_implicationObserver         = ((from,to) -> ++IDG_Implications);
    private Consumer<int[]>   IDG_equivalenceObserver                   = (eqv -> IDG_Equivalences += eqv.length);
    private Consumer<Integer> DIS_TrueLiteralObserver                   = (literal -> ++DIS_TrueLiterals);
    private Consumer<Unsatisfiable> DIS_UnsatisfiabilityObserver        = (literal -> ++DIS_Unsatisifiablities);
    private Consumer<Clause>  DIS_DisjointnessObserver                  = (literal -> ++DIS_Disjointnesses);
    private Consumer<Unsatisfiable> EQV_UnsatisfiabilityObserver        = (literal -> ++EQV_Unsatisfiabilities);
    private Consumer<Integer> EQV_TrueLiteralObserver                   = (literal -> ++EQV_TrueLiterals);

    public void addStatisticsObservers() {
        processor.clauses.addLiteralRemovalObserver       (CLS_literalRemovalObserver);
        processor.clauses.addLiteralReplacementObserver   (CLS_literalReplacementObserver);
        processor.clauses.addClauseRemovalObserver        (CLS_clauseRemovalObserver);
        processor.implicationDAG.addTrueLiteralObserver   (IDG_trueLiteralObserver);
        processor.implicationDAG.addImplicationObserver   (IDG_implicationObserver);
        processor.implicationDAG.addEquivalenceObserver   (IDG_equivalenceObserver);
        processor.disjointnesses.addTrueLiteralObserver   (DIS_TrueLiteralObserver);
        processor.disjointnesses.addUnsatisfiabilityObserver(DIS_UnsatisfiabilityObserver);
        processor.disjointnesses.addDisjointnessObserver  (DIS_DisjointnessObserver);
        processor.equivalences.addUnsatisfiabilityObserver(EQV_UnsatisfiabilityObserver);
        processor.equivalences.addTrueLiteralObserver     (EQV_TrueLiteralObserver);

    }

    public void removeStatisticsObservers() {
        processor.clauses.removeLiteralRemovalObserver       (CLS_literalRemovalObserver);
        processor.clauses.removeLiteralReplacementObserver   (CLS_literalReplacementObserver);
        processor.clauses.removeClauseRemovalObserver        (CLS_clauseRemovalObserver);
        processor.implicationDAG.removeTrueLiteralObserver   (IDG_trueLiteralObserver);
        processor.implicationDAG.removeImplicationObserver   (IDG_implicationObserver);
        processor.implicationDAG.removeEquivalenceObserver   (IDG_equivalenceObserver);
        processor.disjointnesses.removeTrueLiteralObserver   (DIS_TrueLiteralObserver);
        processor.disjointnesses.removeUnsatisfiabilityObserver(DIS_UnsatisfiabilityObserver);
        processor.disjointnesses.removeDisjointnessObserver  (DIS_DisjointnessObserver);
        processor.equivalences.removeUnsatisfiabilityObserver(EQV_UnsatisfiabilityObserver);
        processor.equivalences.removeTrueLiteralObserver     (EQV_TrueLiteralObserver);

    }


}
