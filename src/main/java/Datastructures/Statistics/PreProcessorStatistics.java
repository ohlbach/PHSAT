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
    private PreProcessor preProcessor = null;
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
        this.preProcessor = preProcessor;}

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
        preProcessor.clauses.addLiteralRemovalObserver       (CLS_literalRemovalObserver);
        preProcessor.clauses.addLiteralReplacementObserver   (CLS_literalReplacementObserver);
        preProcessor.clauses.addClauseRemovalObserver        (CLS_clauseRemovalObserver);
        preProcessor.implicationDAG.addTrueLiteralObserver   (IDG_trueLiteralObserver);
        preProcessor.implicationDAG.addImplicationObserver   (IDG_implicationObserver);
        preProcessor.implicationDAG.addEquivalenceObserver   (IDG_equivalenceObserver);
        preProcessor.disjointnesses.addTrueLiteralObserver   (DIS_TrueLiteralObserver);
        preProcessor.disjointnesses.addUnsatisfiabilityObserver(DIS_UnsatisfiabilityObserver);
        preProcessor.disjointnesses.addDisjointnessObserver  (DIS_DisjointnessObserver);
        preProcessor.equivalences.addUnsatisfiabilityObserver(EQV_UnsatisfiabilityObserver);
        preProcessor.equivalences.addTrueLiteralObserver     (EQV_TrueLiteralObserver);

    }

    public void removeStatisticsObservers() {
        preProcessor.clauses.removeLiteralRemovalObserver       (CLS_literalRemovalObserver);
        preProcessor.clauses.removeLiteralReplacementObserver   (CLS_literalReplacementObserver);
        preProcessor.clauses.removeClauseRemovalObserver        (CLS_clauseRemovalObserver);
        preProcessor.implicationDAG.removeTrueLiteralObserver   (IDG_trueLiteralObserver);
        preProcessor.implicationDAG.removeImplicationObserver   (IDG_implicationObserver);
        preProcessor.implicationDAG.removeEquivalenceObserver   (IDG_equivalenceObserver);
        preProcessor.disjointnesses.removeTrueLiteralObserver   (DIS_TrueLiteralObserver);
        preProcessor.disjointnesses.removeUnsatisfiabilityObserver(DIS_UnsatisfiabilityObserver);
        preProcessor.disjointnesses.removeDisjointnessObserver  (DIS_DisjointnessObserver);
        preProcessor.equivalences.removeUnsatisfiabilityObserver(EQV_UnsatisfiabilityObserver);
        preProcessor.equivalences.removeTrueLiteralObserver     (EQV_TrueLiteralObserver);

    }

    public String toString() {
        return Statistic.toString(0,this);}

    public String toString(int size) {
        return Statistic.toString(size,this);}


}
