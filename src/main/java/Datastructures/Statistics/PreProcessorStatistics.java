package Datastructures.Statistics;

import Coordinator.PreProcessor;
import Coordinator.Task;
import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;

import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Created by ohlbach on 10.10.2018.
 */
public class PreProcessorStatistics extends Statistic {
    private PreProcessor preProcessor = null;
    public int tautologies  = 0;
    public int literalDeletions = 0;
    public int clauseDeletions = 0;
    public int forwardSubsumptions  = 0;
    public int backwardSubsumptions  = 0;
    public int purities         = 0;
    public int implicationResolutions = 0;
    public int doubleLiterals = 0;
    public int equivalenceReplacements = 0;
    public int falseLiterals = 0;
    public int trueClauses = 0;
    public int replacementResolutions = 0;
    public int ID_TrueLiterals = 0;
    public int ID_Implications = 0;
    public int ID_DerivedEquivalences = 0;

    public PreProcessorStatistics(PreProcessor preProcessor) {
        this.preProcessor = preProcessor;}

    public void addtautologies(int n) {tautologies += n;}
    public void addLiteralDeletions(int n) {literalDeletions += n;}
    public void addClauseDeletions(int n) {literalDeletions += n;}
    public void addPurities(int n) {purities += n;}
    public void addImplicationResolutions(int n) {implicationResolutions += n;}
    public void addDoubleLiterals(int n) {doubleLiterals += n;}
    public void addEquivalenceReplacements(int n) {equivalenceReplacements += n;}
    public void addFalseLiterals(int n) {falseLiterals += n;}
    public void addTrueClauses(int n) {trueClauses += n;}
    public void addBackwardSubsumptions(int n) {backwardSubsumptions += n;}
    public void addForwardSubsumptions(int n) {forwardSubsumptions += n;}
    public void addReplacementResolutions(int n) {replacementResolutions += n;}


    private Consumer<CLiteral> literalRemovalObserver = (cLiteral -> ++literalDeletions);
    private BiConsumer<CLiteral,Boolean> literalReplacementObserver = ((cLiteral, b) -> ++equivalenceReplacements);
    private Consumer<Clause> clauseRemovalObserver = (cLiteral -> ++clauseDeletions);
    private Consumer<Integer> trueLiteralObserver = (cLiteral -> ++ID_TrueLiterals);
    private BiConsumer<Integer,Integer> implicationObserver = ((from,to) -> ++ID_Implications);
    private Consumer<int[]> derivedEquivalenceObserver = (eqv -> ID_DerivedEquivalences += eqv.length);

    public void addStatisticsObservers() {
        preProcessor.clauses.addLiteralRemovalObserver(literalRemovalObserver);
        preProcessor.clauses.addLiteralReplacementObserver(literalReplacementObserver);
        preProcessor.clauses.addClauseRemovalObserver(clauseRemovalObserver);
        preProcessor.implicationDAG.addTrueLiteralObserver(trueLiteralObserver);
        preProcessor.implicationDAG.addImplicationObserver(implicationObserver);
        preProcessor.implicationDAG.addEquivalenceObserver(derivedEquivalenceObserver);
        /*
        preProcessor.equivalences.addTrueLiteralObserver(literal -> addTask(new Task.OneLiteral(literal,this)));
        preProcessor.equivalences.addUnsatisfiabilityObserver(unsat -> addTask(new Task.Unsatisfiability(unsat,this)));
        preProcessor.disjointnesses.addUnsatisfiabilityObserver(unsat -> addTask(new Task.Unsatisfiability(unsat,this)));
        preProcessor.disjointnesses.addTrueLiteralObserver(literal -> addTask(new Task.OneLiteral(literal,this)));
*/
    }

    public void removeStatisticsObservers() {
        preProcessor.clauses.removeLiteralRemovalObserver(literalRemovalObserver);
        preProcessor.clauses.removeLiteralReplacementObserver(literalReplacementObserver);
        preProcessor.clauses.removeClauseRemovalObserver(clauseRemovalObserver);
        preProcessor.implicationDAG.removeTrueLiteralObserver(trueLiteralObserver);
        preProcessor.implicationDAG.removeImplicationObserver(implicationObserver);
        preProcessor.implicationDAG.removeEquivalenceObserver(derivedEquivalenceObserver);
    }

    public String toString() {
        return Statistic.toString(0,this);}

    public String toString(int size) {
        return Statistic.toString(size,this);}

    public static void main (String[] args) throws Exception {
        PreProcessorStatistics s = new PreProcessorStatistics(null);
        s.replacementResolutions = 50;
        System.out.println(s.toString());
    }

}
