package Coordinator;

import Algorithms.Algorithms;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Statistics.PreProcessorStatistics;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.ImplicationDAG;
import Datastructures.Theory.Model;
import Management.GlobalParameters;
import Management.ProblemSupervisor;

import java.util.*;

/** The preprocessor reads analyses the basic clauses and transfers them to the Clause-datastructure.
 * Created by ohlbach on 14.09.2018.
 */
public class PreProcessor extends Processor {

    /** Constructs the preprocessor
     *
     * @param supervisor        which manages the problem processing
     * @param globalParameters  some global parameters
     * @param problemParameters for specifying the problem
     * @param basicClauseList   the clauses [number,type,literal1,...]
     */
    public PreProcessor(ProblemSupervisor supervisor, GlobalParameters globalParameters, HashMap<String,Object> problemParameters, BasicClauseList basicClauseList) {
        super("PP", supervisor,globalParameters,problemParameters,basicClauseList);
        initializeData();
        addObservers();
        addMonitors("Preprocessor");}

    private void initializeData() {
        model          = new Model(predicates);
        clauses        = new ClauseList(basicClauseList.disjunctions.size(),predicates);
        implicationDAG = new ImplicationDAG();
        equivalences   = new EquivalenceClasses(model, implicationDAG);
        disjointnesses = new DisjointnessClasses(model, implicationDAG,equivalences);
        statistics     = new PreProcessorStatistics(this);
    }

    protected void addObservers() {
        clauses.addLiteralRemovalObserver(longClauseObserver);
        implicationDAG.addTrueLiteralObserver(trueLiteralObserver);
        implicationDAG.addImplicationObserver(implicationObserver);
        implicationDAG.addEquivalenceObserver(equivalenceObserver);
        equivalences.addTrueLiteralObserver(trueLiteralObserver);
        equivalences.addUnsatisfiabilityObserver(unsatisfiabilityObserver);
        disjointnesses.addTrueLiteralObserver(trueLiteralObserver);
        disjointnesses.addUnsatisfiabilityObserver(unsatisfiabilityObserver);
    }


    /** turns the basic clauses into Clause datastructure. Initial simplifications on the clause itself are performed
     *
     * @return Unsatisfiable if a contradiction has detected, otherwise null.
     */
    public Result prepareClauses() {
        long start = System.currentTimeMillis();
        try{statistics.addStatisticsObservers();
            Result result;
            ArrayList<int[]> clauses;
            clauses = basicClauseList.conjunctions;
            if(clauses != null) {
                for(int[] basicClause: clauses) {
                    result = addConjunction(basicClause);
                    if(result != null) {return result;}}}
            clauses = basicClauseList.equivalences;
            if(clauses != null) {
                for(int[] basicClause: clauses) {
                    result = addEquivalence(basicClause);
                    if(result != null) {return result;}}}
            clauses = basicClauseList.xors;
            if(clauses != null) {
                for(int[] basicClause: clauses) {
                    result = addXor(basicClause);
                    if(result != null) {return result;}}}
            clauses = basicClauseList.disjoints;
            if(clauses != null) {
                for(int[] basicClause: clauses) {
                    result = addDisjoint(basicClause);
                    if(result != null) {return result;}}}
            clauses = basicClauseList.disjunctions;
            if(clauses != null) {
                clauses.sort(Comparator.comparingInt(c->c.length));
                for(int[] basicClause: clauses) {
                    result = addDisjunction(basicClause);
                    if(result != null) {return result;}}}
            return purityCheck();}
        finally{statistics.removeStatisticsObservers();
            long end = System.currentTimeMillis();
            statistics.elapsedTime = end-start;}}


    /** This method adds a conjunction to the model.
     *
     * @param basicClause a conjunctive clause
     * @return Unsatisfiable if a contradiction has detected, otherwise null.
     */
    private Result addConjunction(int[] basicClause) {
        for(int i = 2; i < basicClause.length; ++i) {
            int literal = basicClause[i];
            if(model.add(literal) < 0) {
                return new Unsatisfiable(model,literal);}}
        return null;}

    /** adds an equivalence class to the clauses
     *
     * @param basicClause  an equivalence class
     * @return Unsatisfiable if a contradiction has detected, otherwise null.
     */
    private Result addEquivalence(int[] basicClause) {
        equivalences.addEquivalenceClass(basicClause);
        return processTasks();}

    /** adds a disjunctive clause.<br>
     * All possible simplifications on the clause itself and on the other clauses are performed.
     *
     * @param basicClause a disjunction
     * @return Unsatisfiable if a contradiction has detected, otherwise null.
     */
    public Result addDisjunction(int[] basicClause) {
        Clause clause = makeDisjunction(basicClause);
        if(clause == null) {return null;}
        taskQueue.add(makeShortenedClauseTask(clause,this));
        clauses.addClause(clause);
        return processTasks();}

    /** turns a basicClause into a clause. <br>
     * False literals and double literals are ignored. <br>~
     * True literals and complementary literals indicate tautologies. <br>
     * Literals are replaced by their representatives in an equivalence class.
     * Implied literals are removed, i.e.  p,q,r and p -&gt; r causes remove(p)
     *
     * @param basicClause the input clauses
     * @return the new simplified clause, or null if the clause is just to be ignored.
     */
    Clause makeDisjunction(int[] basicClause) {
        Clause clause = new Clause(""+basicClause[0],basicClause.length);
        for(int i = 2; i < basicClause.length;++i) {
            int literal = equivalences.mapToRepresentative(basicClause[i]);
            if(model.isTrue(literal)) {clause = null; break;}
            if(model.isFalse(literal)) {continue;}
            if(clause.addCLiteral(new CLiteral(literal)) == -1) {clause = null; break;}}
        if(clause == null) {
            ++((PreProcessorStatistics)statistics).BCL_RedundantClauses;
            return null;}
        if(clause.size() < basicClause.length-2) {
            ((PreProcessorStatistics)statistics).BCL_RedundantLiterals += basicClause.length-2-clause.size();
            if(monitoring) {monitor.print(id,"Clause " + Arrays.toString(basicClause) + " shortened to " + clause.toString());}}
        if(clause.isEmpty()) {return clause;}
        int removals = Algorithms.simplifyClause(clause,implicationDAG);
        if(removals != 0) {
            ((PreProcessorStatistics)statistics).BCL_ReplacementResolutions += removals;
            if(monitoring) {monitor.print(id,"Replacement Resolution removed " + removals + " from clause " + clause.toString());}}
        return clause;}


    /** adds an Xor clause
     *
     * @param basicClause the basic Xor clause
     * @return Unsatisfiable if a contradiction has detected, otherwise null.
     */
    public Result addXor(int[] basicClause) {
        Result result = addDisjunction(basicClause);
        if(result != null) {return result;}
        return addDisjoint(basicClause);}


    /** adds a disjointness clause to the clauses.
     *  All derivable implications are also added to the clause
     *
     * @param basicClause the basic disjointenss clause.
     * @return Unsatisfiable if a contradiction has detected, otherwise null.
     */
    public Result addDisjoint(int[] basicClause) {
        Clause clause = disjointnesses.addDisjointnessClass(basicClause);
        if(clause != null) {
            int size = clause.size();
            for(int i = 0; i < size; ++i) {
                int literal = clause.getLiteral(i);
                for (int j = i+1; j < size; ++j) {
                    implicationDAG.addImplication(literal,clause.getLiteral(j));}}}
        return processTasks();}




    /** checks the final clause list for purities.
     *  A literal -l is pure if -l does not occur any more in the clauses and
     *  and there are no literals implied by l in the implication DAG.
     *
     * @return null or Satisfiable if the clause set became empty.
     */
    private Result purityCheck() {
        clauses.addPurityObserver(purityObserver);
        for(Integer literal : clauses.pureLiterals()) {
            taskQueue.add(new Task.Purity(literal,this));}
        return processTasks();}


}
