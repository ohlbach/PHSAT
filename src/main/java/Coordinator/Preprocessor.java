package Coordinator;

import Algorithms.Algorithms;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Theory.*;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;

import java.util.*;

/**
 * Created by ohlbach on 14.09.2018.
 */
public class Preprocessor extends Processor {
    private PreprocessingStatistics preprocessingStatistics = new PreprocessingStatistics();

    public Preprocessor(HashMap<String,Object> globalParameters, HashMap<String,Object> problemParameters, BasicClauseList basicClauseList) {
        super(globalParameters,problemParameters,basicClauseList);
        model          = new Model(predicates);
        clauses        = new ClauseList(basicClauseList.disjunctions.size(),predicates);
        implicationDAG = new ImplicationDAG();
        equivalences   = new EquivalenceClasses(model, implicationDAG);
        disjointnesses = new DisjointnessClasses(model, implicationDAG,equivalences);
    }

    public Result prepareClauses() {
        Result result;
        ArrayList<int[]> clauses;
        clauses = basicClauseList.conjunctions;
        if(clauses != null) {
            for(int[] basicClause: clauses) {
                result = addConjunction(basicClause);
                if(result != null) {return result;}}}
        if(clauses != null) {
            for(int[] basicClause: clauses) {
                result = addEquivalence(basicClause);
                if(result != null) {return result;}}}
        clauses = basicClauseList.xor;
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


    /** This method adds a conjunction to the model.
     * It is assumed that no longer clauses are already in the model.
     *
     * @param basicClause a conjunctive clause
     * @return Unsatisfiable if a contradiction has detected, otherwise null.
     */
    private Result addConjunction(int[] basicClause) {
        for(int i = 2; i < basicClause.length; ++i) {
            int literal = basicClause[i];
            if(model.add(literal) < 0) {return new Unsatisfiable(model,literal);}}
        return null;}


    private Result addEquivalence(int[] basicClause) {
        equivalences.addEquivalenceClass(basicClause);
        return processTasks();}

    /** adds a disjunctive clause.<br/>
     * All possible simplifications on the clause itself and on the other clauses are performed.
     *
     * @param basicClause a disjunction
     * @return null or Unsatisfiable
     */
    public Result addDisjunction(int[] basicClause) {
        Clause clause = makeDisjunction(basicClause);
        if(clause == null) {return null;}
        taskQueue.add(makeShortenedClauseTask(clause));
        return processTasks();}

    /** turns a basicClause into a clause. <br/>
     * False literals and double literals are ignored. <br/>~
     * True literals and complementary literals indicate tautologies. <br/>
     * Literals are replaced by their representatives in an equivalence class.
     * Implied literals are removed, i.e.  p,q,r and p -&gt; r causes remove(p)
     *
     * @param basicClause
     * @return the new simplified clause, or null if the clause is just to be ignored.
     */
    Clause makeDisjunction(int[] basicClause) {
        Clause clause = new Clause(""+basicClause[0],basicClause.length);
        for(int i = 2; i < basicClause.length;++i) {
            int lit = basicClause[i];
            int literal = equivalences.mapToRepresentative(lit);
            if(literal != lit) {preprocessingStatistics.addEquivalenceReplacements(1);}
            if(model.isTrue(literal)  || clause.cliterals.contains(-literal)) {
                preprocessingStatistics.addTrueClauses(1);
                return null;}
            if(model.isFalse(literal) || clause.cliterals.contains(literal)) {
                preprocessingStatistics.addFalseLiterals(1);
                continue;}
            CLiteral cLiteral = new CLiteral(literal);
            int result = clause.addCLiteral(cLiteral);
            if(result == 1) {preprocessingStatistics.addDoubleLiterals(1); continue;}
            if(result == 1) {preprocessingStatistics.addtautologies(1); return null;}}
        int removals = Algorithms.simplifyClause(clause,implicationDAG);
        if(removals != 0) {preprocessingStatistics.addImplicationResolutions(removals);}
        return clause;}


    public Result addXor(int[] basicClause) {
        Result result = addDisjunction(basicClause);
        if(result != null) {return result;}
        return addDisjoint(basicClause);}


    public Result addDisjoint(int[] basicClause) {
        Clause clause = disjointnesses.addDisjointnessClass(basicClause);
        if(clause != null) {
            int size = clause.size();
            for(int i = 0; i < size; ++i) {
                int literal = clause.getLiteral(i);
                for (int j = i+1; j < size; ++j) {
                    implicationDAG.addImplication(literal,clause.getLiteral(j));}}}
        return processTasks();}

    protected Result processTasks() {
        while(!taskQueue.isEmpty()) {
            Task task = taskQueue.poll();
            Result result = task.execute();
            if(result != null) {return result;}}
        return null;}

}
