package Solvers.Walker;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Result;
import Datastructures.Statistics.Statistic;
import Management.ProblemSupervisor;
import Solvers.Solver;
import Utilities.Utilities;
import Utilities.IntegerQueue;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.HashMap;

public class Walker extends Solver {

    private final ArrayList<WClause> wClauses = new ArrayList<>();
    private final ArrayList<WClause>[] posOccurrences;
    private final ArrayList<WClause>[] negOccurrences;
    private final int[] flipScores;
    private final byte[] localModel;
    private IntArrayList quantifiers;
    private int maxQuantifier;
    private int falseClauses = 0;
    /** sorts the predicates according to the flipScore. predicates whose flip makes more clauses true come to the front.*/
    private final IntegerQueue predicateQueue;



    /** constructs a new Walker solver.
     *
     * @param solverNumber         for distinguishing different solvers of the same type, but different parameters
     * @param solverParameters     contains the parameters for controlling the solver
     * @param problemSupervisor    coordinates several solvers.
     */
    public Walker(Integer solverNumber, HashMap<String,Object> solverParameters, ProblemSupervisor problemSupervisor) {
        super(solverNumber,solverParameters, problemSupervisor);
        posOccurrences = new ArrayList[predicates+1];
        negOccurrences = new ArrayList[predicates+1];
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            posOccurrences[predicate] = new ArrayList<>();
            negOccurrences[predicate] = new ArrayList<>();}
        flipScores = new int[predicates+1];
        localModel = new byte[predicates];
        predicateQueue = new IntegerQueue(predicates);
    }

    public void addClause(Clause clause) {
        WClause wClause = new WClause(clause);
        wClauses.add(wClause);
        addWClauseToIndex(wClause);
        if(!quantifiers.contains(wClause.quantifier)) quantifiers.add(wClause.quantifier);
    }

    @Override
    public Result solve() {

        return null;
    }

    /** Initializes the local model.
     * A predicate is made true if it makes more clauses true than making the predicate false.
     * The number of true clauses when making a predicat true is estimated by its initial score.
     * Global truth value are transferred unchanged to the local model.
     */
    private void initializeModel() {
        int[] posScores = new int[predicates+1];
        int[] negScores = new int[predicates+1];;
        setInitialScores(posScores,negScores);
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            int status = model.status(predicate);
            if(status != 0) {localModel[predicate] = (byte)status; continue;}
            localModel[predicate] = (posScores[predicate] >= negScores[predicate]) ? (byte)+1 : (byte)-1;}}

    /** computes the initial predicate scores.
     * The higher the score of a predicate the more likely it is to make clauses true.
     * If there are no numeric clause types then
     * posScore[5] = 10 means that it is very likely to make 10 clauses true, if the predicate is made true.<br>
     * For numeric clause types truth does not depend on one literal. <br>
     * Example: atleast 2 p,q,r <br>
     *  In this case two predicates must be made true. <br>
     *  Therefore the score of p, q, and r is incremented by 0.5.<br>
     * Example: atmost 2 p,q,r <br>
     * In this case making a predicate true decrements the chance to make the clause true. <br>
     * Therefore the score of p, q, and r is decremented by 0.5.
     * <br>
     * Example: exactly 2 p,q,r <br>
     * In this case making a predicate true or false has the same chance to make the clause true or false.
     * The score is therefore not changed.
     * <br>
     * In order to keep the scores an integer, they are multiplied by the prduct of all quantifiers.
     *
     * @param posScores for setting the scores to make a predicate true.
     * @param negScores for setting the scores to make a predicate false.
     */
    private void setInitialScores(int[] posScores,int[] negScores) {
        int productQuantifier = Utilities.product(quantifiers);
        for(WClause wClause: wClauses) {
            ClauseType clauseType = wClause.clauseType;
            if(clauseType == ClauseType.EXACTLY) continue;
            int score = productQuantifier;
            switch(wClause.clauseType) {
                case ATLEAST: score /=  wClause.quantifier; break;
                case ATMOST:  score /= -wClause.quantifier; break;}
            for(int literal : wClause.literals) {
                if(literal > 0) posScores[literal] += score;
                else            negScores[literal] += score;}}}

    /** sets the initial global and local truth values of the clauses and counts the number of false clauses
     */
    private void setInitialTruthValues () {
        for(WClause wClause : wClauses) {
            if(!setInitialTruthValue(wClause)) ++falseClauses;}}

    /** sets the initial global and local truth values of a given clause.
     *
     * @param wClause a clause
     * @return true if the clause is true (globally or locally)
     */
    private boolean setInitialTruthValue(WClause wClause) {
        ClauseType clauseType = wClause.clauseType;
        if(clauseType == ClauseType.OR) {
            for(int literal : wClause.literals) {
                switch(model.status(literal)) {
                    case +1: wClause.isGloballyTrue = true; return true;
                    case -1: continue;}
                if(isLocallyTrue(literal)) {wClause.isLocallyTrue = true; return true;} }
            return false;}

        int globallyTrueLiterals = 0;
        int locallyTrueLiterals = 0;
        for(int literal : wClause.literals) {
            switch(model.status(literal)) {
                case +1: ++globallyTrueLiterals; break;
                case -1: continue;}
            if(isLocallyTrue(literal)) ++locallyTrueLiterals;}

        int trueLiterals = globallyTrueLiterals + locallyTrueLiterals;
        int quantifier = wClause.quantifier;
        switch (clauseType) {
            case ATLEAST:
                if(globallyTrueLiterals >= quantifier) {wClause.isGloballyTrue = true; return true;}
                if(trueLiterals >= quantifier)         {wClause.isLocallyTrue = true;  return true;}
            case ATMOST:
                if(globallyTrueLiterals <= quantifier) {wClause.isGloballyTrue = true; return true;}
                if(trueLiterals <= quantifier)         {wClause.isLocallyTrue = true;  return true;}
            case EXACTLY:
                if(globallyTrueLiterals == quantifier) {wClause.isGloballyTrue = true; return true;}
                if(trueLiterals == quantifier)         {wClause.isLocallyTrue = true;  return true;}}
        return false;}

    /** computes the truth value of a clause in the local (and global) model
     *
     * @param wClause a clause
     * @return true if the clause is true in the local (and global) model
     */
    private boolean getTruthValue(WClause wClause) {
        if(wClause.isGloballyTrue) return true;
        ClauseType clauseType = wClause.clauseType;

        if(clauseType == ClauseType.OR) {
            for(int literal : wClause.literals) {
                if(isLocallyTrue(literal)) {return true;} }
            return false;}

        int trueLiterals = 0;
        for(int literal : wClause.literals) {if(isLocallyTrue(literal)) ++trueLiterals;}

        int quantifier = wClause.quantifier;
        switch (clauseType) {
            case ATLEAST: return trueLiterals >= quantifier;
            case ATMOST:  return trueLiterals <= quantifier;
            case EXACTLY: return trueLiterals == quantifier;}
        return false;}


    /** returns +1 if the literal is true, otherwise -1
     *
     * @param literal a literal
     * @return +1 if the literal is true, otherwise -1
     */
    private boolean isLocallyTrue(int literal) {
        return (literal > 0) ? localModel[literal] == (byte)1 :localModel[-literal] == (byte)-1;}


    private void addWClauseToIndex(WClause wClause) {
        for(int literal : wClause.literals) {
            if(literal > 0) posOccurrences[literal].add(wClause);
            else            posOccurrences[-literal].add(wClause);}}

    private ArrayList<WClause> getClauses(int literal) {
        return (literal > 0) ? posOccurrences[literal] : negOccurrences[-literal];}



    @Override
    public Statistic getStatistics() {
        return null;
    }
}
