package Solvers.Walker;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Aborted;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Theory.Model;
import Management.ProblemSupervisor;
import Solvers.Solver;
import Utilities.Utilities;
import Utilities.IntegerQueue;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Random;

public class Walker extends Solver {

    private final ArrayList<WClause> wClauses;
    private final ArrayList<WClause>[] posOccurrences;
    private final ArrayList<WClause>[] negOccurrences;
    private final boolean[] localModel;
    private IntArrayList quantifiers = new IntArrayList();
    private int falseClauses = 0;
    /** sorts the predicates according to the flipScore. predicates whose flip makes more clauses true come to the front.*/
    private final IntegerQueue predicateQueue;
    private int jumpFrequency = 10;
    private int maxFlips = Integer.MAX_VALUE;

    /** collects statistical information */
    public WalkerStatistics statistics;

    private Random random;

    public static String help() {
        return "Random Walker: parameters:\n" +
                "seed:   for the random number generator      (default: random)\n" +
                "flips:  for restricting the number of flips  (default: Max_Integer).\n" +
                "jumps:  frequency of random jumps            (default: 10)\n";}

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
        localModel     = new boolean[predicates];
        predicateQueue = new IntegerQueue(predicates);
        Object seed    = solverParameters.get("seed");
        random         = (seed != null) ? new Random((int)seed) : new Random(0);
        Object flips   = solverParameters.get("flips");
        maxFlips       = (flips != null) ? (int)flips : Integer.MAX_VALUE;
        Object jumps   = solverParameters.get("jumps");
        jumpFrequency  = (jumps != null) ? (int)jumps : 10;
        statistics     = new WalkerStatistics(combinedId);
        wClauses       = new ArrayList<WClause>();}

    /** truansforms a clause to a WClause and adds it to the internal data
     *
     * @param clause a clause
     */
    public void addClause(Clause clause) {
        WClause wClause = new WClause(clause);
        wClauses.add(wClause);
        addWClauseToIndex(wClause);
        if(!quantifiers.contains(wClause.quantifier)) quantifiers.add(wClause.quantifier);}

    @Override
    public Result solve() {
        globalParameters.log(solverId + " for problem " + problemId + " started");
        super.initialize();
        long time = System.currentTimeMillis();
        initializeModel();

        Result result =  walk();
        statistics.elapsedTime = System.currentTimeMillis() - time;
        problemSupervisor.finished(this, result, "done");
        globalParameters.log(solverId + " for problem " + problemId + " finished");
        return result;}


    private Result walk() {
        while(statistics.flips < maxFlips) {
            if(Thread.interrupted()) {
                globalParameters.log("Walker " + combinedId + " interrupted after " + statistics.flips + " flips.\n");
                break;}
            int predicate = selectFlipPredicate();
            flipPredicate(predicate);
            if(falseClauses == 0) {return transferModel();}}
        return new Aborted("Walker aborted after " + statistics.flips + " flips");}

    /** flips the truth value of the predicate and updates the predicateQueue and the falseClauses counter
     *
     * @param predicate to be flipped
     */
    void flipPredicate(int predicate) {
        ++statistics.flips;
        localModel[predicate] = !localModel[predicate];
        updateFlipScores(predicate);}

    /** turns the local model into a new model and returns Satisfiable as result
     *
     * @return Satisfiable with the transferred local model.
     */
    private Satisfiable transferModel() {
        Model model = new Model(predicates,this.model.symboltable);
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            model.addImmediately(localModel[predicate] ? predicate : -predicate);}
        return new Satisfiable(model);}

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
            if(status != 0) {localModel[predicate] = status == 1; continue;}
            localModel[predicate] = (posScores[predicate] >= negScores[predicate]);}}

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

        int trueLiterals = currentlyTrueLiterals(wClause);
        int quantifier = wClause.quantifier;

        switch (clauseType) {
            case ATLEAST: return trueLiterals >= quantifier;
            case ATMOST:  return trueLiterals <= quantifier;
            case EXACTLY: return trueLiterals == quantifier;}
        return false;}

    /** computes the initial flip scores for all literals.
     * The flip score for a predicate p counts the net number of clauses which become true when flipping the
     * truth-value of p, i.e. additional true clauses - new false clauses.
     */
    private void setInitialFlipScores() {
        for(WClause wClause : wClauses) {
            if(wClause.isGloballyTrue) continue;
            boolean hasDoubles = wClause.hasDoubles;
            int trueLiterals = currentlyTrueLiterals(wClause);
            switch(wClause.clauseType) {
                case OR:
                    if(hasDoubles)
                        addScoreAtleastWithDoubles(wClause,trueLiterals);
                    else addScoreAtleast(wClause,trueLiterals);
                        break;
                case ATLEAST:
                    if(hasDoubles)
                        addScoreAtleastWithDoubles(wClause,trueLiterals);
                    else addScoreAtleast(wClause,trueLiterals);
                    break;
                case ATMOST:
                    if(hasDoubles)
                        addScoreAtmostWithDoubles(wClause,trueLiterals);
                    else addScoreAtmost(wClause,trueLiterals);
                    break;
                case EXACTLY:
                    if(hasDoubles)
                        addScoreExactlyWithDoubles(wClause,trueLiterals);
                    else addScoreExactly(wClause,trueLiterals);
                    break;}}}


    private void updateFlipScores(int flippedLiteral) {
        for(WClause wClause : getClauses(flippedLiteral)) {
            if(wClause.isGloballyTrue) continue;
            boolean hasDoubles = wClause.hasDoubles;
            switch(wClause.clauseType) {
                case OR:
                    if(hasDoubles)
                        updateFlipScoresATLEASTwithDoubles(flippedLiteral,wClause);
                    else updateFlipScoresATLEAST(flippedLiteral,wClause);
                    break;
                case ATLEAST:
                    if(hasDoubles)
                        updateFlipScoresATLEASTwithDoubles(flippedLiteral,wClause);
                    else updateFlipScoresATLEAST(flippedLiteral,wClause);
                    break;
                case ATMOST:
                    if(hasDoubles)
                        updateFlipScoresATMOSTwithDoubles(flippedLiteral,wClause);
                    else updateFlipScoresATMOST(flippedLiteral,wClause);
                    break;
                case EXACTLY:
                    if(hasDoubles)
                        updateFlipScoresEXACTLYwithDoubles(flippedLiteral,wClause);
                    else updateFlipScoresEXACTLY(flippedLiteral,wClause);
                    break;}}}



    /** updates the flip score for an Atleast-clause which contains the flipped literal.
     *
     * @param flippedLiteral the flipped literal
     * @param wClause an Atleast-clause containing the flipped literal
     */
    private void updateFlipScoresATLEAST(int flippedLiteral, WClause wClause) {
        int quantifier = wClause.quantifier;
        int newTrueLiterals = currentlyTrueLiterals(wClause);
        int oldTrueLiterals = isLocallyTrue(flippedLiteral) ? newTrueLiterals -1 : newTrueLiterals +1;
        if(oldTrueLiterals < quantifier) --falseClauses;
        if(newTrueLiterals < quantifier) ++falseClauses;
        // undo old flip scores
        if(oldTrueLiterals == quantifier) {
            for(int literal : wClause.literals) {
                if(literal != flippedLiteral && isLocallyTrue(literal))   // flipping it made the clause false.
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: -1. Must be undone.
        else {
            if(oldTrueLiterals == quantifier-1) {
                for(int literal : wClause.literals) {
                    if(literal != flippedLiteral && !isLocallyTrue(literal))   // flipping it made the clause true.
                        predicateQueue.addScore(Math.abs(literal),-1);}} // score: +1. Must be undone.
            }
        addScoreAtleast(wClause,newTrueLiterals);}


    private void addScoreAtleast(WClause wClause,int trueLiterals) {
        int quantifier = wClause.quantifier;
        if(trueLiterals == quantifier) {        // just enough true literals
            for(int literal : wClause.literals) {
                if(isLocallyTrue(literal))         // flipping a true literal makes the clause false.
                    predicateQueue.addScore(Math.abs(literal),-1);} // score: -1.
            return;}
        if(trueLiterals == quantifier-1) {       // not enough true literals
            for(int literal : wClause.literals) {
                if(!isLocallyTrue(literal))         // flipping a false literal it makes the clause true.
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: +1.
        } // all other cases do not change anything.

    /** updates the flip score for an Atleast-clause with double literals which contains the flipped literal.
     *
     * @param flippedLiteral the flipped literal
     * @param wClause an Atleast-clause containing the flipped literal
     */
    private void updateFlipScoresATLEASTwithDoubles(int flippedLiteral, WClause wClause) {
        int quantifier = wClause.quantifier;
        int newTrueLiterals = currentlyTrueLiterals(wClause);
        int oldTrueLiterals = isLocallyTrue(flippedLiteral) ?
                newTrueLiterals - wClause.multiplicity(flippedLiteral) :
                newTrueLiterals + wClause.multiplicity(flippedLiteral);
        if(oldTrueLiterals < quantifier) --falseClauses;
        if(newTrueLiterals < quantifier) ++falseClauses;
        int[] literals = wClause.literals;
        int length = literals.length;

        // undo old flip scores
        if(oldTrueLiterals == quantifier) { // the clause was true
            for(int i = 0; i < length; ++i) {
                int literal = literals[i]; // flipping a true literal made the clause false.
                if(literal != flippedLiteral && isLocallyTrue(literal) && !contains(literals,literal,i))
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: -1. Must be undone.
        else {
            if(oldTrueLiterals < quantifier) { // the clause was false
                for(int i = 0; i < length; ++i) {
                    int literal = literals[i]; // flipping a false literal with enough multiplicity made the clause true.
                    if(literal != flippedLiteral && !isLocallyTrue(literal) &&
                            (oldTrueLiterals + wClause.multiplicity(literal) > quantifier)  )
                        predicateQueue.addScore(Math.abs(literal),-1);}} // score: +1. Must be undone.
        }
        // new flip scores
        addScoreAtleastWithDoubles(wClause,newTrueLiterals);}


    private void addScoreAtleastWithDoubles(WClause wClause,int trueLiterals) {
        int quantifier = wClause.quantifier;
        int[] literals = wClause.literals;
        int length = literals.length;

        if(trueLiterals == quantifier) {        // clause ist true, just enough true literals
            for(int i = 0; i < length; ++i) {
                int literal = literals[i];
                if(isLocallyTrue(literal) && !contains(literals,literal,i))         // flipping a true literal makes the clause false.
                    predicateQueue.addScore(Math.abs(literal),-1);} // score: -1.
            return;}
        if(trueLiterals < quantifier) {       // clause is false, not enough true literals
            for(int i = 0; i < length; ++i) {
                int literal = literals[i]; // flipping a false literal with enough multiplicity made the clause true.
                if(!isLocallyTrue(literal) && (trueLiterals + wClause.multiplicity(literal) > quantifier) &&
                        !contains(literals,literal,i))
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: +1.
    } // all other cases do not change anything.

    /** updates the flip score for an Atmost-clause which contains the flipped literal.
     *
     * @param flippedLiteral the flipped literal
     * @param wClause an Atmost-clause containing the flipped literal
     */
    private void updateFlipScoresATMOST(int flippedLiteral, WClause wClause) {
        int quantifier = wClause.quantifier;
        int newTrueLiterals = currentlyTrueLiterals(wClause);
        int oldTrueLiterals = isLocallyTrue(flippedLiteral) ? newTrueLiterals -1 : newTrueLiterals +1;
        if(oldTrueLiterals > quantifier) --falseClauses;
        if(newTrueLiterals > quantifier) ++falseClauses;
        // undo old flip scores
        if(oldTrueLiterals == quantifier) {
            for(int literal : wClause.literals) {
                if(literal != flippedLiteral && !isLocallyTrue(literal))   // flipping it made the clause false.
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: -1. Must be undone.
        else {
            if(oldTrueLiterals == quantifier+1) {
                for(int literal : wClause.literals) {
                    if(literal != flippedLiteral && isLocallyTrue(literal))   // flipping it made the clause true.
                        predicateQueue.addScore(Math.abs(literal),-1);}} // score: +1. Must be undone.
        }
        // new flip scores
        addScoreAtmost(wClause,newTrueLiterals);}

    private void addScoreAtmost(WClause wClause,int trueLiterals) {
        int quantifier = wClause.quantifier;
        int[] literals = wClause.literals;

        if(trueLiterals == quantifier) {                // just enough true literals
            for(int literal : wClause.literals) {
                if(!isLocallyTrue(literal))                // flipping a false literal it makes the clause false.
                    predicateQueue.addScore(Math.abs(literal),-1);} // score: -1.
            return;}

        if(trueLiterals == quantifier+1) {             // too many true literals
            for(int literal : wClause.literals) {
                if(isLocallyTrue(literal))                // flipping a true literal it makes the clause true.
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: +1.
        } // all other cases do not change anything.

    /** updates the flip score for an Atmost-clause with double literals which contains the flipped literal.
     *
     * @param flippedLiteral the flipped literal
     * @param wClause an Atmost-clause containing the flipped literal
     */
    private void updateFlipScoresATMOSTwithDoubles(int flippedLiteral, WClause wClause) {
        int quantifier = wClause.quantifier;
        int newTrueLiterals = currentlyTrueLiterals(wClause);
        int oldTrueLiterals = isLocallyTrue(flippedLiteral) ?
                newTrueLiterals - wClause.multiplicity(flippedLiteral) :
                newTrueLiterals + wClause.multiplicity(flippedLiteral);
        if(oldTrueLiterals > quantifier) --falseClauses;
        if(newTrueLiterals > quantifier) ++falseClauses;
        int[] literals = wClause.literals;
        int length = literals.length;

        // undo old flip scores
        if(oldTrueLiterals == quantifier) {      // the clause was true
            for(int i = 0; i < length; ++i) {
                int literal = literals[i];       // flipping a false literal made the clause false.
                if(literal != flippedLiteral && !isLocallyTrue(literal) && !contains(literals,literal,i))
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: -1. Must be undone.
        else {
            if(oldTrueLiterals > quantifier) {     // the clause was false
                for(int i = 0; i < length; ++i) {
                    int literal = literals[i];    // flipping a true literal with enough multiplicity made the clause true.
                    if(literal != flippedLiteral && isLocallyTrue(literal) &&
                            (oldTrueLiterals - wClause.multiplicity(literal) <= quantifier) &&
                            !contains(literals,literal,i))
                        predicateQueue.addScore(Math.abs(literal),-1);}} // score: +1. Must be undone.
        }
        // new flip scores
        addScoreAtmostWithDoubles(wClause,newTrueLiterals);}

    private void addScoreAtmostWithDoubles(WClause wClause,int trueLiterals) {
        int quantifier = wClause.quantifier;
        int[] literals = wClause.literals;
        int length = literals.length;

        if(trueLiterals == quantifier) {     // the clause is true, just enough true literals
            for(int i = 0; i < length; ++i) {
                int literal = literals[i];
                if(!isLocallyTrue(literal))                // flipping a false literal makes the clause false.
                    predicateQueue.addScore(Math.abs(literal),-1);} // score: -1.
            return;}

        if(trueLiterals > quantifier) {     // the clause is false, too many true literals
            for(int i = 0; i < length; ++i) {
                int literal = literals[i];
                if(isLocallyTrue(literal) && // flipping a true literal with enough multiplicity makes the clause true.
                        (trueLiterals - wClause.multiplicity(literal) <= quantifier))
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: +1.
    } // all other cases do not change anything.


    /** updates the flip score for an Exactly-clause which contains the flipped literal.
     *
     * @param flippedLiteral the flipped literal
     * @param wClause an Exactly-clause containing the flipped literal
     */
    private void updateFlipScoresEXACTLY(int flippedLiteral, WClause wClause) {
        int quantifier = wClause.quantifier;
        int newTrueLiterals = currentlyTrueLiterals(wClause);
        int oldTrueLiterals = isLocallyTrue(flippedLiteral) ? newTrueLiterals -1 : newTrueLiterals +1;
        if(oldTrueLiterals != quantifier) --falseClauses;
        if(newTrueLiterals != quantifier) ++falseClauses;
        // undo old flip scores
        if(oldTrueLiterals == quantifier) {
            for(int literal : wClause.literals) {
                if(literal != flippedLiteral)   // flipping it made the clause false.
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: -1. Must be undone.
        else {
            if(oldTrueLiterals == quantifier+1) {
                for(int literal : wClause.literals) {
                    if(literal != flippedLiteral && isLocallyTrue(literal))   // flipping it made the clause true.
                        predicateQueue.addScore(Math.abs(literal),-1);}} // score: +1. Must be undone.else {
            else {if(oldTrueLiterals == quantifier-1) {
                    for(int literal : wClause.literals) {
                        if(literal != flippedLiteral && !isLocallyTrue(literal))   // flipping it made the clause true.
                            predicateQueue.addScore(Math.abs(literal),-1);}}} // score: +1. Must be undone.
        }
        // new flip scores
        addScoreExactly(wClause,newTrueLiterals);}

    private void addScoreExactly(WClause wClause,int trueLiterals) {
        int quantifier = wClause.quantifier;

        if(trueLiterals == quantifier) {             // the right number of true literals
            for(int literal : wClause.literals) {       // flipping any literal makes the clause false.
                predicateQueue.addScore(Math.abs(literal),-1);} // score: -1.
            return;}

        if(trueLiterals == quantifier+1) {      // too many true literals
            for(int literal : wClause.literals) {
                if(isLocallyTrue(literal))         // flipping a true literal makes the clause true.
                    predicateQueue.addScore(Math.abs(literal),+1);}
            return;}

        if(trueLiterals == quantifier-1) {      // not enough true literals
            for(int literal : wClause.literals) {
                if(!isLocallyTrue(literal))        // flipping a false literal makes the clause true.
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: +1.
        } // all other cases do not change anything.

    /** updates the flip score for an Exactly-clause with doubles which contains the flipped literal.
     *
     * @param flippedLiteral the flipped literal
     * @param wClause an Exactly-clause containing the flipped literal
     */
    private void updateFlipScoresEXACTLYwithDoubles(int flippedLiteral, WClause wClause) {
        int quantifier = wClause.quantifier;
        int newTrueLiterals = currentlyTrueLiterals(wClause);
        int oldTrueLiterals = isLocallyTrue(flippedLiteral) ? newTrueLiterals -1 : newTrueLiterals +1;
        if(oldTrueLiterals != quantifier) --falseClauses;
        if(newTrueLiterals != quantifier) ++falseClauses;
        int[] literals = wClause.literals;
        int length = literals.length;
        // undo old flip scores
        if(oldTrueLiterals == quantifier) {
            for(int i = 0; i < length; ++i) {
                int literal = literals[i];
                if(literal != flippedLiteral && !contains(literals,literal,i))   // flipping it made the clause false.
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: -1. Must be undone.
        else {
            if(oldTrueLiterals == quantifier+1) {
                for(int i = 0; i < length; ++i) {
                    int literal = literals[i];
                    if(literal != flippedLiteral && isLocallyTrue(literal) && !contains(literals,literal,i))   // flipping it made the clause true.
                        predicateQueue.addScore(Math.abs(literal),-1);}} // score: +1. Must be undone.else {
            else {if(oldTrueLiterals == quantifier-1) {
                for(int i = 0; i < length; ++i) {
                    int literal = literals[i];
                    if(literal != flippedLiteral && !isLocallyTrue(literal) && !contains(literals,literal,i))   // flipping it made the clause true.
                        predicateQueue.addScore(Math.abs(literal),-1);}}} // score: +1. Must be undone.
        }
        // new flip scores
        addScoreExactlyWithDoubles(wClause,newTrueLiterals);}

    private void addScoreExactlyWithDoubles(WClause wClause,int trueLiterals) {
        int quantifier = wClause.quantifier;
        int[] literals = wClause.literals;
        int length = literals.length;

        if(trueLiterals == quantifier) {             // the right number of true literals
            for(int i = 0; i < length; ++i) {
                int literal = literals[i];       // flipping any literal makes the clause false.
                if(!contains(literals,literal,i)) predicateQueue.addScore(Math.abs(literal),-1);} // score: -1.
            return;}

        if(trueLiterals == quantifier+1) {      // too many true literals
            for(int i = 0; i < length; ++i) {
                int literal = literals[i];
                if(isLocallyTrue(literal) && !contains(literals,literal,i))    // flipping a true literal makes the clause true.
                    predicateQueue.addScore(Math.abs(literal),+1);}
            return;}

        if(trueLiterals == quantifier-1) {      // not enough true literals
            for(int i = 0; i < length; ++i) {
                int literal = literals[i];
                if(!isLocallyTrue(literal) && !contains(literals,literal,i))        // flipping a false literal makes the clause true.
                    predicateQueue.addScore(Math.abs(literal),+1);}} // score: +1.
    } // all other cases do not change anything.


    int oldPredicate,oldoldPredicate;

    /** selects the next flip predicate.
     * Normally the next flip predicate is the top of the predicateQueue.
     * Only if the same flip predicate has been selected the last or second but last time,
     * it is the second flip predicate in the predicateQueue.
     *
     * If the flips has reached a multiple of the jumpFrequency, then a random predicate is chosen.
     * @return the next flip predicate.
     */
    int selectFlipPredicate() {
        int predicate = 0;
        if(statistics.flips % jumpFrequency == 0) {
            predicate =  predicateQueue.getRandom(random,3);}
        if(predicateQueue.getScore(predicate) <= 0) {predicate = predicateQueue.nthTopScore(2);}
        else {
            predicate = predicateQueue.topScore();
            if(predicate == oldPredicate || predicate == oldoldPredicate) {
                predicate = predicateQueue.nthTopScore(1);}}
        oldoldPredicate = oldPredicate;
        oldPredicate = predicate;
        return predicate;}

    /** checks if the given literal is in literals[0] ... literals[i-1]
     *
     * @param literals an array of literals
     * @param literal a literal
     * @param index an index into the array
     * @return true if the given literal is in literals[0] ... literals[i-1]
     */
    private boolean contains(int[] literals, int literal, int index) {
        for(int i = 0; i < index; ++i) {if(literals[i] == literal); return  true;}
        return false;}

    /** counts the number of loally true literals in the clause
         *
         * @param wClause a clause
         * @return the number of true literals in the clause.
         */
    private int currentlyTrueLiterals(WClause wClause) {
        int trueLiterals = 0;
        for(int literal : wClause.literals) {if(isLocallyTrue(literal)) ++trueLiterals;}
        return trueLiterals;}

    /** returns +1 if the literal is true, otherwise -1
     *
     * @param literal a literal
     * @return +1 if the literal is true, otherwise -1
     */
    private boolean isLocallyTrue(int literal) {
        return (literal > 0) ? localModel[literal] : !localModel[-literal];}


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
