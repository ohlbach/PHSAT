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
import java.util.Arrays;
import java.util.HashMap;

public class Walker extends Solver {

    private final ArrayList<WClause> wClauses = new ArrayList<>();
    private final ArrayList<WClause>[] posOccurrences;
    private final ArrayList<WClause>[] negOccurrences;
    private final boolean[] localModel;
    private IntArrayList quantifiers = new IntArrayList();
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
        localModel = new boolean[predicates];
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

        int trueLiterals = 0;
        for(int literal : wClause.literals) {if(isLocallyTrue(literal)) ++trueLiterals;}

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
            switch(wClause.clauseType) {
                case OR:       setInitialFlipScoreOR(wClause);      break;
                case ATLEAST:  setInitialFlipScoreATLEAST(wClause); break;
                case ATMOST:   setInitialFlipScoreATMOST(wClause);  break;
                case EXACTLY:  setInitialFlipScoreEXACTLY(wClause); break;}}}

    /** computes the contribution of the OR-clause to its predicates flip scores.
     * If the clause is false then flipping any of the literals makes the clause true.<br>
     * The flip scores of all literals are increased by 1.
     * <br>
     * If the clause is true because two or more literals are true then flipping any of the predicates does not
     * make the clause false.
     * <br>
     * If the clause is true because only one literal is true then flipping this one makes the clause false.
     * Its score contribution is -1. Flipping any of the other literals does not change the truth value of the clause.
     *
     * @param wClause an OR-Clause
     */
    private void setInitialFlipScoreOR(WClause wClause) {
        if(wClause.isLocallyTrue) {
            int trueLiteral = 0;
            for(int literal : wClause.literals) {
                if(isLocallyTrue(literal)) {
                    if(trueLiteral != 0) { // there are at least two true literals.
                                           // Flipping any of the literals does not make the clause false;
                        trueLiteral = Integer.MAX_VALUE; break;}
                    else trueLiteral = literal;}}
            if(trueLiteral != Integer.MAX_VALUE) // flipping the only true literal makes the clause false.
                predicateQueue.addScore(Math.abs(trueLiteral),-1);}
        else { // all literals are false. Flipping each one of them makes the clause true
            for(int literal : wClause.literals) predicateQueue.addScore(Math.abs(literal),1);}}

    /** adds the initial flip score to the literals in wClause.
     * The flip score for a predicate p counts the net number of clauses which become true when flipping the
     * truth-value of p, i.e. additional true clauses - new false clauses.
     * <br>
     * For a clause like atleast 2 p,q,r,s: <br>
     * If 3 or more literals are true, then flipping the truth value of any of the predicates does not change anything <br>
     * IF exactly 2 literals are true, the flipping one of the true literals makes the clause false.
     * <br>
     * If only 1 literal is true then flipping one of the false literals makes the clause true.
     * <br>
     * Special treatment is necessary if the clause contains multiple literals
     * (which can happen if equivalent literals have been replaced by their representative)
     *
     * @param wClause an atleast-clause to be tested.
     */
    private void setInitialFlipScoreATLEAST(WClause wClause) {
        if(wClause.hasDoubles) {setInitialFlipScoreATLEASTwithDoubles(wClause); return;}
        int trueLiterals = 0;
        for(int literal : wClause.literals) {if(isLocallyTrue(literal)) ++trueLiterals;}
        int quantifier = wClause.quantifier;

        if(wClause.isLocallyTrue) {
            if(trueLiterals > quantifier) {return;} // flipping any of the literals does not change the clause's truth value
            for(int literal : wClause.literals) {   // flipping a true literal makes the clause false
                if(isLocallyTrue(literal)) predicateQueue.addScore(Math.abs(literal),-1);}}
        else {
            if(trueLiterals == quantifier-1) {
                for(int literal : wClause.literals) {  // flipping a false literal makes the clause true
                    if(!isLocallyTrue(literal)) predicateQueue.addScore(Math.abs(literal),1);}}}}

    private IntArrayList trueLiterals = new IntArrayList();

    /** adds the initial flip score to the literals in wClause with double literals.
     * Flipping the truth value of a literal which occurs 2 or more times in a clause
     * changes the counting of true literals by its multiplicity.
     * This must be taken into account.
     *
     * @param wClause an atleast-clause to be tested.
     */
    private void setInitialFlipScoreATLEASTwithDoubles(WClause wClause) {
        trueLiterals.clear();
        int trueLits = 0;
        for(int literal : wClause.literals) { // first we collect the true literals
            if(isLocallyTrue(literal)) {
                ++trueLits; // total number of true literals, with multiplicities
                if(!trueLiterals.contains(literal)) trueLiterals.add(literal);}} // true literals without multiplicities

        int quantifier = wClause.quantifier;
        if(wClause.isLocallyTrue) {
            for(int literal : trueLiterals) {
                if(trueLits - wClause.multiplicities.get(literal) < quantifier) {
                    predicateQueue.addScore(Math.abs(literal),-1);}}}
        else {
            int falseLits = wClause.literals.length - trueLits;
            for(int literal : wClause.literals) {
                if(!isLocallyTrue(literal) && (falseLits + wClause.multiplicities.get(literal) >= quantifier))
                    predicateQueue.addScore(Math.abs(literal),1);}}
}
    /** adds the initial flip score to the literals in wClause.
     * The flip score for a predicate p counts the net number of clauses which become true when flipping the
     * truth-value of p, i.e. additional true clauses - new false clauses.
     * <br>
     * For a clause like atmost 3 p,q,r,s,t: <br>
     * If 3 or less literals are true then flipping any of them does not change anything <br>
     * If exactly 3 literals are true then flipping one of the false literals makes the clause false.<br>
     * If 4 literals are true then flipping one of the true literals makes the clause false.<br>
     * If more than 4 literals are true then nothing is changed by flipping a literal.
     * <br>
     * Special treatment is necessary if the clause contains multiple literals
     * (which can happen if equivalent literals have been replaced by their representative)
     *
     * @param wClause an atmost-clause to be tested.
     */
    private void setInitialFlipScoreATMOST(WClause wClause) {
        if(wClause.hasDoubles) {setInitialFlipScoreATMOSTwithDoubles(wClause); return;}
        int trueLiterals = 0;
        for(int literal : wClause.literals) {if(isLocallyTrue(literal)) ++trueLiterals;}
        int quantifier = wClause.quantifier;

        if(wClause.isLocallyTrue) {
            if(trueLiterals < quantifier) {return;} // flipping any of the literals does not change the clause's truth value
            for(int literal : wClause.literals) {   // flipping a false literal makes the clause false
                if(!isLocallyTrue(literal)) predicateQueue.addScore(Math.abs(literal),-1);}}
        else {
            if(trueLiterals == quantifier+1) {
                for(int literal : wClause.literals) {  // flipping a superfluous true literal makes the clause true
                    if(isLocallyTrue(literal)) predicateQueue.addScore(Math.abs(literal),1);}}}}

    /** adds the initial flip score to the literals in wClause with double literals.
     * Flipping the truth value of a literal which occurs 2 or more times in a clause
     * changes the counting of true literals by its multiplicity.
     * This must be taken into account.
     *
     * @param wClause an atleast-clause to be tested.
     */
    private void setInitialFlipScoreATMOSTwithDoubles(WClause wClause) {
        trueLiterals.clear();
        int trueLits = 0;
        for(int literal : wClause.literals) { // first we collect the true literals
            if(isLocallyTrue(literal)) {
                ++trueLits; // total number of true literals, with multiplicities
                if(!trueLiterals.contains(literal)) trueLiterals.add(literal);}} // true literals without multiplicities

        int quantifier = wClause.quantifier;
        if(wClause.isLocallyTrue) { // too little true literals
            int falseLits = wClause.literals.length - trueLits;
            for(int literal : wClause.literals) {
                if(!isLocallyTrue(literal) && (falseLits + wClause.multiplicities.get(literal) >= quantifier))
                    predicateQueue.addScore(Math.abs(literal),-1);}}
            else { // too many true literals. Some of them must become false to make the clause true
                for(int literal : trueLiterals) {
                    if(trueLits - wClause.multiplicities.get(literal) <= quantifier) {
                        predicateQueue.addScore(Math.abs(literal),1);}}}}

    /** adds the initial flip score to the literals in wClause.
     * The flip score for a predicate p counts the net number of clauses which become true when flipping the
     * truth-value of p, i.e. additional true clauses - new false clauses.
     * <br>
     * For a clause like exactly 3 p,q,r,s,t: <br>
     * If exactly 3 literals are true then flipping any of the literals them makes the clause false.<br>
     * If 4 literals are true then flipping one of the true literals makes the clause true.<br>
     * If 3 literals are true then flipping one of the false literals makes the clause true.<br>
     * In the other cases nothing changes.
     * <br>
     * Special treatment is necessary if the clause contains multiple literals
     * (which can happen if equivalent literals have been replaced by their representative)
     *
     * @param wClause an atmost-clause to be tested.
     */
    private void setInitialFlipScoreEXACTLY(WClause wClause) {
        if(wClause.hasDoubles) {setInitialFlipScoreEXACTLYwithDoubles(wClause); return;}
        int trueLiterals = 0;
        for(int literal : wClause.literals) {if(isLocallyTrue(literal)) ++trueLiterals;}
        int quantifier = wClause.quantifier;

        if(wClause.isLocallyTrue) { // exactly quantifier may literals are true
            for(int literal : wClause.literals) {   // flipping any literal makes the clause false
                predicateQueue.addScore(Math.abs(literal),-1);}}
        else {
            if(trueLiterals == quantifier-1) {
                for(int literal : wClause.literals) {  // flipping a false literal makes the clause true
                    if(!isLocallyTrue(literal)) predicateQueue.addScore(Math.abs(literal),1);}
                return;}
            if(trueLiterals == quantifier+1) {
                for(int literal : wClause.literals) {  // flipping a true literal makes the clause true
                    if(isLocallyTrue(literal)) predicateQueue.addScore(Math.abs(literal),1);}}}}

    /** adds the initial flip score to the literals in wClause with double literals.
     * Flipping the truth value of a literal which occurs 2 or more times in a clause
     * changes the counting of true literals by its multiplicity.
     * This must be taken into account.
     *
     * @param wClause an atleast-clause to be tested.
     */
    private void setInitialFlipScoreEXACTLYwithDoubles(WClause wClause) {
        if(wClause.isLocallyTrue) { // exactly quantifier may literals are true
            for(int literal : wClause.literals) {   // flipping any literal makes the clause false
                predicateQueue.addScore(Math.abs(literal),-1);}
            return;}

        trueLiterals.clear();
        int trueLits = 0;
        for(int literal : wClause.literals) { // first we collect the true literals
            if(isLocallyTrue(literal)) {
                ++trueLits; // total number of true literals, with multiplicities
                if(!trueLiterals.contains(literal)) trueLiterals.add(literal);}} // true literals without multiplicities

        int quantifier = wClause.quantifier;
        if(trueLits > quantifier) {
            for(int literal : wClause.literals) {
                if(isLocallyTrue(literal) && (trueLits - wClause.multiplicities.get(literal) == quantifier))
                    predicateQueue.addScore(Math.abs(literal),1);}
            return;}
        int falseLits = wClause.literals.length - trueLits;
        for(int literal : wClause.literals) {
            if(!isLocallyTrue(literal) && (falseLits + wClause.multiplicities.get(literal) == quantifier))
                    predicateQueue.addScore(Math.abs(literal),1);}}

    private void updateFlipScores(int predicate) {
        predicateQueue.setScore(predicate,0);
        for(WClause wClause : posOccurrences[predicate]) {
            if(wClause.isGloballyTrue) continue;
            switch(wClause.clauseType) {
                case OR:      updateFlipScoresOr(predicate, wClause); break;
                case EXACTLY: updateFlipScoresEXACTLY(predicate,wClause); break;}
        }
        for(WClause wClause : negOccurrences[-predicate]) {
            if(wClause.isGloballyTrue) continue;
            switch(wClause.clauseType) {
                case OR:      updateFlipScoresOr(-predicate, wClause); break;
                case EXACTLY: updateFlipScoresEXACTLY(predicate,wClause); break;}
        }


    }

    /** updates the flip score for an OR-clause which contains the flipped literal.
     *
     * @param flippedLiteral the flipped literal
     * @param wClause an OR-clause containing the flipped literal
     */
    private void updateFlipScoresOr(int flippedLiteral, WClause wClause) {
        if(wClause.isLocallyTrue) {
            int trueLiterals = 0;
            int trueLiteral = 0;
            for(int literal : wClause.literals) {
                if(wClause.isLocallyTrue) {
                    if(literal != flippedLiteral) trueLiteral = literal;
                    ++trueLiterals;}}

            switch(trueLiterals) {
                case 0: // The flippedLiteral was true and is false now
                    ++falseClauses;
                    wClause.isLocallyTrue = false;
                    for (int literal : wClause.literals) // flipping any of the literals makes the clause true now
                        predicateQueue.addScore(Math.abs(literal), +1);
                    return;
                case 1: // Some other literal must be true, otherwise wClause.isLocallyTrue = false
                    // The flippedLiteral was true and is false now
                    // Originally there were two or more true literals.
                    // The contribution of all literals to the flip score was 0.
                    // Flipping the only true literal can now make the clause false
                    predicateQueue.addScore(Math.abs(trueLiteral), -1);
                    return;
                case 2: // 2 or more literals are true now
                    if (isLocallyTrue(flippedLiteral)) {
                        // The flipped literal was false and is true now.
                        // Before there was exactly one true literal
                        // Flipping this one has made the clause false, which is now no longer true
                        predicateQueue.addScore(Math.abs(trueLiteral), 1);}
                    // If the flipped literal was true and is false now, there were originally 3 true literals.
                    // Flipping had no effect, and has now no effect.
                    }
                    // If there are more than 2 true literals, flipping had no effect and has now no effect.
            }
        else { // the clause was false, and by flipping the flippedLiteral, it became true
            wClause.isLocallyTrue = true;
            --falseClauses;
            for(int literal : wClause.literals) // Flipping the flippedLiteral again makes the clause false
                predicateQueue.addScore(Math.abs(literal),-1);}
                    // Flipping the other predicates so far made the clause true, which is no longer valid.
        }

    /** updates the flip score for an Exactly-clause which contains the flipped literal.
     *
     * @param flippedLiteral the flipped literal
     * @param wClause an Exactly-clause containing the flipped literal
     */
    private void updateFlipScoresEXACTLY(int flippedLiteral, WClause wClause) {
        int quantifier = wClause.quantifier;
        int trueLiterals = 0;
        for(int literal : wClause.literals) {if(isLocallyTrue(literal)) ++trueLiterals;}

        if(wClause.isLocallyTrue) { // the clause had exactly quantifier many true literals.
            // since the flipped literal is flipped, there are either less of more than quantifier many literals
            wClause.isLocallyTrue = false;
            ++falseClauses;
            if(trueLiterals < quantifier) {// the flipped literal was true and is false now
                for(int literal : wClause.literals) // Flipping any of the literals makes the clause true
                    predicateQueue.addScore(Math.abs(literal),+1);
                return;}
            // now there is trueLiterals = quantifier + 1
            // The flipped literal was false and is true now
            // Making any of the true literals false makes the clause true again
            for(int literal : wClause.literals) {
                if(isLocallyTrue(literal)) predicateQueue.addScore(Math.abs(literal),+1);}
            return;}

        // The clause was false before the flipping
        if(trueLiterals == quantifier) { // now the clause became true
            wClause.isLocallyTrue = true;
            --falseClauses;
            if(isLocallyTrue(flippedLiteral)) { // the flipped literal was false and became true
                // There were quantifier -1 true literals.
                // Flipping a false literal made the clause true. Score +1 must be undone.
                // Flipping any of the literals makes the clause false.
                for(int literal : wClause.literals) {
                    if(isLocallyTrue(literal)) predicateQueue.addScore(Math.abs(literal),-1);
                    else predicateQueue.addScore(Math.abs(literal),-2);}
                return;}
            // The flipped literal was true and became false.
            // There were quantifier +1 true literals.
            // Flipping a true literal made the clause true. Score +1 must be undone.
            // Flipping any of the literals makes the clause false.
            for(int literal : wClause.literals) {
                if(isLocallyTrue(literal)) predicateQueue.addScore(Math.abs(literal),-2);
                else predicateQueue.addScore(Math.abs(literal),-1);}
            return;}
            // The clause was false and still is false.
            // We must undo scores which could make clauses true.
            // This was the case when trueLiterals = quantifier - 1 or trueLiterals = quantifier + 1.
            // In this case we have now trueLiterals = quantifier - 2 or trueLiterals = quantifier + 2.
        if((trueLiterals == quantifier - 2 && !isLocallyTrue(flippedLiteral)) ||
            // flippedLiteral was true and therefore it was trueLiterals == quantifier - 1
           (trueLiterals == quantifier + 2 && isLocallyTrue(flippedLiteral))) {
            // flippedLiteral was false and therefore it was trueLiterals == quantifier + 1
            for(int literal : wClause.literals) {
                if(literal != flippedLiteral) predicateQueue.addScore(Math.abs(literal),-1);}
        }}

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
