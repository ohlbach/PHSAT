package Solver.RandomWalk;

import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Model;

import java.util.HashSet;
import java.util.PriorityQueue;

/**
 * Created by ohlbach on 01.09.2018.
 */
public class RandomWalker {
    private ClauseList clauseList;
    private int seed;
    public String info;
    private LiteralIndex index ;
    private Model model;
    private int predicates;
    private int[] flipConsequences;
    private PriorityQueue<Integer> literalQueue;
    private HashSet<Integer> affected = new HashSet<>();

    public RandomWalker(ClauseList clauseList, int seed) {
        this.clauseList = clauseList;
        index = clauseList.literalIndex;
        model = clauseList.model;
        predicates = model.predicates;
        flipConsequences = new int[predicates+1];
        literalQueue = new PriorityQueue<Integer>(predicates,(
                (l1,l2) -> {
                    int f1 = flipConsequences[l1];
                    int f2 = flipConsequences[l2];
                    if(f1 > f2) {return -1;}
                    return(f1 < f2) ? 1 : 0;}));
        this.seed = seed;
        info = "Random Walker with seed " + seed;
    }

    /** generates a candidate model for the clauses.
     * A predicate becomes true if it occurs in more clauses than its negation.
     */
    private void initializeModel() {
        for(int predicate = 1; predicate <= predicates; ++predicate) {
                model.push((index.getLiterals(predicate).size() > index.getLiterals(-predicate).size()) ? predicate : -predicate);}}

    /** initializes flipConsequences and literalQueue.
     *  literalQueue contains the predicates ordered by the number
     *  of clauses made true when flipping the predicate.
     *  The head of the queue is the prediate which makes most clauses true by flipping it.
     */
    private void initializeFlipConsequences() {
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            flipConsequences[predicate] = flipMakesTrue(predicate);}
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            literalQueue.add(predicate);}}


    /** computes how many clauses more become true after flipping the predicate.
     * Example: p is true. </br>
     * All clauses with -p where all literals are false become true</br>
     * All clauses with p where all other literals are false become false</br>
     * The result is then the difference between these numbers.
     *
     * @param predicate the predicate to be checked
     * @return how many clauses more become true after flipping the predicate.
     */
    private int flipMakesTrue(int predicate) {
        int becomesTrue = 0;
        for(CLiteral cliteral : index.getLiterals(predicate*model.status(predicate))) {
            boolean remainstrue = false;
            for(CLiteral othercliteral : cliteral.getClause().cliterals) {
                if(cliteral != othercliteral && model.isTrue(othercliteral.literal)) {remainstrue  = true; break;}}
            if(!remainstrue) {--becomesTrue;}      // clause becomes false after flip.
            }
        for(CLiteral cliteral : index.getLiterals(-predicate*model.status(predicate))) {
            boolean remainstrue = false;
            for(CLiteral othercliteral : cliteral.getClause().cliterals) {
                if(cliteral != othercliteral && model.isTrue(othercliteral.literal)) {remainstrue  = true; break;}}
            if(!remainstrue) {++becomesTrue;}      // clause becomes true after flip.
        }
        return becomesTrue;}

    /** flips the truth value of the predicate and updates the literalQueue
     *
     * @param predicate
     */
    private void flip(int predicate) {
        affected.clear();
        int trueLiteral = 0;
        for(CLiteral cliteral : index.getLiterals(predicate*model.status(predicate))) {
            trueLiteral = findTrueLiteral(cliteral);}
        if(trueLiteral < 0) { // all are false
            for(CLiteral cliteral : index.getLiterals(predicate*model.status(predicate))) {
                int pred = Math.abs(cliteral.literal);
                ++flipConsequences[pred];
                affected.add(pred);}}
        else {
            if(trueLiteral != 0) {
                int pred = Math.abs(trueLiteral);
                --flipConsequences[pred];
                affected.add(pred);}}

        for(CLiteral cliteral : index.getLiterals(-predicate*model.status(predicate))) {
            trueLiteral = findTrueLiteral(cliteral);
            if(trueLiteral < 0) {
                --flipConsequences[predicate];
                affected.add(predicate);}
            else {
            if(trueLiteral > 0) {
                int pred = Math.abs(trueLiteral);
                ++flipConsequences[pred];
                affected.add(pred);}}}

        for(Integer pred : affected) {
            literalQueue.remove(pred);
            literalQueue.add(pred);}
        model.flip(predicate);}

    /** checks if all literals in the clause except the literals itself are false or at most one is true
     *
     * @param cliteral the literal to be checked
     * @return -1 if all other literals are false, 0 if there is more than one true literal, otherwise the true literal.
     */
    private int findTrueLiteral(CLiteral cliteral) {
        boolean allFalse = true;
        int trueLiteral = 0;
        for(CLiteral otherliteral : cliteral.getClause().cliterals) {
            if(otherliteral != cliteral && model.isTrue(otherliteral.literal)) {
                allFalse = false;
                if(trueLiteral != 0) {trueLiteral = 0;}
                else {trueLiteral = otherliteral.literal;}}}
        return allFalse ? -1 : trueLiteral;}


}
