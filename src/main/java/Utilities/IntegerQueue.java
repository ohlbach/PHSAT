package Utilities;


import it.unimi.dsi.fastutil.ints.IntArrays;

import java.util.Random;

/** This class provides an alternative to PriorityQueue.
 * It keeps an int-queue, sorted by int-values which are defined separately.
 * An example is: the flips-scores for predicates in the walker solver.
 * The higher the score for a predicate, the more clauses are made true by flipping the truth value of the predicate.
 * The differences to PriorityQueue is: <br>
 *  - the queue is entirely sorted. Therefore, one can access all elements according to the sorting. <br>
 *  For initializing the queue one has to define all scores first, and then sort the queue. <br>
 *
 * Created by ohlbach on 17.01.2020.
 */
public class IntegerQueue {
    /** the number of items in the queue, eg. the number or predicates in the SAT-application*/
    private final int size;

    /** the score for each item */
    public final float[] scores;

    /** the priority queue, i.e. queue[0] is the item with the top score */
    private final int[] queue;

    /** indicates that the queue is sorted */
    private boolean sorted = false;

    public Random random = new Random(0);

    /** generates a new (empty) priority queue of int-values
     *
     * @param size e.g. the number of predicates in the walker-solver
     */
    public IntegerQueue(int size) {
        this.size = size;
        ++size;
        scores    = new float[size];
        queue     = new int[size];
        selected  = new short[size];
    }

    /** sets the score for the given item
     *
     * @param item   typically a predicate
     * @param score  its score, e.g. the number of clauses made true when the predicate is flipped
     */
    public void setScore(int item, float score) {
        scores[item] = score;
        sorted = false;}

    /** adds the score for the given item
     *
     * @param item   typically a predicate
     * @param score  its score, e.g. the number of clauses made true when the predicate is flipped
     */
    public void addScore(int item, float score) {
        scores[item] += score;
        sorted = false;}

    /** gets the score for the given item
     *
     * @param item  e.g. a predicate
     * @return its score
     */
    public float getScore(int item) {
        return scores[item];}

    /** returns the item with the larges score
     *
     * @return the item with the largest score.
     */
    public int topScore() {
        if(!sorted) sort();

        int scoreLimit = topScoreLimit(0);
        if(scoreLimit > 2) {
            int counter = 0;
            while(++counter < scoreLimit) {
                int item = queue[random.nextInt(scoreLimit)];
                if(item == oldItem || item == oldoldItem) {continue;}
                else {
                    oldoldItem = oldItem;
                    oldItem = item;
                    return item;}}}

        int item = 0;
        for(int i = 0; i < queue.length; ++i) {
            item = queue[i];
            if(item == oldItem || item == oldoldItem) {continue;}
            break;}
        oldoldItem = oldItem;
        oldItem = item;
        return item;}

    /** returns the item with the nth largest score
     *
     * @param n an index
     * @return the item with the nth largest score
     */
    public int nthTopScore(int n) {
        if(!sorted) sort();
        return queue[n];}

    /** sorts the queue, such that the item with the largest score comes first.
     */
    public void sort() {
        for(int i = 0; i <= size; ++i) {queue[i] = i;}
        IntArrays.quickSort(queue,
                ((i,j)-> {
                    float sci = scores[i];
                    float scj = scores[j];
                    if(sci == scj) {return 0;}
                    if(sci > scj) {return -1;}
                    return 1;}));
        sorted = true;}

    public int oldItem = 0;
    public int oldoldItem = 0;
    short[] selected;

    public int getTopItem(int threshold) {
        if(!sorted) sort();
        int startIndex = 0;
        int scoreLimit = topScoreLimit(startIndex);
        int item = 0;
        try{
        while(scoreLimit < queue.length) {
            int counter = 0;
            while(++counter <= scoreLimit-startIndex) {
                item = queue[startIndex + random.nextInt(scoreLimit-startIndex)];
                if(item == oldItem || item == oldoldItem) {continue;}
                int flips = selected[item];
                //System.out.println("S " + item + " " + counter + " " +startIndex + " " + scoreLimit + " " + flips);
                if(flips >= 0) {
                    if(flips == threshold) {selected[item] = -1; continue;}
                    else {++selected[item]; return item;}}
                if(flips == -threshold) {selected[item] = +1; return item;}
                else {--selected[item]; continue;}}
            startIndex = scoreLimit;
            scoreLimit = topScoreLimit(startIndex);}}
        finally {oldoldItem = oldItem; oldItem = item;}
        return queue[0];}

    private int selectInTopScore() {
        int counter = 0;
        int scoreLimit = topScoreLimit(0);
        while(++counter < scoreLimit) {
            int item = queue[random.nextInt(scoreLimit)];
            if(item == oldItem || item == oldoldItem) continue;
            return item;}
        return 0;}


    /** gets an item with positive score in the queue which is defined by a random generator.
     * If exponent = 1, the chance is equal for every item<br>
     * If exponent = 2, the chance for items with higher score is increased quadratically. <br>
     * If exponent = 3, the chance for items with higher score is increased cubically, etc. <br>
     *
     * @param random   a random number generator
     * @param exponent for influencing the chances for items with higher score.
     * @return an item, according to the random process.
     */
    public int getRandom(Random random, int exponent, int jumpDistance) {
        if(!sorted) sort();
        int limit = size*jumpDistance/100;
        if(limit == 0) {return queue[0];}
        for(int i = 0; i < exponent; ++i) {
            limit = random.nextInt(limit);
            if(limit == 0) {return queue[0];}}
        return queue[limit];}

    /** returns the first index of the queue, whose score is different to the top score.
     *
     * @return the first index of the queue, whose score is different to the top score.
     */
    private int topScoreLimit(int startIndex) {
        float topScore = scores[queue[startIndex]];
        int length = queue.length;
        for(int i = startIndex+1; i < length; ++i) {
            float score = scores[queue[i]];
            if(score != topScore) return i;}
        return length+1;}

    /** turns the data into a string
     *
     * @return a string representation of the data.
     */
    public String toString() {
        if(!sorted) sort();
        StringBuilder st = new StringBuilder();
        st.append("Integer Queue:  item: score\n");
        for(int i = 0; i <= size; ++i) {
            st.append(queue[i]).append(":").append(scores[queue[i]]).append(", ");
            if(i > 0 && i % 10 == 0) st.append("\n");}
        return st.toString();
    }
}
