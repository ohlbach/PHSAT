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
    private final int[] scores;

    /** the priority queue, i.e. queue[0] is the item with the top score */
    private final int[] queue;

    /** indicates that the queue is sorted */
    private boolean sorted = false;

    /** generates a new (empty) priority queue of int-values
     *
     * @param size e.g. the number of predicates in the walker-solver
     */
    public IntegerQueue(int size) {
        this.size = size;
        ++size;
        scores    = new int[size];
        queue     = new int[size];
    }

    /** sets the score for the given item
     *
     * @param item   typically a predicate
     * @param score  its score, e.g. the number of clauses made true when the predicate is flipped
     */
    public void setScore(int item, int score) {
        scores[item] = score;
        sorted = false;}

    /** adds the score for the given item
     *
     * @param item   typically a predicate
     * @param score  its score, e.g. the number of clauses made true when the predicate is flipped
     */
    public void addScore(int item, int score) {
        scores[item] += score;
        sorted = false;}

    /** gets the score for the given item
     *
     * @param item  e.g. a predicate
     * @return its score
     */
    public int getScore(int item) {
        return scores[item];}

    /** returns the item with the larges score
     *
     * @return the item with the largest score.
     */
    public int topScore() {
        if(!sorted) sort();
        return queue[0];}

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
                    int sci = scores[i];
                    int scj = scores[j];
                    if(sci == scj) {return 0;}
                    if(sci > scj) {return -1;}
                    return 1;}));
        sorted = true;}


    /** gets an item with positive score in the queue which is defined by a random generator.
     * If exponent = 1, the chance is equal for every item<br>
     * If exponent = 2, the chance for items with higher score is increased quadratically. <br>
     * If exponent = 3, the chance for items with higher score is increased cubically, etc. <br>
     *
     * @param random   a random number generator
     * @param exponent for influencing the chances for items with higher score.
     * @return an item, according to the random process.
     */
    public int getRandom(Random random, int exponent) {
        if(!sorted) sort();
        int limit = lastPositive();
        if(limit == 0) {return queue[0];}
        for(int i = 0; i < exponent; ++i) {
            limit = random.nextInt(limit);
            if(limit == 0) {return queue[0];}}
        return queue[limit];}

    /** gets the last item with a positive score
     *
     * @return the last item with a positive score
     */
    public int lastPositive() {
        int last = size;
        if(scores[queue[last]] > 0) {return size;}
        int half = last/2;
        while(last-half > 1) {
            if(scores[queue[half]] > 0 ) { half += (last-half)/2;}
            else {last = half; half /=2;}}
        return half;}

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
