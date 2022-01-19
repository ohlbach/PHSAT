package Utilities;


import it.unimi.dsi.fastutil.ints.IntArrays;

import javax.security.auth.callback.TextOutputCallback;
import java.util.Arrays;
import java.util.Random;

/** This class provides an alternative to PriorityQueue.
 * It keeps an int-queue, sorted by int-values which are defined separately.
 * An example is: the flip-scores for predicates in the walker solver.
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
    public final int[] scores;

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
        for(int i = 0; i < size; ++i) {queue[i] = i;}
        scores[0] = Short.MIN_VALUE;
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

    /** returns the top item of the sorted queue.
     *
     * @return the top item of the sorted queue
     */
    public int topItem() {
        if(!sorted) sort();
        return queue[0];}

    /** sorts the queue, such that the item with the largest score comes first. */
    public void sort() {
        IntArrays.quickSort(queue,((i,j)-> {return Integer.compare(scores[j], scores[i]);}));
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
    public int getRandom(Random random, int exponent, int jumpDistance) {
        if(!sorted) sort();
        int limit = size*jumpDistance/100;
        int item = 0;
        if(limit == 0) {item = queue[0];}
        else {
            for(int i = 0; i < exponent; ++i) {
                limit = random.nextInt(limit);
                if(limit == 0) {item = queue[0]; break;}}}
        if(item == 0) item = queue[limit];
        //oldoldItem = oldItem; oldItem = item;
        return item;}

    /** turns the data into a string
     *
     * @return a string representation of the data.
     */
    public String toString() {
        if(!sorted) sort();
        int length = Integer.toString(size).length();
        for(int i = 1; i < scores.length; ++i) length = Math.max(length,Integer.toString(scores[i]).length());
        StringBuilder st = new StringBuilder();
        st.append("Integer Queue:  item: score\n");
        int blocks = 10;
        int i = 0;
        while(i < size) {
            String items = ""; String scos = "";
            for(int j = i; j < i + blocks; ++j) {
                if(j == size) break;
                items += String.format(" %"+length+"d",queue[j]);
                scos += String.format(" %"+length+"d",scores[queue[j]]);}
            st.append(items).append("\n").append(scos).append("\n\n");
            i += blocks;}
        return st.toString();
    }
}
