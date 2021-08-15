package Utilities;

import it.unimi.dsi.fastutil.ints.IntArrays;
import java.util.Random;


/** This class provides an alternative to PriorityQueue.
 * It keeps an int-queue, sorted by int-values which are defined separately.
 * An example is: the flips-scores for predicates in the walker solver.
 * The higher the score for a predicate, the more clauses are made true by flipping the truth value of the predicate.
 * The differences to PriorityQueue are: <br>
 *     - the queue is entirely sorted. Therefore one can access all elements according to the sorting. <br>
 *     - changing the score causes a reordering of the queue, whose complexity depends on the number of moves in the queue only. <br>
 *
 *  For initializing the queue one has to define all scores first, and then sort the queue. <br>
 *  Afterwards one can change the score arbitrarily, and the queue is kept sorted.
 *
 * Created by ohlbach on 17.01.2020.
 */
public class IntegerQueue {
    /** the number of items in the queue, eg. the number or predicates in the SAT-application*/
    private int size = 0;
    /** the score for each item */
    private int[] scores = null;
    /** the priority queue */
    private int[] queue = null;
    /** keeps the actual positions of the items in the queue */
    private int[] positions = null;
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
        positions = new int[size];}

    /** sets the score for the given item
     *
     * @param item   typically a predicate
     * @param score  its score, e.g. the number of clauses made true when the predicate is flipped
     */
    public void setScore(int item, int score) {
        assert item >= 0 && item <= size && !sorted;
        scores[item] = score;}

    /** adds the score for the given item
     *
     * @param item   typically a predicate
     * @param score  its score, e.g. the number of clauses made true when the predicate is flipped
     */
    public void addScore(int item, int score) {
        assert item >= 0 && item <= size && !sorted;
        scores[item] += score;}

    /** gets the score for the given item
     *
     * @param item  e.g. a predicate
     * @return its score
     */
    public int getScore(int item) {
        assert item >= 0 && item <= size;
        return scores[item];}

    /** returns the item with the larges score
     *
     * @return the item with the largest score.
     */
    public int topScore() {
        assert sorted;
        return queue[0];}

    /** returns the item with the nth largest score
     *
     * @param n an index
     * @return the item with the nth largest score
     */
    public int nthTopScore(int n) {
        assert n >= 0 && n <= size && sorted;
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
        for(int i = 0; i <= size; ++i) {positions[queue[i]] = i;}
        sorted = true;}

    /** changes the item's score and reorders the queue
     *
     * @param item     an item
     * @param newscore its new score
     */
    public void changeScore(int item, int newscore) {
        assert item >= 0 && item <= size && sorted;
        int position = positions[item];
        int oldscore = scores[item];
        if(oldscore == newscore) {return;}
        scores[item] = newscore;
        if(newscore > oldscore) {
            for(int pos = position - 1; pos >= 0; --pos) {
                if(scores[queue[pos]] < newscore) {
                    int dummy = queue[pos+1];
                    queue[pos+1] = queue[pos];
                    queue[pos] = dummy;
                    positions[queue[pos]] = pos;
                    positions[queue[pos+1]] = pos+1;}
                else{break;}}}
        else {

            for(int pos = position + 1; pos <= size; ++pos) {
                if(scores[queue[pos]] > newscore) {
                    int dummy = queue[pos-1];
                    queue[pos-1] = queue[pos];
                    queue[pos] = dummy;
                    positions[queue[pos]] = pos;
                    positions[queue[pos-1]] = pos-1;}
                else{break;}}}
    }

    /** gets an item in the queue which is defined by a random generator.
     * If exponent = 1, the chance is equal for every item<br>
     * If exponent = 2, the chance for items with higher score is increased quadratically. <br>
     * If exponent = 3, the chance for items with higher score is increased qubically, etc. <br>
     *
     * @param random   a random number generator
     * @param exponent for influencing the chances for items with higher score.
     * @return an item, according to the random process.
     */
    public int getRandom(Random random, int exponent) {
        int limit = lastPositive();
        if(limit == 0) {return 0;}
        for(int i = 0; i < exponent; ++i) {
            limit = random.nextInt(limit);
            if(limit == 0) {return queue[0];}}
        return queue[limit];}

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
        StringBuilder st = new StringBuilder();
        st.append("item score, queue, position\n");
        for(int i = 0; i <= size; ++i) {
            st.append(Integer.toString(i)).append(":     ").append(Integer.toString(scores[i])).append(",     ");
            st.append(Integer.toString(queue[i])). append(",     ").append(positions[i]).append("\n");
        }
        return st.toString();
    }
}
