package Utilities;


import it.unimi.dsi.fastutil.ints.IntArrays;

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

    public Random random = new Random(0);

    /** generates a new (empty) priority queue of int-values
     *
     * @param size e.g. the number of predicates in the walker-solver
     */
    public IntegerQueue(int size) {
        this.size = size;
        ++size;
        scores    = new int[size];
        queue     = new int[size];
        selected  = new int[size];
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

    public int topScore1() {
        if(!sorted) sort();
        int item = topScoreBasic();
        if(item == oldItem || item == oldoldItem) {
            for (int j : queue) {
                item = j;
                if (j != oldItem && j != oldoldItem) break;
            }
        }
        //System.out.println("IT " + item + " " + oldItem + " " + oldoldItem);
        oldoldItem = oldItem;
        oldItem = item;
        return item;}

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
        for (int j : queue) {
            item = j;
            if (item == oldItem || item == oldoldItem) {continue;}
            break;}
        oldoldItem = oldItem;
        oldItem = item;
        return item;}

    int topScoreCounter = 0;
    /** returns the item with the largest score
     *
     * @return the item with the largest score.
     */
    public int topScoreBasic() {
        int predicate0 = queue[0];
        int score0 = scores[predicate0];
        int predicate1 = queue[++topScoreCounter];
        int score1 = scores[predicate1];
        if(score1 == score0) {return predicate1;}
        topScoreCounter = 0;
        return predicate0;}

    /** sorts the queue, such that the item with the largest score comes first. */
    public void sort() {
        for(int i = 0; i <= size; ++i) {queue[i] = i;}
        IntArrays.quickSort(queue,((i,j)-> {return Integer.compare(scores[j], scores[i]);}));
        sorted = true;}

    public int oldItem = 0;
    public int oldoldItem = 0;
    int[] selected;

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
                if(flips == -threshold) {selected[item] = 1; return item;}
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
        int item = 0;
        if(limit == 0) {item = queue[0];}
        else {
            for(int i = 0; i < exponent; ++i) {
                limit = random.nextInt(limit);
                if(limit == 0) {item = queue[0]; break;}}}
        if(item == 0) item = queue[limit];
        //oldoldItem = oldItem; oldItem = item;
        return item;}

    /** returns the first index of the queue, whose score is different to the top score.
     *
     * @return the first index of the queue, whose score is different to the top score.
     */
    private int topScoreLimit(int startIndex) {
        int topScore = scores[queue[startIndex]];
        int length = queue.length;
        for(int i = startIndex+1; i < length; ++i) {
            int score = scores[queue[i]];
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
