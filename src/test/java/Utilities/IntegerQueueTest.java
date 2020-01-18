package Utilities;

import org.junit.Test;

import java.util.Arrays;
import java.util.Random;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 17.01.2020.
 */
public class IntegerQueueTest {
    @Test
    public void sort() throws Exception {
        System.out.println("sort");
        IntegerQueue q = new IntegerQueue(5);
        q.setScore(1,5);
        q.setScore(2,3);
        q.setScore(3,6);
        q.setScore(4,1);
        q.setScore(5,10);
        assertEquals(6, q.getScore(3));
        assertEquals(10, q.getScore(5));
        q.sort();
        assertEquals(5,q.topScore());
        assertEquals(5,q.nthTopScore(0));
        assertEquals(3,q.nthTopScore(1));
        //System.out.println(q.toString());
        q.changeScore(3,-1);
        assertEquals(5,q.topScore());
        assertEquals(1,q.nthTopScore(1));
        assertEquals(3,q.nthTopScore(5));
        //System.out.println(q.toString());
    }


    @Test
    public void getRandom() throws Exception {
        System.out.println("getRandom");
        IntegerQueue q = new IntegerQueue(100);
        for(int i = 0; i <= 100; ++i ) {q.setScore(i,-i);}
        q.sort();
        //System.out.println(q.toString());
        int[] results = new int[101];
        Random rnd = new Random(0);
        for(int i = 0; i < 1000; ++i) {++results[q.getRandom(rnd,1)];}
        assertEquals("[10, 8, 16, 8, 18, 10, 13, 11, 8, 15, 14, 3, 7, 7, 17, 11, 9, 13, 10, 9, 9, 10, 12, 5, 5, 12, 11, 15, 12, 10, 7, 7, 6, 4, 15, 13, 11, 11, 11, 8, 9, 9, 6, 14, 10, 8, 14, 6, 8, 11, 14, 11, 7, 8, 9, 9, 8, 5, 12, 15, 8, 9, 9, 5, 9, 14, 10, 9, 15, 9, 15, 8, 11, 10, 8, 10, 8, 15, 9, 13, 7, 5, 7, 9, 10, 8, 9, 12, 8, 11, 14, 3, 9, 9, 10, 8, 13, 12, 8, 12, 8]",Arrays.toString(results));
        results = new int[101];
        for(int i = 0; i < 1000; ++i) {++results[q.getRandom(rnd,2)];}
        assertEquals("[79, 42, 37, 26, 23, 29, 35, 22, 21, 25, 23, 28, 21, 17, 16, 15, 16, 14, 15, 16, 19, 17, 14, 23, 13, 10, 13, 17, 16, 6, 9, 5, 11, 10, 12, 15, 8, 4, 11, 8, 9, 11, 5, 3, 9, 7, 5, 6, 6, 13, 6, 5, 5, 12, 5, 8, 9, 10, 5, 5, 5, 4, 7, 7, 3, 4, 1, 5, 2, 8, 3, 4, 4, 4, 4, 4, 4, 3, 5, 3, 0, 1, 2, 2, 2, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0]" ,Arrays.toString(results));
        results = new int[101];
        for(int i = 0; i < 1000; ++i) {++results[q.getRandom(rnd,3)];}
        assertEquals("[192, 73, 62, 57, 53, 42, 32, 35, 38, 27, 20, 21, 22, 25, 16, 16, 19, 13, 13, 11, 13, 18, 10, 9, 8, 7, 8, 9, 10, 10, 7, 6, 7, 0, 6, 5, 5, 4, 4, 4, 8, 4, 2, 5, 6, 0, 3, 4, 1, 4, 1, 0, 2, 3, 2, 2, 0, 1, 3, 2, 0, 2, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]",Arrays.toString(results));

        //System.out.println(Arrays.toString(results));
    }
}