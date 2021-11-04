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
        System.out.println(q.toString());

    }


    @Test
    public void getRandom() throws Exception {
        System.out.println("getRandom");
        IntegerQueue q = new IntegerQueue(100);
        for(int i = 0; i <= 100; ++i ) {q.setScore(i,90-i);}
        q.sort();
        //System.out.println(q.toString());
        int[] results = new int[101];
        Random rnd = new Random(0);
        for(int i = 0; i < 1000; ++i) {++results[q.getRandom(rnd,1)];}
        assertEquals("[7, 12, 11, 11, 10, 12, 14, 7, 9, 8, 10, 14, 9, 15, 8, 9, 11, 9, 10, 8, 9, 10, 15, 14, 17, 10, 12, 12, 11, 6, 6, 16, 11, 12, 13, 11, 6, 15, 9, 9, 15, 12, 8, 17, 7, 17, 11, 11, 5, 17, 11, 9, 14, 7, 10, 15, 13, 15, 6, 12, 13, 7, 8, 14, 17, 11, 19, 12, 10, 9, 14, 6, 11, 6, 9, 10, 18, 14, 16, 10, 14, 7, 14, 13, 14, 10, 13, 12, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]",Arrays.toString(results));
        results = new int[101];
        for(int i = 0; i < 1000; ++i) {++results[q.getRandom(rnd,2)];}
        assertEquals("[76, 39, 39, 32, 35, 26, 34, 31, 32, 20, 22, 27, 25, 14, 20, 28, 14, 9, 19, 12, 12, 14, 21, 16, 14, 10, 22, 17, 5, 16, 13, 9, 11, 5, 8, 11, 14, 13, 9, 8, 8, 13, 5, 7, 10, 9, 6, 3, 12, 5, 9, 13, 7, 4, 2, 2, 2, 7, 5, 7, 3, 1, 5, 6, 4, 4, 5, 4, 2, 4, 4, 2, 1, 1, 3, 1, 2, 4, 1, 0, 3, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]" ,Arrays.toString(results));
        results = new int[101];
        for(int i = 0; i < 1000; ++i) {++results[q.getRandom(rnd,3)];}
        assertEquals("[216, 83, 60, 60, 46, 41, 35, 27, 28, 33, 25, 19, 18, 16, 24, 20, 21, 18, 14, 11, 9, 12, 12, 19, 9, 5, 4, 8, 4, 6, 10, 8, 5, 5, 5, 3, 3, 2, 4, 1, 2, 4, 5, 5, 2, 1, 1, 1, 4, 2, 2, 1, 1, 3, 4, 1, 2, 3, 1, 0, 2, 0, 0, 0, 0, 0, 2, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]",Arrays.toString(results));

        //System.out.println(Arrays.toString(results));
    }
}