package Utilities;

import org.junit.Test;

import java.util.Arrays;
import java.util.Iterator;
import java.util.Random;
import java.util.function.Function;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 03.12.2019.
 */
public class BucketSortedListTest {
    static class Items implements Positioned {
        int position = 0;
        int bucket = 0;
        int index;
        static int counter = 0;

        public Items(int bucket) {
            this.bucket = bucket;
            this.index = ++counter;}

        @Override
        public int getPosition() {
            return position;}

        @Override
        public void setPosition(int position) {
            this.position = position;}

        public String toString() {return "C"+index + "B"+bucket + "P"+position;}
    }

    public Function<Items,Integer> getter = (item -> item.bucket);

    @Test
    public void singleEntry() throws Exception {
        System.out.println("single entry in one bucket");
        Items.counter = 0;
        BucketSortedList<Items> list = new BucketSortedList<BucketSortedListTest.Items>(getter);
        assertTrue(list.isEmpty());
        Items i1 = new Items(3);
        list.add(i1);
        assertTrue(list.contains(i1));
        assertFalse(list.isEmpty());
        assertEquals(1,list.size());
        assertEquals(1,list.size(3));
        assertEquals(0,list.size(2));
        assertEquals(0,list.size(4));
        assertEquals("C1B3P0",i1.toString());
        assertEquals("[C1B3P0]",list.getAllItems().toString());

        list.remove(i1);
        assertFalse(list.contains(i1));
        assertTrue(list.isEmpty());
        assertEquals(0,list.size());
        assertEquals("[]",list.getAllItems().toString());
    }

    @Test
    public void doubleEntry() throws Exception {
        System.out.println("double entry in one bucket");
        Items.counter = 0;
        BucketSortedList<Items> list = new BucketSortedList<BucketSortedListTest.Items>(getter);
        assertTrue(list.isEmpty());
        Items i1 = new Items(3);
        Items i2 = new Items(3);
        list.add(i1); list.add(i2);
        assertTrue(list.contains(i1));
        assertTrue(list.contains(i2));
        assertFalse(list.isEmpty());
        assertEquals(2,list.size());
        assertEquals(2,list.size(3));
        assertEquals(0,list.size(2));
        assertEquals(0,list.size(4));
        assertEquals("[C1B3P0, C2B3P1]",list.getAllItems().toString());

        list.remove(i1);
        assertFalse(list.contains(i1));
        assertTrue(list.contains(i2));
        assertFalse(list.isEmpty());
        assertEquals(1,list.size());
        assertEquals("[C2B3P0]",list.getAllItems().toString());
        list.remove(i2);
        assertFalse(list.contains(i2));
        assertTrue(list.isEmpty());
        assertEquals(0,list.size());
        assertEquals("[]",list.getAllItems().toString());
    }
    @Test
    public void tripleEntry() throws Exception {
        System.out.println("triple entry in one bucket");
        Items.counter = 0;
        BucketSortedList<Items> list = new BucketSortedList<BucketSortedListTest.Items>(getter);
        assertTrue(list.isEmpty());
        Items i1 = new Items(3);
        Items i2 = new Items(3);
        Items i3 = new Items(3);
        list.add(i1); list.add(i2);list.add(i3);
        assertTrue(list.contains(i1));
        assertTrue(list.contains(i3));
        assertFalse(list.isEmpty());
        assertEquals(3,list.size());
        assertEquals(3,list.size(3));
        assertEquals(0,list.size(2));
        assertEquals(0,list.size(4));
        assertEquals("[C1B3P0, C2B3P1, C3B3P2]",list.getAllItems().toString());

        list.remove(i1);
        assertFalse(list.contains(i1));
        assertTrue(list.contains(i2));
        assertTrue(list.contains(i3));
        assertFalse(list.isEmpty());
        assertEquals(2,list.size());
        assertEquals("[C3B3P0, C2B3P1]",list.getAllItems().toString());
        list.clearBucket(3);
        assertFalse(list.contains(i2));
        assertTrue(list.isEmpty());
        assertEquals(0,list.size());
        assertEquals("[]",list.getAllItems().toString());
    }

    @Test
    public void tripleEntryDist() throws Exception {
        System.out.println("triple entry in two buckets");
        Items.counter = 0;
        BucketSortedList<Items> list = new BucketSortedList<BucketSortedListTest.Items>(getter);
        assertTrue(list.isEmpty());
        Items i1 = new Items(3);
        Items i2 = new Items(3);
        Items i3 = new Items(0);
        list.add(i1); list.add(i2);list.add(i3);
        assertTrue(list.contains(i1));
        assertTrue(list.contains(i2));
        assertTrue(list.contains(i3));
        assertFalse(list.isEmpty());
        assertEquals(3,list.size());
        assertEquals(1,list.size(0));
        assertEquals(0,list.size(1));
        assertEquals(2,list.size(3));
        assertEquals("[C3B0P0, C1B3P0, C2B3P1]",list.getAllItems().toString());

        list.remove(i2);
        assertFalse(list.contains(i2));
        assertTrue(list.contains(i1));
        assertTrue(list.contains(i3));
        assertFalse(list.isEmpty());
        assertEquals(2,list.size());
        assertEquals("[C3B0P0, C1B3P0]",list.getAllItems().toString());

        list.remove(i2);
        assertEquals("[C3B0P0, C1B3P0]",list.getAllItems().toString());
        list.remove(i3);
        assertEquals("[C1B3P0]",list.getAllItems().toString());
        list.remove(i1);
        assertEquals("[]",list.getAllItems().toString());

    }


    @Test
    public void getRandom() throws Exception {
        System.out.println("getRandom");
        Items.counter = 0;
        Random rnd = new Random(0);
        BucketSortedList<Items> list = new BucketSortedList<BucketSortedListTest.Items>(getter);
        for(int i = 0; i < 50; ++i) {
            for(int j = 0; j < 10; ++j) {
                list.add(new Items(j));}}
        int[] stat = new int[10];
        for(int i = 0; i < 200; ++i) {
            ++stat[list.getItem(list.getRandomIndex(rnd)).bucket];}
        assertEquals("[69, 32, 29, 19, 22, 15, 5, 5, 2, 2]",Arrays.toString(stat));
         // should be distributed quadratically decreasing
    }



    @Test
    public void iterator() throws Exception {
        System.out.println("iterator");
        Items.counter = 0;
        BucketSortedList<Items> list = new BucketSortedList<BucketSortedListTest.Items>(getter);
        for(int i = 0; i < 5; ++i) {
            for(int j = 0; j < 2; ++j) {
                list.add(new Items(2*j));}}
        String st = "";
        for(Items i : list) {st += " "+i.toString();}
        assertEquals(" C1B0P0 C3B0P1 C5B0P2 C7B0P3 C9B0P4 C2B2P0 C4B2P1 C6B2P2 C8B2P3 C10B2P4",st);

        st = "";
        Iterator<Items> it = list.iteratorFrom(1);
        while(it.hasNext()) {st += " "+it.next().toString();}
        assertEquals(" C2B2P0 C4B2P1 C6B2P2 C8B2P3 C10B2P4",st);

        st = "";
        it = list.iteratorTo(1);
        while(it.hasNext()) {st += " "+it.next().toString();}
        assertEquals(" C1B0P0 C3B0P1 C5B0P2 C7B0P3 C9B0P4",st);

    }

}