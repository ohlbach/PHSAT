package Utilities;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Random;
import java.util.function.Function;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 05.12.2019.
 */
public class BucketSortedIndexTest {

    static class Items implements Positioned {
        int position = 0;
        int literal = 0;
        int bucket = 0;
        int index;
        static int counter = 0;

        public Items(int bucket, int literal) {
            this.bucket = bucket;
            this.literal = literal;
            this.index = ++counter;}

        @Override
        public int getPosition() {
            return position;}

        @Override
        public void setPosition(int position) {
            this.position = position;}

        public String toString() {return "C"+index + "L"+literal+"B"+bucket + "P"+position;}
    }

    Function<Items,Integer> getBucketIndex = (item -> item.bucket);
    Function<Items,Integer> getItemIndex   = (item-> item.literal);


    @Test
    public void singleItem() throws Exception {
        System.out.println("single item");
        Items.counter = 0;
        BucketSortedIndex<Items> bIndex = new BucketSortedIndex(5,getItemIndex,getBucketIndex);
        Items i1 = new Items(3,2);
        bIndex.add(i1);
        assertEquals(1,bIndex.size(2));
        assertEquals(0,bIndex.size(-2));
        assertEquals(0,bIndex.size(5));
        assertEquals(0,bIndex.size(-5));
        assertFalse(bIndex.isEmpty(2));
        assertTrue(bIndex.isEmpty(-2));
        ArrayList list = bIndex.getAllItems(2);
        assertEquals("[C1L2B3P0]",list.toString());
        bIndex.remove(i1);
        assertTrue(bIndex.isEmpty(2));
        list = bIndex.getAllItems(2);
        assertEquals("[]",list.toString());
    }

    @Test
    public void multipleItem() throws Exception {
        System.out.println("multiple items");
        Items.counter = 0;
        BucketSortedIndex<Items> bIndex = new BucketSortedIndex(11, getItemIndex, getBucketIndex);
        Random rnd = new Random(0);
        int sign = 1;
        for(int i = 0; i < 50; ++i) {
            Items item = new Items(rnd.nextInt(20),sign*rnd.nextInt(10));
            sign *= -1;
            bIndex.add(item);
        }
        assertEquals(" 1: C45L1B1P0,C37L1B17P0,C23L1B18P0,\n" +
                "-1: C50L-1B5P0,C4L-1B11P0,\n" +
                " 2: C43L2B5P0,C25L2B12P0,C7L2B13P0,C39L2B17P0,C49L2B17P1,\n" +
                "-2: C18L-2B2P0,C14L-2B3P0,C36L-2B18P0,\n" +
                " 3: C19L3B5P0,C41L3B7P0,C15L3B12P0,C3L3B15P0,\n" +
                "-3: C44L-3B9P0,C48L-3B16P0,\n" +
                " 4: C47L4B8P0,C5L4B19P0,\n" +
                "-4: C8L-4B15P0,\n" +
                " 5: C21L5B0P0,C9L5B4P0,\n" +
                "-5: C16L-5B5P0,C20L-5B18P0,\n" +
                " 6: C27L6B16P0,\n" +
                "-6: C28L-6B7P0,C24L-6B14P0,\n" +
                " 7: C29L7B8P0,C33L7B8P1,C17L7B17P0,\n" +
                "-7: C30L-7B3P0,C12L-7B4P0,C2L-7B9P0,C6L-7B17P0,C26L-7B19P0,\n" +
                " 8: C1L8B0P0,C11L8B3P0,C31L8B7P0,C35L8B18P0,\n" +
                "-8: C32L-8B5P0,C34L-8B11P0,\n" +
                "-9: C42L-9B17P0,\n",bIndex.toString());
    }

    @Test
    public void iterator() throws Exception {
        System.out.println("iterator");
        Items.counter = 0;
        BucketSortedIndex<Items> bIndex = new BucketSortedIndex(11, getItemIndex, getBucketIndex);
        Random rnd = new Random(0);
        int sign = 1;
        for(int i = 0; i < 50; ++i) {
            Items item = new Items(rnd.nextInt(20),sign*rnd.nextInt(10));
            sign *= -1;
            bIndex.add(item);
        }
        Iterator it = bIndex.iterator(2);
        String st = "";
        while(it.hasNext()) {st += it.next().toString()+",";}
        assertEquals("C43L2B5P0,C25L2B12P0,C7L2B13P0,C39L2B17P0,C49L2B17P1,",st);it = bIndex.iterator(2);
        it = bIndex.iterator(-2);
        st = "";
        while(it.hasNext()) {st += it.next().toString()+",";}
        assertEquals("C18L-2B2P0,C14L-2B3P0,C36L-2B18P0,",st);

        it = bIndex.iteratorFrom(2,10);
        st = "";
        while(it.hasNext()) {st += it.next().toString()+",";}
        assertEquals("C25L2B12P0,C7L2B13P0,C39L2B17P0,C49L2B17P1,",st);

        it = bIndex.iteratorTo(2,10);
        st = "";
        while(it.hasNext()) {st += it.next().toString()+",";}
        assertEquals("C43L2B5P0,",st);
    }

}