package Utilities;

import org.junit.Test;

import java.util.Random;
import java.util.function.BiConsumer;
import java.util.function.Function;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

/**
 * Created by ohlbach on 08.06.2019.
 */

public class BucketSortedTest {
    class Item {
        public int size; public int position;
        public Item(int size) {this.size = size;}
        public String toString() {return ""+size+"@"+position;}
    }

    Function<Item,Integer> getSize       = ((Item item)->item.size);
    Function<Item,Integer> getPosition   = ((Item item)->item.position);
    BiConsumer<Item,Integer> setPosition = ((Item item, Integer position)->item.position = position);


    @Test
    public void add() throws Exception {
        System.out.println("add");
        Utilities.BucketSortedList bck = new Utilities.BucketSortedList(getSize,getPosition,setPosition);
        Item i1 = new Item(3);
        bck.add(i1);
        Item i2 = new Item(3);
        bck.add(i2);
        Item i3 = new Item(0);
        bck.add(i3);
        System.out.printf(bck.toString());
        assertEquals(1,i2.position);

    }

    @Test
    public void remove() throws Exception {
        System.out.println("remove");
        Utilities.BucketSortedList bck = new Utilities.BucketSortedList(getSize,getPosition,setPosition);
        Item i1 = new Item(3);
        bck.add(i1);
        Item i2 = new Item(3);
        bck.add(i2);
        Item i3 = new Item(0);
        bck.add(i3);
        System.out.printf(bck.toString());
        bck.remove(i1);
        assertEquals(0,i2.position);
        System.out.printf(bck.toString());
        bck.remove(i2);
        System.out.printf(bck.toString());
        bck.remove(i3);
        System.out.printf(bck.toString());


    }

    @Test
    public void getRandom() throws Exception {
        System.out.println("getRandom");
        Utilities.BucketSortedList<Item> bck = new Utilities.BucketSortedList(getSize,getPosition,setPosition);
        Random rnd = new Random(2);
        assertNull(bck.getRandom(rnd));
        Item i1 = new Item(3);
        bck.add(i1);
        Item i2 = new Item(3);
        bck.add(i2);
        Item i3 = new Item(10);
        bck.add(i3);
        Item i4 = new Item(6);
        bck.add(i4);
        Item i5 = new Item(20);
        bck.add(i5);
        for(int i = 0; i < 5000; ++i) {
            Item it = bck.getRandom(rnd);
            if(it.size == 20) {
                System.out.println(it);}}
    }

}