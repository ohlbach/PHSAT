package Utilities;

import org.junit.Test;

import java.util.function.Function;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 03.12.2019.
 */
public class BucketSortedListTest {
    class Items implements Positioned {
        int position = 0;
        public Items(int position) {this.position = position;}

        @Override
        public int getPosition() {
            return position;}

        @Override
        public void setPosition(int position) {
            this.position = position;}

        public String toString() {return "P"+position;}
    }

    public Function<Items,Integer> getter = (item -> item.position);

    @Test
    public void add() throws Exception {
        BucketSortedList<Items> list = new BucketSortedList<BucketSortedListTest.Items>(getter);
        list.add(new Items(3));
        System.out.println(list);
    }

    @Test
    public void contains() throws Exception {

    }

    @Test
    public void remove() throws Exception {

    }

    @Test
    public void getRandom() throws Exception {

    }

    @Test
    public void getAllItems() throws Exception {

    }

    @Test
    public void isEmpty() throws Exception {

    }

    @Test
    public void size() throws Exception {

    }

    @Test
    public void size1() throws Exception {

    }

    @Test
    public void clearBucket() throws Exception {

    }

    @Test
    public void iterator() throws Exception {

    }

    @Test
    public void iteratorFrom() throws Exception {

    }

    @Test
    public void iteratorTo() throws Exception {

    }

}