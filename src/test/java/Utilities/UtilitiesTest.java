package Utilities;

import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.function.BiConsumer;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 03.09.2018.
 */
public class UtilitiesTest {
    @Test
    public void parseInteger() throws Exception {
        System.out.println("parseInteger");
        StringBuffer errors = new StringBuffer();
        Integer n = Utilities.parseInteger("test","55",errors);
        assertEquals(55,(int)n);
        n = Utilities.parseInteger("test","55a",errors);
        assertNull(n);
        assertTrue(errors.length() > 0);
    }

    @Test
    public void parseRange1() throws Exception {
        System.out.println("parseIntRange 1: number, comma");
        StringBuffer errors = new StringBuffer();
        ArrayList<Integer> n = Utilities.parseIntRange("test","55",errors);
        assertEquals("[55]",n.toString());

        n = Utilities.parseIntRange("test","55,66,77",errors);
        assertEquals("[55, 66, 77]",n.toString());

        n = Utilities.parseIntRange("test","55,66a,77",errors);
        assertNull(n);
        assertTrue(errors.length() > 0);}

    @Test
    public void parseRange2() throws Exception {
        System.out.println("parseIntRange 2: range");
        StringBuffer errors = new StringBuffer();
        ArrayList<Integer> n = Utilities.parseIntRange("test", "55 to 60", errors);
        assertEquals("[55, 56, 57, 58, 59, 60]", n.toString());
        n = Utilities.parseIntRange("test", "-3 to -1", errors);
        assertEquals("[-3, -2, -1]", n.toString());

        n = Utilities.parseIntRange("test", "55 to 60a", errors);
        assertNull(n);
        assertTrue(errors.length() > 0);

        n = Utilities.parseIntRange("test", "3  to 10 step 2",errors);
        assertEquals("[3, 5, 7, 9]", n.toString());

        n = Utilities.parseIntRange("test", "-10  to -5 step 2",errors);
        assertEquals("[-10, -8, -6]", n.toString());


        n = Utilities.parseIntRange("test", "3  to 10 step 2a",errors);
        assertNull(n);
        assertTrue(errors.length() > 0);
    }

    @Test
    public void crossProduct1() throws Exception {
        System.out.println("crossProduct 1");
        assertNull(Utilities.crossProduct());
        ArrayList list = new ArrayList<>();
        list.add(1); list.add(2);
        ArrayList<ArrayList<Integer>> product = Utilities.crossProduct(list);
        assertEquals(1,product.size());
        assertEquals(list,product.get(0));
    }

    @Test
    public void crossProduct2() throws Exception {
        System.out.println("crossProduct 2");
        ArrayList list1 = new ArrayList<>();
        list1.add(1); list1.add(2);
        ArrayList list2 = new ArrayList<>();
        list2.add(3); list2.add(4);
        ArrayList<ArrayList> product = Utilities.crossProduct(list1,list2);
        assertEquals("[[1, 3], [1, 4], [2, 3], [2, 4]]",product.toString());

        ArrayList list3 = new ArrayList<>();
        list3.add(5); list3.add(6);
        product = Utilities.crossProduct(list1,list2,list3);
        assertEquals("[[1, 3, 5], [1, 4, 5], [2, 3, 5], [2, 4, 5], [1, 3, 6], [1, 4, 6], [2, 3, 6], [2, 4, 6]]",product.toString());

        ArrayList list4 = new ArrayList<>();
        list4.add(7); list4.add(8);
        product = Utilities.crossProduct(list1,list2,list3,list4);
        assertEquals("[[1, 3, 5, 7], [1, 4, 5, 7], [2, 3, 5, 7], [2, 4, 5, 7], [1, 3, 6, 7], [1, 4, 6, 7], [2, 3, 6, 7], [2, 4, 6, 7], [1, 3, 5, 8], [1, 4, 5, 8], [2, 3, 5, 8], [2, 4, 5, 8], [1, 3, 6, 8], [1, 4, 6, 8], [2, 3, 6, 8], [2, 4, 6, 8]]",product.toString());
    }


    @Test
    public void writeTmpFile() throws Exception {
        System.out.println("writeTmpFile 2");
        Utilities.writeTmpFile("TEST", "test.cnf", "text\ntexfgdfgt");
    }

    @Test
    public void stdoutLogger() {
        BiConsumer<String,String> logger = Utilities.stdoutLogger();
        logger.accept("ID","Test");
    }

    @Test
    public void appendArrays1() {
        System.out.println("append arrays 1");
        int[] a1 = new int[]{1,2};
        int[] a2 = new int[]{3,4,5};
        int[] a = Utilities.appendArrays(a1,a2);
        assertEquals("[1, 2, 3, 4, 5]", Arrays.toString(a));

        int[] a3 = new int[]{};
        int[] a4 = new int[]{};
        int[] b = Utilities.appendArrays(a3,a4);
        assertEquals("[]", Arrays.toString(b));

    }

    @Test
    public void appendArrays2() {
        System.out.println("append arrays 2");
        int[] a1 = new int[]{1, 2};
        int[] a2 = new int[]{3, 4, 5};
        int[] a3 = new int[]{6, 7};
        ArrayList<int[]> a = new ArrayList<>(3);
        a.add(a1); a.add(a2); a.add(a3);
        int[] b = Utilities.appendArrays(a);
        assertEquals("[1, 2, 3, 4, 5, 6, 7]", Arrays.toString(b));

        int[] a4 = new int[]{};
        a.add(a4);
        b = Utilities.appendArrays(a);
        assertEquals("[1, 2, 3, 4, 5, 6, 7]", Arrays.toString(b));

        a.clear();
        b = Utilities.appendArrays(a);
        assertEquals("[]", Arrays.toString(b));
    }
    @Test
    public void joinIntArrays() {
        System.out.println("joinIntArrays");
        assertNull(Utilities.joinIntArrays1(null,null));
        IntArrayList list1 = new IntArrayList();
        list1.add(1); list1.add(2);
        IntArrayList list2 = new IntArrayList();
        list2.add(2); list2.add(3);
        IntArrayList list = Utilities.joinIntArrays1(list1,list2);
        assertEquals("[1, 2]",list1.toString());
        assertEquals("[2, 3]",list2.toString());
        assertEquals("[1, 2, 3]",list.toString());
    }
    @Test
    public void addIntArray() {
        System.out.println("addIntArray");
        assertNull(Utilities.addIntArray(null,null));
        IntArrayList list1 = new IntArrayList();
        list1.add(1); list1.add(2);
        IntArrayList list2 = new IntArrayList();
        list2.add(2); list2.add(3);
        IntArrayList list = Utilities.addIntArray(list1,list2);
        assertEquals("[1, 2, 3]",list1.toString());
        assertEquals("[2, 3]",list2.toString());
        assertEquals("[1, 2, 3]",list.toString());
    }

    @Test
    public void addInt() {
        System.out.println("addInt");
        assertEquals("[1]",Utilities.addInt(null,1).toString());
        IntArrayList list1 = new IntArrayList();
        list1.add(1); list1.add(2);
        IntArrayList list = Utilities.addInt(list1,1);
        assertEquals("[1, 2]",list.toString());
        list = Utilities.addInt(list1,3);
        assertEquals("[1, 2, 3]",list.toString());
    }
    @Test
    public void replaceBy() {
        System.out.println("replaceBy");
        IntArrayList list = new IntArrayList();
        list.add(1); list.add(2); list.add(2); list.add(1);
        IntArrayList list1 = Utilities.replaceBy(list,1,3);
        assertTrue(list == list1);
        assertEquals("[3, 2, 2, 3]",list.toString());}

    @Test
    public void isSubset() {
        System.out.println("isSubset");
        IntArrayList list1 = new IntArrayList();
        list1.add(1); list1.add(2); list1.add(3); list1.add(4);

        IntArrayList list2 = new IntArrayList();
        list2.add(2); list2.add(1);
        assertTrue(Utilities.isSubset(list2,list1));
        assertFalse(Utilities.isSubset(list1,list2));

        list2.add(4); list2.add(3);
        assertTrue(Utilities.isSubset(list2,list1));
        assertTrue(Utilities.isSubset(list1,list2));}

}