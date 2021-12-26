package Utilities;

import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import static org.junit.Assert.*;

public class DiophantineEquationTest {

    @Test
    public void maxValue() {
        System.out.println("maxValue");
        DiophantineEquation de = new DiophantineEquation(IntArrayList.wrap(new int[]{2,3}),4,5);
        assertEquals(9,de.maxValue());
    }

    @Test
    public void isSolvable() {
        System.out.println("isSolvable");
        DiophantineEquation de = new DiophantineEquation(IntArrayList.wrap(new int[]{2,2}),0,4);
        assertTrue(de.isSolvable());
        de = new DiophantineEquation(IntArrayList.wrap(new int[]{2,3,4}),5,11);
        assertTrue(de.isSolvable());
        de = new DiophantineEquation(IntArrayList.wrap(new int[]{2,3,4}),5,15);
        assertFalse(de.isSolvable());
        de = new DiophantineEquation(IntArrayList.wrap(new int[]{2,2,2}),0,2);
        assertTrue(de.isSolvable());
        de = new DiophantineEquation(IntArrayList.wrap(new int[]{2,2,2}),0,3);
        //System.out.println(de);
        assertFalse(de.isSolvable());
        de = new DiophantineEquation(IntArrayList.wrap(new int[]{2,2,2}),0,5);
        assertFalse(de.isSolvable());
        de = new DiophantineEquation(IntArrayList.wrap(new int[]{2,2,2}),0,7);
        assertFalse(de.isSolvable());
        de = new DiophantineEquation(IntArrayList.wrap(new int[]{2,2,2}),1,5);
        assertTrue(de.isSolvable());
        de = new DiophantineEquation(IntArrayList.wrap(new int[]{2,2,2}),5,5);
        assertTrue(de.isSolvable());
        //System.out.println(de);
    }

    @Test
    public void minSolution() {
        System.out.println("minSolution");
        DiophantineEquation de = new DiophantineEquation(IntArrayList.wrap(new int[]{2, 2}), 0, 3);
        assertEquals(4,de.minSolution());
        de = new DiophantineEquation(IntArrayList.wrap(new int[]{4,4,4}),1,5);
        assertEquals(5,de.minSolution());
        de = new DiophantineEquation(IntArrayList.wrap(new int[]{4,4,4}),1,10);
        assertEquals(12,de.minSolution());
        //System.out.println(de);
    }
    }