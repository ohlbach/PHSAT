package Datastructures.Theory;

import org.junit.Test;

import java.util.Arrays;
import java.util.function.Consumer;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 25.08.2018.
 */
public class ModelTest {
    @Test
    public void add() throws Exception {
        System.out.println("add");
        Model mod = new Model(5);
        assertEquals(0,mod.add(3));
        assertEquals(1,mod.add(3));
        assertEquals(-1,mod.add(-3));
        assertEquals(0,mod.add(-2));
        assertEquals(-1,mod.add(2));
        assertEquals(1,mod.add(-2));
        //System.out.println(mod.toString());
        }


    @Test
    public void isTrueFalse() throws Exception {
        System.out.println("isTrue, isFalse");
        Model mod = new Model(5);
        assertFalse(mod.isTrue(3));
        assertFalse(mod.isTrue(-3));
        assertFalse(mod.isFalse(3));
        assertFalse(mod.isFalse(-3));

        mod.add(3);
        assertTrue(mod.isTrue(3));
        assertFalse(mod.isTrue(-3));
        assertFalse(mod.isFalse(3));
        assertTrue(mod.isFalse(-3));

        mod.add(-2);
        assertTrue(mod.isTrue(-2));
        assertFalse(mod.isTrue(2));
        assertFalse(mod.isFalse(-2));
        assertTrue(mod.isFalse(2));

    }


    @Test
    public void contains() throws Exception {
        System.out.println("contains");
        Model mod = new Model(5);
        assertFalse(mod.contains(3));
        assertFalse(mod.contains(-3));
        mod.add(3);
        assertTrue(mod.contains(3));
        assertTrue(mod.contains(-3));
    }

    @Test
    public void isFull() throws Exception {
        System.out.println("isFull");
        Model mod = new Model(2);
        assertTrue(mod.isEmpty());
        assertFalse(mod.isFull());
        mod.add(1);
        assertFalse(mod.isEmpty());
        assertFalse(mod.isFull());
        mod.add(2);
        assertTrue(mod.isFull());
    }

    @Test
    public void cloneStatus() throws Exception {
        System.out.println("cloneStatus");
        Model mod = new Model(5);
        mod.add(3);
        mod.add(-5);
        byte[] st = mod.cloneStatus();
        assertEquals("[0, 0, 0, 1, 0, -1]",Arrays.toString(st));
    }

    @Test
    public void cloneModel() throws Exception {
        System.out.println("cloneModel");
        Model mod = new Model(5);
        mod.add(3);
        mod.add(-5);
        Model mod1 = mod.clone();
        assertEquals("[3, -5]",mod1.toString());
        assertTrue(mod1.isTrue(3));
    }

    @Test
    public void observer() throws Exception {
        System.out.println("observer");
        Model mod = new Model(5);
        StringBuilder st = new StringBuilder();
        Consumer<Integer> obs = lit->st.append(lit + " ");
        mod.addTrueLiteralObserver(obs);
        mod.add(3);
        mod.add(-5);
        assertEquals("3 -5 ",st.toString());
        mod.removeTrueLiteralObserver(obs);
        mod.add(2);
        assertEquals("3 -5 ",st.toString());
    }
}