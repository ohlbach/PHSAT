package Datastructures.Theory;

import Datastructures.LocalModel;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 25.08.2018.
 */
public class ModelTest {
    @Test
    public void push() throws Exception {
        System.out.println("push");
        LocalModel mod = new LocalModel(5);
        assertEquals(0,mod.push(3));
        assertEquals(1,mod.push(3));
        assertEquals(-1,mod.push(-3));
        assertEquals(0,mod.push(-2));
        assertEquals(-1,mod.push(2));
        assertEquals(1,mod.push(-2));
        System.out.println(mod.toString());}

    @Test
    public void pop() throws Exception {
        System.out.println("pop");
        LocalModel mod = new LocalModel(5);
        assertEquals(0,mod.size());
        assertEquals(0,mod.pop());
        mod.push(3);
        assertEquals(1,mod.size());
        assertEquals(1,mod.status(3));
        assertEquals(-1,mod.status(-3));
        mod.push(-2);
        assertEquals(-1,mod.status(2));
        assertEquals(1,mod.status(-2));
        assertEquals(2,mod.size());
        assertEquals(-2,mod.pop());
        assertEquals(1,mod.size());
        assertEquals(0,mod.status(2));
        assertEquals(1,mod.status(3));
        assertEquals(3,mod.pop());
        assertEquals(0,mod.size());
        assertEquals(0,mod.pop());
        assertEquals(0,mod.status(3));
    }

    @Test
    public void isTrueFalse() throws Exception {
        System.out.println("isTrue, isFalse");
        LocalModel mod = new LocalModel(5);
        assertFalse(mod.isTrue(3));
        assertFalse(mod.isTrue(-3));
        assertFalse(mod.isFalse(3));
        assertFalse(mod.isFalse(-3));

        mod.push(3);
        assertTrue(mod.isTrue(3));
        assertFalse(mod.isTrue(-3));
        assertFalse(mod.isFalse(3));
        assertTrue(mod.isFalse(-3));

        mod.push(-2);
        assertTrue(mod.isTrue(-2));
        assertFalse(mod.isTrue(2));
        assertFalse(mod.isFalse(-2));
        assertTrue(mod.isFalse(2));

    }


    @Test
    public void contains() throws Exception {
        System.out.println("contains");
        LocalModel mod = new LocalModel(5);
        assertFalse(mod.contains(3));
        assertFalse(mod.contains(-3));
        mod.push(3);
        assertTrue(mod.contains(3));
        assertTrue(mod.contains(-3));

        mod.pop();
        assertFalse(mod.contains(3));
        assertFalse(mod.contains(-3));
    }

    @Test
    public void isFull() throws Exception {
        System.out.println("isFull");
        LocalModel mod = new LocalModel(2);
        assertTrue(mod.isEmpty());
        assertFalse(mod.isFull());
        mod.push(1);
        assertFalse(mod.isEmpty());
        assertFalse(mod.isFull());
        mod.push(2);
        assertTrue(mod.isFull());
        mod.pop();
        assertFalse(mod.isFull());

    }

}