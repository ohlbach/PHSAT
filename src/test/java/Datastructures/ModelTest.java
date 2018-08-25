package Datastructures;

import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 25.08.2018.
 */
public class ModelTest {
    @Test
    public void pushLiteral() throws Exception {
        Model mod = new Model(5);
        assertEquals(0,mod.pushLiteral(3));
        assertEquals(1,mod.pushLiteral(3));
        assertEquals(-1,mod.pushLiteral(-3));
        assertEquals(0,mod.pushLiteral(-2));
        assertEquals(-1,mod.pushLiteral(2));
        assertEquals(1,mod.pushLiteral(-2));
        System.out.println(mod.toString());}

    @Test
    public void pop() throws Exception {

    }

    @Test
    public void isTrue() throws Exception {

    }

    @Test
    public void isFalse() throws Exception {

    }

    @Test
    public void truth() throws Exception {

    }

    @Test
    public void isAsserted() throws Exception {

    }

}