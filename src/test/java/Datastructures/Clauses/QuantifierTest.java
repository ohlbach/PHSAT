package Datastructures.Clauses;

import junit.framework.TestCase;

public class QuantifierTest extends TestCase {

    public void testGetConnective() {
        System.out.println("getConnective");
        assertEquals(">=", Quantifier.getQuantifier(">=").toString());
        assertEquals("<=", Quantifier.getQuantifier("<=").toString());
        assertEquals("=", Quantifier.getQuantifier("=").toString());
        assertEquals("o", Quantifier.getQuantifier("o").toString());
        assertEquals("a", Quantifier.getQuantifier("a").toString());
        assertEquals("e", Quantifier.getQuantifier("e").toString());

        assertEquals(",", Quantifier.getQuantifier(">=").separator);
        assertEquals(",", Quantifier.getQuantifier("<=").separator);
        assertEquals(",", Quantifier.getQuantifier("=").separator);
        assertEquals(",", Quantifier.getQuantifier("o").separator);
        assertEquals("&", Quantifier.getQuantifier("a").separator);
        assertEquals("=", Quantifier.getQuantifier("e").separator);

    }

    public void testGetConnectiveInt() {
        System.out.println("getConnective from int");
        assertEquals(Quantifier.OR, Quantifier.getQuantifier(Quantifier.OR.ordinal()));
        assertEquals(Quantifier.AND, Quantifier.getQuantifier(Quantifier.AND.ordinal()));
        assertEquals(Quantifier.EXACTLY, Quantifier.getQuantifier(Quantifier.EXACTLY.ordinal()));
        assertEquals(Quantifier.EQUIV, Quantifier.getQuantifier(Quantifier.EQUIV.ordinal()));
        assertEquals(Quantifier.ATMOST, Quantifier.getQuantifier(Quantifier.ATMOST.ordinal()));
        assertEquals(Quantifier.ATLEAST, Quantifier.getQuantifier(Quantifier.ATLEAST.ordinal()));
        assertEquals(Quantifier.INTERVAL, Quantifier.getQuantifier(Quantifier.INTERVAL.ordinal()));
    }

    public void testIsInterval() {
        System.out.println("isInterval");
        assertTrue(Quantifier.isInterval(Quantifier.INTERVAL.ordinal()));
        assertFalse(Quantifier.isInterval(Quantifier.ATLEAST.ordinal()));

        assertTrue(Quantifier.INTERVAL.isInterval());
        assertFalse(Quantifier.OR.isInterval());
    }


    public void testIsQuantifier() {
        System.out.println("isQuantifier");
        assertTrue(Quantifier.isQuantifier(Quantifier.ATLEAST.ordinal()));
        assertTrue(Quantifier.isQuantifier(Quantifier.ATMOST.ordinal()));
        assertTrue(Quantifier.isQuantifier(Quantifier.EXACTLY.ordinal()));
        assertFalse(Quantifier.isQuantifier(Quantifier.AND.ordinal()));
        assertFalse(Quantifier.isQuantifier(Quantifier.OR.ordinal()));
        assertFalse(Quantifier.isQuantifier(Quantifier.EQUIV.ordinal()));
        assertTrue(Quantifier.isQuantifier(Quantifier.INTERVAL.ordinal()));

        assertTrue(Quantifier.ATLEAST.isQuantifier());
        assertTrue(Quantifier.ATMOST.isQuantifier());
        assertTrue(Quantifier.EXACTLY.isQuantifier());
        assertFalse(Quantifier.AND.isQuantifier());
        assertFalse(Quantifier.OR.isQuantifier());
        assertFalse(Quantifier.EQUIV.isQuantifier());
        assertTrue(Quantifier.INTERVAL.isQuantifier());
    }

    public void testSize() {
        System.out.println("size");
        assertEquals(7, Quantifier.size());
    }
}