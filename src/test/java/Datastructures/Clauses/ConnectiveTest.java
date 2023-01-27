package Datastructures.Clauses;

import junit.framework.TestCase;

public class ConnectiveTest extends TestCase {

    public void testGetConnective() {
        System.out.println("getConnective");
        assertEquals(">=",Connective.getConnective(">=").toString());
        assertEquals("<=",Connective.getConnective("<=").toString());
        assertEquals("=",Connective.getConnective("=").toString());
        assertEquals("o",Connective.getConnective("o").toString());
        assertEquals("a",Connective.getConnective("a").toString());
        assertEquals("e",Connective.getConnective("e").toString());

        assertEquals(",",Connective.getConnective(">=").separator);
        assertEquals(",",Connective.getConnective("<=").separator);
        assertEquals(",",Connective.getConnective("=").separator);
        assertEquals(",",Connective.getConnective("o").separator);
        assertEquals("&",Connective.getConnective("a").separator);
        assertEquals("=",Connective.getConnective("e").separator);

    }

    public void testGetConnectiveInt() {
        System.out.println("getConnective from int");
        assertEquals(Connective.OR,Connective.getConnective(Connective.OR.ordinal()));
        assertEquals(Connective.AND,Connective.getConnective(Connective.AND.ordinal()));
        assertEquals(Connective.EXACTLY,Connective.getConnective(Connective.EXACTLY.ordinal()));
        assertEquals(Connective.EQUIV,Connective.getConnective(Connective.EQUIV.ordinal()));
        assertEquals(Connective.ATMOST,Connective.getConnective(Connective.ATMOST.ordinal()));
        assertEquals(Connective.ATLEAST,Connective.getConnective(Connective.ATLEAST.ordinal()));
        assertEquals(Connective.INTERVAL,Connective.getConnective(Connective.INTERVAL.ordinal()));
    }

    public void testIsInterval() {
        System.out.println("isInterval");
        assertTrue(Connective.isInterval(Connective.INTERVAL.ordinal()));
        assertFalse(Connective.isInterval(Connective.ATLEAST.ordinal()));

        assertTrue(Connective.INTERVAL.isInterval());
        assertFalse(Connective.OR.isInterval());
    }


    public void testIsQuantifier() {
        System.out.println("isQuantifier");
        assertTrue(Connective.isQuantifier(Connective.ATLEAST.ordinal()));
        assertTrue(Connective.isQuantifier(Connective.ATMOST.ordinal()));
        assertTrue(Connective.isQuantifier(Connective.EXACTLY.ordinal()));
        assertFalse(Connective.isQuantifier(Connective.AND.ordinal()));
        assertFalse(Connective.isQuantifier(Connective.OR.ordinal()));
        assertFalse(Connective.isQuantifier(Connective.EQUIV.ordinal()));
        assertTrue(Connective.isQuantifier(Connective.INTERVAL.ordinal()));

        assertTrue(Connective.ATLEAST.isQuantifier());
        assertTrue(Connective.ATMOST.isQuantifier());
        assertTrue(Connective.EXACTLY.isQuantifier());
        assertFalse(Connective.AND.isQuantifier());
        assertFalse(Connective.OR.isQuantifier());
        assertFalse(Connective.EQUIV.isQuantifier());
        assertTrue(Connective.INTERVAL.isQuantifier());
    }

    public void testSize() {
        System.out.println("size");
        assertEquals(7,Connective.size());
    }
}