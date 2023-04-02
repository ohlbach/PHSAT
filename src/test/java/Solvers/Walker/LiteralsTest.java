package Solvers.Walker;

import junit.framework.TestCase;

public class LiteralsTest extends TestCase {


    public void testAddLiteral() {
        System.out.println("addLiteral");
        Literals literals = new Literals(10);
        Literal lit1 = new Literal(3,2);
        literals.addLiteral(lit1);
        assertEquals(1,literals.size(3));
        assertEquals(0,literals.size(-3));
        assertEquals("3^2@0,",literals.toString(3));
        Literal lit2 = new Literal(3,3);
        literals.addLiteral(lit2);
        assertEquals(2,literals.size(3));
        assertEquals(0,literals.size(-3));
        assertEquals("3^3@0,3^2@0,",literals.toString(3));
        Literal lit3 = new Literal(-3,1);
        literals.addLiteral(lit3);
        assertEquals(2,literals.size(3));
        assertEquals(1,literals.size(-3));
        assertEquals("3^3@0,3^2@0,",literals.toString(3));
        assertEquals("-3@0,",literals.toString(-3));
        assertEquals(0,literals.size(2));
        assertEquals("",literals.toString(2));
    }

    public void testRemoveLiteral() {
        System.out.println("removeLiteral");
        Literals literals = new Literals(10);
        Literal lit1 = new Literal(3,2);
        literals.addLiteral(lit1);
        literals.removeLiteral(lit1);
        assertEquals(0,literals.size(3));
        assertEquals("",literals.toString(3));
        literals.addLiteral(lit1);
        Literal lit2 = new Literal(3,1);
        literals.addLiteral(lit2);
        assertEquals(2,literals.size(3));
        assertEquals("3@0,3^2@0,",literals.toString(3));
        literals.removeLiteral(lit1);
        assertEquals(1,literals.size(3));
        assertEquals("3@0,",literals.toString(3));
        literals.addLiteral(lit1);
        literals.removeLiteral(lit1);
        assertEquals(1,literals.size(3));
        assertEquals("3@0,",literals.toString(3));
        assertTrue(lit2 == literals.getFirstLiteralObject(3));

        lit1 = new Literal(1,1);
        literals.addLiteral(lit1);
        lit2 = new Literal(1,2);
        literals.addLiteral(lit2);
        Literal lit3 = new Literal(1,3);
        literals.addLiteral(lit3);
        Literal lit4 = new Literal(1,4);
        literals.addLiteral(lit4);
        assertEquals(4,literals.size(1));
        assertEquals("1^4@0,1^3@0,1^2@0,1@0,",literals.toString(1));
        literals.removeLiteral(lit2);
        assertEquals(3,literals.size(1));
        assertEquals("1^4@0,1^3@0,1@0,",literals.toString(1));
        literals.removeLiteral(lit3);
        assertEquals(2,literals.size(1));
        assertEquals("1^4@0,1@0,",literals.toString(1));
        literals.removeLiteral(lit1);
        assertEquals(1,literals.size(1));
        assertEquals("1^4@0,",literals.toString(1));
        literals.removeLiteral(lit4);
        assertEquals(0,literals.size(1));
        assertEquals("",literals.toString(1));
        assertTrue(literals.isEmpty(1));

    }

    public void testReplaceLiteral() {
        System.out.println("replaceLiteral");
        Literals literals = new Literals(10);
        Literal lit1 = new Literal(3,1);
        literals.addLiteral(lit1);
        lit1.literal = 5;
        assertTrue(literals.replaceLiteral(lit1,3));
        assertEquals("5@0,",literals.toString(5));
    }

    public void testRemovePredicate() {
        System.out.println("removePredicate");
        Literals literals = new Literals(10);
        Literal lit1 = new Literal(3,1);
        literals.addLiteral(lit1);
        literals.removePredicate(3);
        assertTrue(literals.isEmpty(3));

    }

}