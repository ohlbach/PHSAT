package Solvers.Resolution;

import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import junit.framework.TestCase;

public class LiteralsTest extends TestCase {
    static Symboltable symboltable = new Symboltable(10);
    static {
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
    }

    static String toString(Literals literals, int literal) {
        StringBuilder st = new StringBuilder();
        Literal l = literals.getFirstLiteralObject(1);
        while(l != null) {
            st.append(l.toString(symboltable)).append(",");
            l = l.nextLiteral;}
        return st.toString();}

    public void testAddLiteral() {
        System.out.println("addLiteral");
        Literals literals = new Literals(10);
        Literal l1 = new Literal(1,1);
        assertTrue(literals.isEmpty(1));
        assertEquals(0,literals.size(1));
        assertNull(literals.getFirstLiteralObject(1));
        //System.out.println(literals.toString(symboltable));

        literals.addLiteral(l1);
        assertFalse(literals.isEmpty(1));
        assertEquals(1,literals.size(1));
        assertEquals(l1, literals.getFirstLiteralObject(1));
        assertNull(literals.getFirstLiteralObject(-1));
        //System.out.println(literals.toString(symboltable));
        //System.out.println(literals.toString());

        Literal lm1 = new Literal(-1,1);
        literals.addLiteral(lm1);
        assertEquals(1,literals.size(1));
        assertEquals(1,literals.size(-1));
        assertEquals(l1, literals.getFirstLiteralObject(1));
        assertEquals(lm1, literals.getFirstLiteralObject(-1));
        //System.out.println(literals.toString(symboltable));

        Literal l12 = new Literal(1,2);
        literals.addLiteral(l12);
        assertEquals(2,literals.size(1));
        //System.out.println(literals.toString(symboltable));

        Literal l31 = new Literal(3,1);
        literals.addLiteral(l31);
        assertEquals(2,literals.size(1));
        assertEquals(1,literals.size(3));
        //System.out.println(literals.toString(symboltable));
    }

    public void testWhile() {
        System.out.println("while");
        Literals literals = new Literals(10);
        Literal l1 = new Literal(1,1);
        literals.addLiteral(l1);
        assertEquals("p,",toString(literals,1));

        Literal l2 = new Literal(1,2);
        literals.addLiteral(l2);
        assertEquals("p^2,p,",toString(literals,1));

        Literal l3 = new Literal(1,3);
        literals.addLiteral(l3);
        assertEquals("p^3,p^2,p,",toString(literals,1));

    }

    public void testRemoveLiteral() {
        System.out.println("remove");
        Literals literals = new Literals(10);
        Literal l1 = new Literal(1,1);
        literals.addLiteral(l1);
        literals.removeLiteral(l1);
        assertNull(literals.getFirstLiteralObject(1));
        assertEquals(0,literals.size(1));

        literals.addLiteral(l1);
        assertNull(l1.nextLiteral);
        assertNull(l1.previousLiteral);

        Literal l2 = new Literal(1,2);
        literals.addLiteral(l2);
        assertEquals(l1, l2.nextLiteral);
        assertEquals(l2,l1.previousLiteral);

        literals.removeLiteral(l2); // the first literal
        assertEquals(1,literals.size(1));
        assertNull(l1.nextLiteral);
        assertNull(l1.previousLiteral);
        assertEquals(l1, literals.getFirstLiteralObject(1));

        literals.addLiteral(l2);
        Literal l3 = new Literal(1,3);
        literals.addLiteral(l3);
        assertEquals("p^3,p^2,p,",toString(literals,1));

        literals.removeLiteral(l2);
        assertEquals("p^3,p,",toString(literals,1));
        literals.removeLiteral(l1);
        assertEquals("p^3,",toString(literals,1));
        literals.removeLiteral(l3);
        assertEquals("",toString(literals,1));

    }

    public void testRemovePredicate() {
        System.out.println("removePredicate");
        Literals literals = new Literals(10);
        Literal l1 = new Literal(1,1);
        literals.addLiteral(l1);
        Literal l2 = new Literal(1,2);
        literals.addLiteral(l2);
        assertEquals("p^2,p,",toString(literals,1));
        literals.removePredicate(-1);
        assertEquals("",toString(literals,1));
    }

    public void testIterating() {
        System.out.println("iterating");
        Clause clause = new Clause(new int[]{1, Quantifier.OR.ordinal(),1,2,3});
        Literals literals = new Literals(10);
        Literal l1 = new Literal(1, 1);l1.clause = clause;
        literals.addLiteral(l1);
        Literal l2 = new Literal(1, 2);l2.clause = clause;
        literals.addLiteral(l2);
        Literal l3 = new Literal(1, 3);l3.clause = clause;
        literals.addLiteral(l3);
        Literal l4 = new Literal(1, 4);l4.clause = clause;
        literals.addLiteral(l4);

        String s = "";
        Literal lit = literals.getFirstLiteralObject(1);
        while(lit != null) {s += lit.multiplicity; lit =lit.nextLiteral;}
        assertEquals("4321",s);

        s = "";
        lit = literals.getFirstLiteralObject(1).nextLiteral;
        literals.removeLiteral(l3);
        while(lit != null) {
            if(lit.clause == null) {lit =lit.nextLiteral; continue;}
            s += lit.multiplicity; lit =lit.nextLiteral;}
        assertEquals("21",s);

        s = "";
        lit = literals.getFirstLiteralObject(1).nextLiteral;
        literals.removeLiteral(l2);
        while(lit != null) {
            if(lit.clause == null) {lit =lit.nextLiteral; continue;}
            s += lit.multiplicity; lit =lit.nextLiteral;}
        assertEquals("1",s);

        s = "";
        lit = literals.getFirstLiteralObject(1);

        while(lit != null) {
            if(lit.clause == null) {lit =lit.nextLiteral; continue;}
            s += lit.multiplicity; lit =lit.nextLiteral;}
        assertEquals("41",s);

    }
    }