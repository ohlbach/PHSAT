package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 26.08.2018.
 */
public class ClauseTest {
    
    @Test
    public void addCLiteral() throws Exception {
        System.out.println("addCLiteral, size");
        Clause cl = new Clause("1",3);
        assertEquals(0,cl.size());
        CLiteral lit = new CLiteral(5);
        assertEquals(0,cl.addCLiteral(lit));
        assertEquals(1,cl.size());
        CLiteral lit1 = new CLiteral(5);
        assertEquals(1,cl.addCLiteral(lit1));
        assertEquals(1,cl.size());
        CLiteral lit2 = new CLiteral(-5);
        assertEquals(-1,cl.addCLiteral(lit2));
        assertEquals(1,cl.size());
        CLiteral lit3 = new CLiteral(-6);
        assertEquals(0,cl.addCLiteral(lit3));
        assertEquals(2,cl.size());
        assertEquals(cl,lit3.clause);
        assertEquals(1,lit3.position);
    }

    @Test
    public void addCLiteralDirectly() throws Exception {
        System.out.println("addCLiteralDirectly");
        Clause cl = new Clause("1", 3);
        assertEquals(0, cl.size());
        CLiteral lit = new CLiteral(5);
        cl.addCLiteralDirectly(lit);
        assertEquals(1, cl.size());
        CLiteral lit1 = new CLiteral(5);
        cl.addCLiteralDirectly(lit1);
        assertEquals(2, cl.size());
        CLiteral lit2 = new CLiteral(-5);
        cl.addCLiteralDirectly(lit2);
        assertEquals(3, cl.size());
        assertEquals("1: (5,5,-5)",cl.toString());
    }

        @Test
    public void removeLiteral() throws Exception {
        System.out.println("removeLiteral");
        Clause cl = new Clause("1",3);
        CLiteral lit1 = new CLiteral(5);
        assertEquals(0,cl.addCLiteral(lit1));
        CLiteral lit2 = new CLiteral(-6);
        assertEquals(0,cl.addCLiteral(lit2));
        assertEquals(1,lit2.position);
        cl.removeLiteral(lit1);
        assertEquals(1,cl.size());
        assertEquals(0,lit2.position);
        cl.removeLiteral(lit2);
        assertEquals(0,cl.size());
    }

    @Test
    public void removeLiteralAtPosition() throws Exception {
        System.out.println("removeLiteralAtPosition");
        Clause cl = new Clause("1",3);
        CLiteral lit1 = new CLiteral(5);
        assertEquals(0,cl.addCLiteral(lit1));
        CLiteral lit2 = new CLiteral(-6);
        assertEquals(0,cl.addCLiteral(lit2));
        assertEquals(1,lit2.position);
        cl.removeLiteralAtPosition(0);
        assertEquals(1,cl.size());
        assertEquals(0,lit2.position);
        cl.removeLiteralAtPosition(0);
        assertEquals(0,cl.size());
    }


    @Test
    public void contains() throws Exception {
        System.out.println("getLiteral, contains");
        Clause cl = new Clause("1", 3);
        CLiteral lit1 = new CLiteral(5);
        cl.addCLiteral(lit1);
        CLiteral lit2 = new CLiteral(-6);
        cl.addCLiteral(lit2);
        CLiteral lit3 = new CLiteral(7);
        cl.addCLiteral(lit3);
        assertEquals(0, cl.contains(5));
        assertEquals(1, cl.contains(-6));
        assertEquals(2, cl.contains(7));
        assertEquals(-1, cl.contains(6));

        assertEquals(5,cl.getLiteral(0));
        assertEquals(-6,cl.getLiteral(1));
        assertEquals(7,cl.getLiteral(2));
    }

    @Test
    public void isSubset() throws Exception {
        System.out.println("isSubset");
        Clause cl1 = new Clause("1", 3);
        CLiteral lit1 = new CLiteral(5);
        cl1.addCLiteral(lit1);
        CLiteral lit2 = new CLiteral(-6);
        cl1.addCLiteral(lit2);
        CLiteral lit3 = new CLiteral(7);
        cl1.addCLiteral(lit3);

        Clause cl2 = new Clause("1", 3);
        CLiteral lit10 = new CLiteral(10);
        cl2.addCLiteral(lit10);
        assertTrue(cl2.isSubset(lit10, cl1));
        CLiteral lit11 = new CLiteral(7);
        cl2.addCLiteral(lit11);
        assertTrue(cl2.isSubset(lit10, cl1));
        CLiteral lit12 = new CLiteral(-6);
        cl2.addCLiteral(lit12);
        assertTrue(cl2.isSubset(lit10, cl1));
        assertFalse(cl2.isSubset(lit11, cl1));
    }

    @Test
    public void cloneTest() throws Exception {
        System.out.println("clone");
        Clause cl1 = new Clause("1", 3);
        CLiteral lit1 = new CLiteral(5);
        cl1.addCLiteral(lit1);
        CLiteral lit2 = new CLiteral(-6);
        cl1.addCLiteral(lit2);
        CLiteral lit3 = new CLiteral(7);
        cl1.addCLiteral(lit3);
        Clause cl2 = cl1.clone();
        assertEquals(cl1.toString(),cl2.toString());
    }

    @Test
    public void applyCLiteral() throws Exception {
        System.out.println("applyCLiteral");
        Clause cl1 = new Clause("1", 3);
        CLiteral lit1 = new CLiteral(5);
        cl1.addCLiteral(lit1);
        CLiteral lit2 = new CLiteral(-6);
        cl1.addCLiteral(lit2);
        CLiteral lit3 = new CLiteral(7);
        cl1.addCLiteral(lit3);
        StringBuilder st = new StringBuilder();
        cl1.applyToCLiteral(lit -> st.append(Integer.toString(lit.literal)+" "));
        cl1.applyToLiteral(lit -> st.append(Integer.toString(lit)+" "));
        assertEquals("5 -6 7 5 -6 7 ",st.toString());
    }

    @Test
    public void symboltable() throws Exception {
        System.out.println("symboltable");
        Clause cl1 = new Clause("CL1", 3);
        CLiteral lit1 = new CLiteral(5);
        cl1.addCLiteral(lit1);
        CLiteral lit2 = new CLiteral(-6);
        cl1.addCLiteral(lit2);
        CLiteral lit3 = new CLiteral(7);
        cl1.addCLiteral(lit3);
        Symboltable st = new Symboltable(10);
        st.setName(5, "five");
        st.setName(6, "six");
        st.setName(7, "seven");
        assertEquals("CL1: (five,-six,seven)", cl1.toString(st));
        assertEquals("       CL1: (five,-six,seven)", cl1.toString(10,st));
    }
    }