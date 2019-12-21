package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;
import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 26.08.2018.
 */
public class ClauseTest {

    private static int counter = 1;

    private Clause make(int... literals) {
        Clause cl = new Clause(counter++,literals.length);
        int i = -1;
        for(int l:literals) {
            cl.add(new CLiteral(l,cl,++i));}
        return cl;}

    @Test
    public void addCLiteral() throws Exception {
        System.out.println("add");
        Clause cl = new Clause(1, 3);
        assertEquals(0, cl.size());
        CLiteral lit = new CLiteral(5);
        cl.add(lit);
        assertEquals(1, cl.size());
        CLiteral lit1 = new CLiteral(5);
        cl.add(lit1);
        assertEquals(2, cl.size());
        CLiteral lit2 = new CLiteral(-5);
        cl.add(lit2);
        assertEquals(3, cl.size());
        assertEquals("1:(5,5,-5)",cl.toString());
        assertEquals(ClauseStructure.MIXED,cl.structure);
        assertTrue(cl.hasDoubles());
        assertTrue(cl.hasComplementaries());
        assertEquals(3,cl.size());
    }


    @Test
    public void clause() throws Exception {
        System.out.println("clause");
        ArrayList<CLiteral<Clause>> lits = new ArrayList<>();
        lits.add(new CLiteral<Clause>(1));
        lits.add(new CLiteral<Clause>(2));
        Clause cl = new Clause(1,lits);
        assertEquals("1:(1,2)",cl.toString());
    }
    @Test
    public void make() throws Exception {
        System.out.println("make");
        counter = 1;
        Clause c1 = make(1, 2, 3);
        Clause c2 = make(-1, -2, 3);
        Clause c3 = make(-1, -2, -3);
        assertEquals("1:(1,2,3)",c1.toString());
        assertEquals("2:(-1,-2,3)",c2.toString());
        assertEquals("3:(-1,-2,-3)",c3.toString());
        assertEquals(ClauseStructure.POSITIVE,c1.structure);
        assertEquals(ClauseStructure.MIXED,c2.structure);
        assertEquals(ClauseStructure.NEGATIVE,c3.structure);
    }

    @Test
    public void removeLiteral() throws Exception {
        System.out.println("remove");
        counter = 1;
        Clause c1 = make(1, 2, 3);
        c1.remove(c1.getCLiteral(1));
        assertEquals("1:(1,3)",c1.toString());
        c1.remove(c1.getCLiteral(1));
        assertEquals("1:(1)",c1.toString());
        c1.remove(c1.getCLiteral(0));
        assertEquals("1:()",c1.toString());
        assertTrue(c1.isEmpty());

        c1 = make(1, 2, 3);
        c1.remove(c1.getCLiteral(2));
        assertEquals("2:(1,2)",c1.toString());
        c1.remove(c1.getCLiteral(0));
        assertEquals("2:(2)",c1.toString());
    }

    @Test
    public void removeAtPosition() throws Exception {
        System.out.println("removeAtPosition");
        counter = 1;
        Clause c1 = make(1, 2, 3);
        c1.removeAtPosition(1);
        assertEquals("1:(1,3)",c1.toString());
        CLiteral<Clause> lit = c1.getCLiteral(1);
        assertEquals(1,lit.clausePosition);
    }


    @Test
    public void contains() throws Exception {
        System.out.println("getLiteral, contains");
        counter = 1;
        Clause cl = make(5, -6, 7);

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
        counter = 1;
        Clause cll = make(5, -6, 7);
        Clause cl2 = new Clause(2,3);
        CLiteral lit10 = new CLiteral(10);
        cl2.add(lit10);
        assertTrue(cl2.isSubset(lit10, cll));
        CLiteral lit11 = new CLiteral(7);
        cl2.add(lit11);
        assertTrue(cl2.isSubset(lit10, cll));
        CLiteral lit12 = new CLiteral(-6);
        cl2.add(lit12);
        assertTrue(cl2.isSubset(lit10, cll));
        assertFalse(cl2.isSubset(lit11, cll));
    }



    @Test
    public void symboltable() throws Exception {
        System.out.println("symboltable");
        Clause cl1 = new Clause(1, 3);
        CLiteral lit1 = new CLiteral(5);
        cl1.add(lit1);
        CLiteral lit2 = new CLiteral(-6);
        cl1.add(lit2);
        CLiteral lit3 = new CLiteral(7);
        cl1.add(lit3);
        Symboltable st = new Symboltable(10);
        st.setName(5, "five");
        st.setName(6, "six");
        st.setName(7, "seven");
        assertEquals("1:(five,-six,seven)", cl1.toString(st));
    }

    @Test
    public void doubles() throws Exception {
        System.out.println("double tautology");
        counter = 1;
        Clause c1 = make(5, -6, -5, -6, -6);
        assertTrue(c1.hasDoubles());
        assertTrue(c1.hasComplementaries());
        assertTrue(c1.removeDoubles());
        assertEquals("1:(5,-6,-5)",c1.toString());

    }

    @Test
    public void iterator() throws Exception {
        System.out.println("iterator");
        counter = 1;
        Clause c1 = make(5, -6, -5, -6, -6);
        String st = "";
        for(CLiteral<Clause> lit : c1) {st += lit.toString();}
        assertEquals("5-6-5-6-6",st);
    }

    }