package Datastructures.Clauses;
import Datastructures.Literals.CLiteral;
import Datastructures.Theory.ImplicationGraph;
import Utilities.Utilities;
import com.sun.istack.internal.Pool;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 28.08.2018.
 */
public class ClauseListTest {


    @Test
    public void addClause() throws Exception {
        System.out.println("add, remove Clause");
        ClauseList clauses = new ClauseList(10, 10);
        Clause c1 = Utilities.makeClause("1","1,-3");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2","-1,3,5");
        clauses.addClause(c2);
        assertEquals("1: (1,-3)\n" +
                "2: (-1,3,5)\n",clauses.toString());
        assertEquals(c1,clauses.getClause("1"));
        clauses.removeClause(c1);
        assertEquals("2: (-1,3,5)\n",clauses.toString());
        assertNull(clauses.getClause("1"));
    }

    @Test
    public void index() throws Exception {
        System.out.println("Literal Index");
        ClauseList clauses = new ClauseList(10,10);
        assertEquals(0,clauses.getOccurrences(1));
        assertTrue(clauses.isEmpty());
        Clause c1 = Utilities.makeClause("1","1,-3,6");
        clauses.addClause(c1);
        assertEquals(1,clauses.getOccurrences(1));
        assertEquals(1,clauses.getOccurrences(-3));
        assertEquals(0,clauses.getOccurrences(3));
        assertTrue(clauses.isPure(1));
        assertEquals(c1,clauses.getClause("1"));
        Clause c2 = Utilities.makeClause("2","3,-6,1");
        clauses.addClause(c2);
        assertEquals(1,clauses.getOccurrences(6));
        assertTrue(clauses.isPure(1));
        assertFalse(clauses.isPure(6));
        assertEquals("[6]",clauses.getLiterals(6).toString());
        assertEquals("[1, 1]",clauses.getLiterals(1).toString());
    }
    @Test
    public void removeClause() throws Exception {
        System.out.println("remove Clauses");
        ClauseList clauses = new ClauseList(10, 10);
        Clause c11 = Utilities.makeClause("1", "1,-3,6");
        clauses.addClause(c11);
        Clause c12 = Utilities.makeClause("2", "-3,6,-7");
        clauses.addClause(c12);
        assertEquals("[6, 6]",clauses.getLiterals(6).toString());
        clauses.removeClause(c11);
        assertEquals("[6]",clauses.getLiterals(6).toString());
        assertEquals(c12,clauses.getLiterals(6).get(0).clause);
        assertEquals(0,clauses.getLiterals(1).size());
    }

    @Test
    public void removeLiteral() throws Exception {
        System.out.println("remove Literal no observers");
        ClauseList clauses = new ClauseList(10, 10);
        Clause c11 = Utilities.makeClause("1", "1,-3,6");
        clauses.addClause(c11);
        Clause c12 = Utilities.makeClause("2", "-3,6,-7");
        clauses.addClause(c12);
        CLiteral lit1 = c11.cliterals.get(0);
        clauses.removeLiteral(lit1);
        assertEquals("1: (-3,6)\n" +
                "2: (-3,6,-7)\n", clauses.toString());
    }

    @Test
    public void removeLiteralObs() throws Exception {
        System.out.println("remove Literal with observers");
        ClauseList clauses = new ClauseList(10, 10);
        Clause c11 = Utilities.makeClause("1", "1,-3,6");
        clauses.addClause(c11);
        Clause c12 = Utilities.makeClause("2", "-3,6,-7");
        clauses.addClause(c12);
        CLiteral lit1 = c11.cliterals.get(0);
        StringBuilder st = new StringBuilder();
        clauses.literalRemovalObservers.add(cl -> st.append(cl.toString()));
        clauses.removeLiteral(lit1);
        assertEquals("1: (-3,6)",st.toString());
    }

    @Test
    public void makeTrue() throws Exception {
        System.out.println("makeTrue");
        ClauseList clauses = new ClauseList(10, 10);
        Clause c1 = Utilities.makeClause("1", "1,-3,6");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2", "-3,6,-7");
        clauses.addClause(c2);
        Clause c3 = Utilities.makeClause("3", "2,3,7");
        clauses.addClause(c3);
        Clause c4 = Utilities.makeClause("4", "4,6,-7");
        clauses.addClause(c4);
        Clause c5 = Utilities.makeClause("5", "3,8,-7");
        clauses.addClause(c5);
        StringBuilder st = new StringBuilder();
        clauses.literalRemovalObservers.add(cl -> st.append(cl.toString()));
        clauses.makeTrue(3);
        assertEquals("1: (1,6)\n" +
                "2: (6,-7)\n" +
                "4: (4,6,-7)\n",clauses.toString());
        assertEquals("1: (1,6)2: (6,-7)",st.toString());
    }

    @Test
    public void replaceBy() throws Exception {
        System.out.println("replaceBy");
        ClauseList clauses = new ClauseList(10, 10);
        Clause c1 = Utilities.makeClause("1", "1,-3,6");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2", "-3,6,-7");
        clauses.addClause(c2);
        Clause c3 = Utilities.makeClause("3", "2,3,7");
        clauses.addClause(c3);
        Clause c4 = Utilities.makeClause("4", "4,6,-7");
        clauses.addClause(c4);
        Clause c5 = Utilities.makeClause("5", "3,8,-7");
        clauses.addClause(c5);
        StringBuilder str = new StringBuilder();
        clauses.literalRemovalObservers.add(cl -> str.append(cl.toString()));
        StringBuilder stp = new StringBuilder();
        clauses.literalReplacementObservers.add(cl -> stp.append(cl.toString()));
        clauses.replaceByRepresentative(7,3);
        assertEquals("1: (1,-7,6)\n" +
                "2: (6,-7)\n" +
                "3: (2,7)\n" +
                "4: (4,6,-7)\n",clauses.toString());
        assertEquals("3: (2,7)2: (6,-7)",str.toString());
        assertEquals("-7",stp.toString());
    }

    @Test
    public void literalImplies() throws Exception {
        System.out.println("literalImplies");
        ClauseList clauses = new ClauseList(10, 10);
        Clause c1 = Utilities.makeClause("1", "1,-3,6");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2", "-3,6,-7");
        clauses.addClause(c2);
        Clause c3 = Utilities.makeClause("3", "2,3,7");
        clauses.addClause(c3);
        Clause c4 = Utilities.makeClause("4", "4,6,-7");
        clauses.addClause(c4);
        Clause c5 = Utilities.makeClause("5", "3,8,7");
        clauses.addClause(c5);

        ImplicationGraph ig = new ImplicationGraph(10);
        ig.addClause(-1,3);
        StringBuilder st1 = new StringBuilder();
        clauses.literalImplies(1,ig).forEach(lit-> st1.append(lit.toFullString()+". "));
        assertEquals("1@1,0. 3@3,1. 3@5,0. ",st1.toString());

        ig.addClause(-7,1);
        StringBuilder st2 = new StringBuilder();
        clauses.literalIsImplied(1,ig).forEach(lit-> st2.append(lit.toFullString()+". "));
        assertEquals("1@1,0. 7@3,2. 7@5,2. ",st2.toString());

        StringBuilder st3 = new StringBuilder();
        clauses.literalContradict(-3,ig).forEach(lit-> st3.append(lit.toFullString()+". "));
        assertEquals("3@3,1. 3@5,0. 7@3,2. 7@5,2. 1@1,0. ",st3.toString());

    }
}