package Datastructures.Clauses;
import Utilities.Utilities;
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
        Clause c2 = Utilities.makeClause("2","3,-6,7");
        clauses.addClause(c2);
        assertEquals(1,clauses.getOccurrences(6));
        assertTrue(clauses.isPure(1));
        assertFalse(clauses.isPure(6));
    }

}