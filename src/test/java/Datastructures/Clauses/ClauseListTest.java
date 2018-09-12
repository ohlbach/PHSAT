package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Model;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 28.08.2018.
 */
public class ClauseListTest {
    @Test
    public void addClause() throws Exception {
        System.out.println("addClause");
        Model model = new Model(10);
        ClauseList clauses = new ClauseList(10,null);
        Clause c1 = new Clause(1,"1,-3");
        clauses.addClause(c1);
        Clause c2 = new Clause(2,"-1,3,5");
        clauses.addClause(c2);
        assertEquals("1: 1,-3,\n" +
                "2: -1,3,5,\n",clauses.toString());
        assertEquals(c1,clauses.getClause(1));
        clauses.removeClause(c1);
        assertEquals("2: -1,3,5,\n",clauses.toString());
        assertNull(clauses.getClause(1));
    }

    @Test
    public void removeClause() throws Exception {

    }

}