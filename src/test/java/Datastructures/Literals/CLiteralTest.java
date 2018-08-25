package Datastructures.Literals;

import Datastructures.Clauses.Clause;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 25.08.2018.
 */
public class CLiteralTest {
    @Test
    public void getLiteral1() throws Exception {
        System.out.println("getLiteral");
        CLiteral lit = new CLiteral(3);
        assertEquals(3,lit.getLiteral());
    }

    @Test
    public void getClause() throws Exception {
        System.out.println("getClause, getPosition");
        Clause cl = new Clause(3);
        CLiteral lit = new CLiteral(3,cl,2);
        assertEquals(cl,lit.getClause());
        assertEquals(2,lit.getPosition());}


    @Test
    public void setClause() throws Exception {
        System.out.println("setClause");
        Clause cl = new Clause(3);
        CLiteral lit = new CLiteral(3);
        lit.setClause(cl,2); assertEquals(cl,lit.getClause());
        assertEquals(2,lit.getPosition());}


    @Test
    public void removeClause() throws Exception {
        System.out.println("removeClause");
        Clause cl = new Clause(3);
        CLiteral lit = new CLiteral(3,cl,2);
        lit.removeClause();
        assertNull(lit.getClause());
        assertEquals(-1,lit.getPosition());
    }

    @Test
    public void getLiteral() throws Exception {
        CLiteral lit = new CLiteral(5);
        assertEquals(5, lit.getLiteral());

    }

}