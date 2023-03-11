package Datastructures.Literals;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 25.08.2018.
 */
public class CLiteralTest {

    @Test
    public void getLiteral1()  {
        System.out.println("literal");
        CLiteral lit = new CLiteral(3);
        assertEquals(3,lit.literal);
    }

    @Test
    public void clauseTest() throws Exception {
        System.out.println("getClause, getPosition");
        Clause cl = new Clause(1, Quantifier.OR, 3);
        CLiteral lit = new CLiteral(3,cl,2,(short)1);
        assertEquals(cl,lit.clause);
        assertEquals(2,lit.clausePosition);}


    @Test
    public void setClause() throws Exception {
        System.out.println("setClause");
        Clause cl = new Clause(1, Quantifier.OR,3);
        CLiteral lit = new CLiteral(3);
        lit.setClause(cl,2); assertEquals(cl,lit.clause);
        assertEquals(2,lit.clausePosition);}

    @Test
    public void symboltable() throws Exception {
        System.out.println("symboltable");
        Symboltable stb = new Symboltable(5);
        stb.setName(1, "A");
        stb.setName(2, "B");

        Clause cl = new Clause(1, Quantifier.OR, 3);
        CLiteral lit = new CLiteral(1, cl, 1,(short)1);
        assertEquals("1",lit.toString());
        assertEquals("A@1",lit.toString(stb,(cla->""+cla.id)));

    }



}