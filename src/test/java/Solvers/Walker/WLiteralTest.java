package Solvers.Walker;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 28.01.2020.
 */
public class WLiteralTest {

    @Test
    public void construct() throws Exception {
        System.out.println("Construct");
        WLiteral wl1 = new WLiteral(1);
        assertEquals("1",wl1.toString());

        Clause cl1 = new Clause(1,ClauseType.OR);
        cl1.add(wl1);


        Clause cl2 = new Clause(2, ClauseType.OR, 2);
        WLiteral wl2 = new WLiteral(2,cl2,0);
        WLiteral wl3 = new WLiteral(3,cl2,1);
        cl2.add(wl2); cl2.add(wl3);
        assertEquals("2:(2,3)",cl2.toString());
    }

    @Test
    public void score() throws Exception {
        System.out.println("score");
        WLiteral wl1 = new WLiteral(1);
        wl1.score = 5;
        assertEquals(5,wl1.score);}

}