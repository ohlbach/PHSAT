package Datastructures.Clauses;

import Datastructures.Literals.LiteralGenerator;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 26.08.2018.
 */
public class ClauseGeneratorTest {
    @Test
    public void newClause() throws Exception {
        System.out.println("newClause");
        LiteralGenerator lg = new LiteralGenerator();
        ClauseGenerator cg = new ClauseGenerator(lg);
        ArrayList<Integer> literals = new ArrayList<>();
        literals.add(3);literals.add(5);literals.add(-2);
        Clause cl = cg.newClause(1,literals);
        assertEquals("1: 3,5,-2,",cl.toString());

    }

}