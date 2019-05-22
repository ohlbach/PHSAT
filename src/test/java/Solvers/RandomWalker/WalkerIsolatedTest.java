package Solvers.RandomWalker;

import Datastructures.Clauses.ClauseList;
import Datastructures.Theory.ImplicationDAG;
import org.junit.*;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 16.05.2019.
 */
public class WalkerIsolatedTest {
    @org.junit.Test
    public void addImplications() throws Exception {
        System.out.println("addImplications");
        ClauseList clauses = new ClauseList(10,10);
        ImplicationDAG dag = new ImplicationDAG();
        dag.addClause(-1,2);
        dag.addClause(-1,3);
        dag.addClause(-2,4);
        dag.addClause(-3,4);
        dag.addClause(-4,5);
        WalkerIsolated.addImplications(clauses,dag);
        assertTrue(clauses.toString().startsWith("P_0: (-4,5)\n" +
                "P_1: (-3,5)\n" +
                "P_2: (-2,5)\n" +
                "P_3: (-1,2)\n" +
                "P_4: (-1,3)\n" +
                "P_5: (-1,4)\n" +
                "P_6: (-1,5)"));
        //System.out.println(clauses);
        //System.out.println(dag);

    }

}