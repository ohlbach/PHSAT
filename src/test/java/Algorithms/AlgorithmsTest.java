package Algorithms;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Theory.ImplicationDAG;
import Utilities.Utilities;
import org.junit.Test;

import java.lang.reflect.Array;
import java.util.Arrays;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 08.10.2018.
 */
public class AlgorithmsTest {
    @Test
    public void subsumed() throws Exception {
        System.out.println("subsumed");
        ClauseList clauses = new ClauseList(10, 10);
        ImplicationDAG id = new ImplicationDAG();
        Clause c1 = Utilities.makeClause("1","1,-3,5");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2","-3,1,5");
        assertNull(Algorithms.subsumedAndResolved(c2,clauses,id));
        Clause c3 = Utilities.makeClause("3","-3,2,5");
        assertEquals(c3,Algorithms.subsumedAndResolved(c3,clauses,id));
        Clause c4 = Utilities.makeClause("4","-3,-1,5");
        assertEquals(c4,Algorithms.subsumedAndResolved(c4,clauses,id));
    }

    @Test
    public void subsumedID() throws Exception {
        System.out.println("subsumed with Implication DAG");
        ClauseList clauses = new ClauseList(10, 10);
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1, 5);
        id.addClause(-5, 6);
        Clause c1 = Utilities.makeClause("1", "1,3,5");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2", "8,6,3,7");
        assertNull(Algorithms.subsumedAndResolved(c2, clauses, id));
    }

    @Test
    public void subsumedID1() throws Exception {
        System.out.println("subsumed1 with Implication DAG");
        ClauseList clauses = new ClauseList(10, 10);
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1, 4);
        id.addClause(-1, 5);
        id.addClause(-2, 6);
        id.addClause(-3, 6);
        id.addClause(-3, 4);
        Clause c1 = Utilities.makeClause("1", "1,2,3");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2", "4,5,6,7,8,9");
        assertNull(Algorithms.subsumedAndResolved(c2, clauses, id));
    }




    @Test
    public void resolved() throws Exception {
        System.out.println("resolveBackwardLiterals");
        ClauseList clauses = new ClauseList(10, 10);
        ImplicationDAG id = new ImplicationDAG();
        Clause c1 = Utilities.makeClause("1","1,-3,5");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2","-1,-3,5");
        assertEquals("2: (-3,5)",Algorithms.subsumedAndResolved(c2,clauses,id).toString());
        Clause c3 = Utilities.makeClause("3","3,1,5");
        assertEquals("3: (1,5)",Algorithms.subsumedAndResolved(c3,clauses,id).toString());
        Clause c4 = Utilities.makeClause("4","-3,1,-5");
        assertEquals("4: (-3,1)",Algorithms.subsumedAndResolved(c4,clauses,id).toString());
        Clause c5 = Utilities.makeClause("5","-3,1,-5,6");
        assertEquals("5: (-3,1,6)",Algorithms.subsumedAndResolved(c5,clauses,id).toString());
        Clause c6 = Utilities.makeClause("6","-3,-1,-5,6");
        assertEquals("6: (-3,-1,-5,6)",Algorithms.subsumedAndResolved(c6,clauses,id).toString());
    }

    @Test
    public void resolvedMultiple() throws Exception {
        System.out.println("resolve multiple");
        ClauseList clauses = new ClauseList(10, 10);
        ImplicationDAG id = new ImplicationDAG();
        Clause c1 = Utilities.makeClause("1","1,3,5");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2","2,3,6");
        clauses.addClause(c2);
        Clause c3 = Utilities.makeClause("3","-1,-2,3,5,6");
        assertEquals("3: (3,5,6)",Algorithms.subsumedAndResolved(c3,clauses,id).toString());
    }

    @Test
    public void resolvedID() throws Exception {
        System.out.println("resolve with Implication DAG");
        ClauseList clauses = new ClauseList(10, 10);
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1,-2);
        id.addClause(-5,6);
        Clause c1 = Utilities.makeClause("1", "1,3,5");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2","2,3,6");
        assertEquals("2: (3,6)",Algorithms.subsumedAndResolved(c2,clauses,id).toString());
    }

    @Test
    public void resolvedID1() throws Exception {
        System.out.println("resolve1 with Implication DAG");
        ClauseList clauses = new ClauseList(10, 10);
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1,-7);
        id.addClause(-2,6);
        id.addClause(-3,8);
        Clause c1 = Utilities.makeClause("1", "1,2,3");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2","4,5,6,7,8");
        assertEquals("2: (4,5,6,8)",Algorithms.subsumedAndResolved(c2,clauses,id).toString());
    }



    @Test
    public void subsumes() throws Exception {
        System.out.println("subsumes");
        ClauseList clauses = new ClauseList(10, 10);
        ImplicationDAG id = new ImplicationDAG();
        Clause c1 = Utilities.makeClause("1", "1,2,3");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2", "3,2,1,4");
        clauses.addClause(c2);
        Clause c3 = Utilities.makeClause("3", "1,3,5");
        clauses.addClause(c3);

        Clause cs = Utilities.makeClause("s", "1,2,3");
        assertEquals("[2, 0]", Arrays.toString(Algorithms.subsumeAndResolve(cs,clauses,id)));
        assertEquals("3: (1,3,5)\n",clauses.toString());
    }

    @Test
    public void subsumesID() throws Exception {
        System.out.println("subsumes with Implication DAG");
        ClauseList clauses = new ClauseList(10, 10);
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1,2);
        id.addClause(-3,4);
        id.addClause(-5,6);
        Clause c1 = Utilities.makeClause("1", "1,3,6");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2", "3,6,7,8");
        clauses.addClause(c2);
        Clause c3 = Utilities.makeClause("3", "2,4,6");
        clauses.addClause(c3);

        Clause cs = Utilities.makeClause("s", "1,3,5");
        assertEquals("[2, 0]",Arrays.toString(Algorithms.subsumeAndResolve(cs,clauses,id)));
        assertEquals("2: (3,6,7,8)\n",clauses.toString());
    }

    @Test
    public void resolveFW() throws Exception {
        System.out.println("forward resolution");
        ClauseList clauses = new ClauseList(10, 10);
        ImplicationDAG id = new ImplicationDAG();
        Clause c1 = Utilities.makeClause("1", "-1,5,3");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2", "1,-3,6,5");
        clauses.addClause(c2);
        Clause cs = Utilities.makeClause("s", "1,3,5");
        assertEquals("[0, 2]", Arrays.toString(Algorithms.subsumeAndResolve(cs, clauses, id)));
        assertEquals("1: (5,3)\n" +
                "2: (1,6,5)\n",clauses.toString());
    }

    @Test
    public void resolveFWID() throws Exception {
        System.out.println("forward resolution with Implication DAG");
        ClauseList clauses = new ClauseList(10, 10);
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1,-4);
        id.addClause(-2,-5);
        id.addClause(-1,6);
        id.addClause(-3,8);
        id.addClause(-3,9);

        Clause c1 = Utilities.makeClause("1", "6,7,5,8");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2", "6,4,9");
        clauses.addClause(c2);
        Clause cs = Utilities.makeClause("s", "1,2,3");
        assertEquals("[0, 2]", Arrays.toString(Algorithms.subsumeAndResolve(cs, clauses, id)));
        assertEquals("1: (6,7,8)\n" +
                "2: (6,9)\n",clauses.toString());
    }

    @Test
    public void resolveFWID1() throws Exception {
        System.out.println("forward resolution1 with Implication DAG");
        ClauseList clauses = new ClauseList(10, 10);
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1,-4);
        id.addClause(-2,5);
        id.addClause(-2,6);
        id.addClause(-3,5);
        id.addClause(-3,8);

        Clause c1 = Utilities.makeClause("1", "4,5,6,7,8");
        clauses.addClause(c1);
        Clause cs = Utilities.makeClause("s", "1,2,3");
        assertEquals("[0, 1]", Arrays.toString(Algorithms.subsumeAndResolve(cs, clauses, id)));
        assertEquals("1: (6,7,8)\n",clauses.toString());
    }


    }