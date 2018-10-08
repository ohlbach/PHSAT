package Algorithms;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Theory.ImplicationDAG;
import Utilities.Utilities;
import org.junit.Test;

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
        assertTrue(Algorithms.isSubsumed(c2,clauses,id));
        Clause c3 = Utilities.makeClause("3","-3,2,5");
        assertFalse(Algorithms.isSubsumed(c3,clauses,id));
        Clause c4 = Utilities.makeClause("4","-3,-1,5");
        assertFalse(Algorithms.isSubsumed(c4,clauses,id));
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
        assertTrue(Algorithms.isSubsumed(c2, clauses, id));
    }


    @Test
    public void resolved() throws Exception {
        System.out.println("resolveBackwardLiterals");
        ClauseList clauses = new ClauseList(10, 10);
        ImplicationDAG id = new ImplicationDAG();
        Clause c1 = Utilities.makeClause("1","1,-3,5");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2","-1,-3,5");
        assertEquals("2: (-3,5)",Algorithms.resolveBackwardLiterals(c2,clauses,id).toString());
        Clause c3 = Utilities.makeClause("3","3,1,5");
        assertEquals("3: (1,5)",Algorithms.resolveBackwardLiterals(c3,clauses,id).toString());
        Clause c4 = Utilities.makeClause("4","-3,1,-5");
        assertEquals("4: (-3,1)",Algorithms.resolveBackwardLiterals(c4,clauses,id).toString());
        Clause c5 = Utilities.makeClause("5","-3,1,-5,6");
        assertEquals("5: (-3,1,6)",Algorithms.resolveBackwardLiterals(c5,clauses,id).toString());
        Clause c6 = Utilities.makeClause("6","-3,-1,-5,6");
        assertEquals("6: (-3,-1,-5,6)",Algorithms.resolveBackwardLiterals(c6,clauses,id).toString());
    }

    @Test
    public void resolvedMultiple() throws Exception {
        System.out.println("resolveBackwardLiterals multiple");
        ClauseList clauses = new ClauseList(10, 10);
        ImplicationDAG id = new ImplicationDAG();
        Clause c1 = Utilities.makeClause("1","1,3,5");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2","2,3,6");
        clauses.addClause(c2);
        Clause c3 = Utilities.makeClause("3","-1,-2,3,5,6");
        assertEquals("3: (3,5,6)",Algorithms.resolveBackwardLiterals(c3,clauses,id).toString());
    }

    @Test
    public void resolvedID() throws Exception {
        System.out.println("resolveBackwardLiterals with Implication DAG");
        ClauseList clauses = new ClauseList(10, 10);
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1,-2);
        id.addClause(-5,6);
        Clause c1 = Utilities.makeClause("1", "1,3,5");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2","2,3,6");
        clauses.addClause(c2);
        assertEquals("2: (3,6)",Algorithms.resolveBackwardLiterals(c2,clauses,id).toString());
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
        assertEquals(2,Algorithms.subsumes(cs,clauses,id));
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
        assertEquals(2,Algorithms.subsumes(cs,clauses,id));
        assertEquals("2: (3,6,7,8)\n",clauses.toString());
    }

    }