package Datastructures.Clauses;
import Datastructures.Literals.CLiteral;
import Datastructures.Theory.ImplicationDAG;
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
        assertEquals(c12,clauses.getLiterals(6).peek().clause);
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
        clauses.addLiteralRemovalObserver(cl -> st.append(cl.clause.toString()));
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
        clauses.addLiteralRemovalObserver(cl -> st.append(cl.clause.toString()));
        clauses.makeTrue(3);
        assertEquals("1: (1,6)\n" +
                "2: (6,-7)\n" +
                "4: (4,6,-7)\n",clauses.toString());
        assertEquals("1: (1,6)2: (6,-7)",st.toString());
    }

    @Test
    public void getOccurrences() throws Exception {
        System.out.println("getOccurrences");
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
        assertEquals(1,clauses.getOccurrences(1));
        assertEquals(0,clauses.getOccurrences(-1));
        assertEquals(2,clauses.getOccurrences(3));
        assertEquals(2,clauses.getOccurrences(-3));
    }

    @Test
    public void isPure() throws Exception {
        System.out.println("isPure");
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
        assertTrue(clauses.isPure(1));
        assertFalse(clauses.isPure(-1));
        assertTrue(clauses.isPure(6));
        assertFalse(clauses.isPure(7));
    }

    @Test
    public void replaceByRepresentative() throws Exception {
        System.out.println("replaceByRepresentative");
        ClauseList clauses = new ClauseList(10, 10);
        Clause c1 = Utilities.makeClause("1", "1,-3,6");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2", "3,6,-7");
        clauses.addClause(c2);
        Clause c3 = Utilities.makeClause("3", "2,3,7");
        clauses.addClause(c3);
        Clause c4 = Utilities.makeClause("4", "4,6,-7");
        clauses.addClause(c4);
        Clause c5 = Utilities.makeClause("5", "3,8,7");
        clauses.addClause(c5);
        StringBuilder st1 = new StringBuilder();
        StringBuilder st2 = new StringBuilder();
        StringBuilder st3 = new StringBuilder();
        clauses.addLiteralRemovalObserver(lit-> st1.append("LR " +lit.clause.toString()+"\n"));
        clauses.addClauseRemovalObserver(cls-> st2.append("CR " +cls.toString()+"\n"));
        clauses.addLiteralReplacementObserver((lit,state) -> st3.append("RP " + lit.clause.toString()+ " " + state + "\n"));
        clauses.replaceByRepresentative(6,3);
        assertEquals("LR 2: (6,-7)\n",st1.toString());
        assertEquals("CR 1: (1,-3,6)\n",st2.toString());
        assertEquals("RP 3: (2,3,7) true\n" +
                "RP 3: (2,6,7) false\n" +
                "RP 5: (3,8,7) true\n" +
                "RP 5: (6,8,7) false\n",st3.toString());

    }

    @Test
    public void applyTest() throws Exception {
        System.out.println("apply");
        ClauseList clauses = new ClauseList(10, 10);
        Clause c1 = Utilities.makeClause("1", "1,-3,6");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2", "3,6,-7");
        clauses.addClause(c2);
        Clause c3 = Utilities.makeClause("3", "2,3,7");
        clauses.addClause(c3);
        Clause c4 = Utilities.makeClause("4", "4,6,-7");
        clauses.addClause(c4);
        Clause c5 = Utilities.makeClause("5", "3,8,7");
        clauses.addClause(c5);
        ImplicationDAG id = new ImplicationDAG();
        StringBuilder st = new StringBuilder();
        clauses.apply(1,id,true, (lit -> st.append(lit.clause.toString()+"\n")));
        assertEquals("1: (1,-3,6)\n",st.toString());
        clauses.apply(3,id,true, (lit -> st.append(lit.clause.toString()+"\n")));
        assertEquals("1: (1,-3,6)\n" +
                "2: (3,6,-7)\n" +
                "3: (2,3,7)\n" +
                "5: (3,8,7)\n",st.toString());

        id.addClause(-1,3);
        id.addClause(-3,4);
        StringBuilder st2 = new StringBuilder();
        clauses.apply(1,id,true, (lit -> st2.append(lit.clause.toString()+"\n")));
        assertEquals("1: (1,-3,6)\n" +
                "2: (3,6,-7)\n" +
                "3: (2,3,7)\n" +
                "5: (3,8,7)\n" +
                "4: (4,6,-7)\n",st2.toString());
        StringBuilder st3 = new StringBuilder();
        clauses.apply(4,id,false, (lit -> st3.append(lit.clause.toString()+"\n")));
        assertEquals("4: (4,6,-7)\n" +
                "2: (3,6,-7)\n" +
                "3: (2,3,7)\n" +
                "5: (3,8,7)\n" +
                "1: (1,-3,6)\n",st3.toString());
    }

    @Test
    public void applyContradicting() throws Exception {
        System.out.println("applyContradiciting");
        ClauseList clauses = new ClauseList(10, 10);
        Clause c1 = Utilities.makeClause("1", "1,-3,6");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2", "3,6,-7");
        clauses.addClause(c2);
        Clause c3 = Utilities.makeClause("3", "2,3,7");
        clauses.addClause(c3);
        Clause c4 = Utilities.makeClause("4", "4,6,-7");
        clauses.addClause(c4);
        Clause c5 = Utilities.makeClause("5", "-2,8,7");
        clauses.addClause(c5);
        ImplicationDAG id = new ImplicationDAG();
        StringBuilder st = new StringBuilder();
        clauses.applyContradicting(3, id, (lit -> st.append(lit.clause.toString() + "\n")));
        assertEquals("1: (1,-3,6)\n", st.toString());

        id.addClause(-2,-3);
        StringBuilder st1 = new StringBuilder();
        clauses.applyContradicting(2, id, (lit -> st1.append(lit.clause.toString() + "\n")));
        assertEquals("5: (-2,8,7)\n" +
                "2: (3,6,-7)\n" +
                "3: (2,3,7)\n",st1.toString());
    }

    @Test
    public void streamTest() throws Exception {
        System.out.println("stream");
        ClauseList clauses = new ClauseList(10, 10);
        Clause c1 = Utilities.makeClause("1", "1,-3,5");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2", "3,6,-7");
        clauses.addClause(c2);
        Clause c3 = Utilities.makeClause("3", "2,3,7");
        clauses.addClause(c3);
        Clause c4 = Utilities.makeClause("4", "4,6,-7");
        clauses.addClause(c4);
        Clause c5 = Utilities.makeClause("5", "3,8,7");
        clauses.addClause(c5);
        ImplicationDAG id = new ImplicationDAG();
        StringBuilder st = new StringBuilder();
        clauses.stream(3,id,true).forEach(lit->st.append(lit.clause.toString()+"\n"));
        assertEquals("2: (3,6,-7)\n" +
                "3: (2,3,7)\n" +
                "5: (3,8,7)\n",st.toString());

        id.addClause(-1,6);
        StringBuilder st1 = new StringBuilder();
        clauses.stream(1,id,true).forEach(lit->st1.append(lit.clause.toString()+"\n"));
        assertEquals("1: (1,-3,5)\n" +
                "2: (3,6,-7)\n" +
                "4: (4,6,-7)\n",st1.toString());
        StringBuilder st2 = new StringBuilder();
        clauses.stream(6,id,false).forEach(lit->st2.append(lit.clause.toString()+"\n"));
        assertEquals("2: (3,6,-7)\n" +
                "4: (4,6,-7)\n" +
                "1: (1,-3,5)\n",st2.toString());
    }

    @Test
    public void streamContradicting() throws Exception {
        System.out.println("streamContradicting");
        ClauseList clauses = new ClauseList(10, 10);
        Clause c1 = Utilities.makeClause("1", "1,-3,5");
        clauses.addClause(c1);
        Clause c2 = Utilities.makeClause("2", "3,6,-7");
        clauses.addClause(c2);
        Clause c3 = Utilities.makeClause("3", "2,3,7");
        clauses.addClause(c3);
        Clause c4 = Utilities.makeClause("4", "4,6,-7");
        clauses.addClause(c4);
        Clause c5 = Utilities.makeClause("5", "3,8,7");
        clauses.addClause(c5);
        ImplicationDAG id = new ImplicationDAG();
        StringBuilder st = new StringBuilder();
        clauses.streamContradicting(3, id).forEach(lit -> st.append(lit.clause.toString() + "\n"));
        assertEquals("1: (1,-3,5)\n",st.toString());

        id.addClause(-1,-6);
        StringBuilder st1 = new StringBuilder();
        clauses.streamContradicting(1, id).forEach(lit -> st1.append(lit.clause.toString() + "\n"));
        assertEquals("2: (3,6,-7)\n" +
                "4: (4,6,-7)\n",st1.toString());
    }


    @Test
    public void groups() throws Exception {
        System.out.println("groups");
        ClauseList clauses = new ClauseList(10, Clause.sizeComparator,Clause.priorityComparator);
        Clause c1 = Utilities.makeClause("1", "1,-3,5");
        Clause c2 = Utilities.makeClause("2", "-5,6");
        clauses.addClause(c1,0);
        clauses.addClause(c2,0);
        Clause c3 = Utilities.makeClause("3", "5,6"); c3.listPosition = 5;
        Clause c4 = Utilities.makeClause("4", "6,7,8"); c4.listPosition = 0;
        clauses.addClause(c3,1);
        clauses.addClause(c4,1);
        assertEquals("Clause group 0\n" +
                "2: (-5,6)\n" +
                "1: (1,-3,5)\n" +
                "Clause group 1\n" +
                "4: (6,7,8)\n" +
                "3: (5,6)\n\n",clauses.toString());
        assertEquals("[4: (6,7,8), 3: (5,6)]",clauses.getClauses(1).toString());
        assertEquals("[5, 5]",clauses.getLiterals(5).toString());
    }
    }