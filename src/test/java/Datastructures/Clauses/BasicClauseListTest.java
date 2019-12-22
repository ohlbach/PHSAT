package Datastructures.Clauses;

import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 03.09.2018.
 */
public class BasicClauseListTest {
    @Test
    public void disjunctions() throws Exception {
        System.out.println("disjunctions");
        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,0, 3,-2,1};
        int[] clause2 = {2,0,-3,1,2};
        clauses.addClause(clause1);
        clauses.addClause(clause2);
        assertEquals("Disjunctions:\n" +
                "1 : 3 | -2 | 1\n" +
                "2 : -3 | 1 | 2\n",clauses.toString());

        Model model = new Model(10);
        model.add(-2); model.add(-1); model.add(3);
        ArrayList<int[]> falseClauses = clauses.falseClauses(model);
        assertEquals(1,falseClauses.size());
        assertEquals(clause2,falseClauses.get(0));
    }

    @Test
    public void conjunctions() throws Exception {
        System.out.println("conjunctions");
        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,1, 3,-2,1};
        int[] clause2 = {2,1, 4,1,3};
        clauses.addClause(clause1);
        clauses.addClause(clause2);
        assertEquals("Conjunctions:\n" +
                "1 : 3 & -2 & 1\n" +
        "2 : 4 & 1 & 3\n",clauses.toString());

        Model model = new Model(10);
        model.add(-2); model.add(1); model.add(3);
        ArrayList<int[]> falseClauses = clauses.falseClauses(model);
        assertEquals(1,falseClauses.size());
        assertEquals(clause2,falseClauses.get(0));
    }

    @Test
    public void xor() throws Exception {
        System.out.println("xors");
        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,2, 3,-2,1};
        int[] clause2 = {2,2, 4,1,3};
        clauses.addClause(clause1);
        clauses.addClause(clause2);
        assertEquals("Xor:\n" +
                "1 : 3 x -2 x 1\n" +
                "2 : 4 x 1 x 3\n",clauses.toString());

        Model model = new Model(10);
        model.add(-2);
        ArrayList<int[]> falseClauses = clauses.falseClauses(model);
        assertEquals(1,falseClauses.size());
        assertEquals(clause2,falseClauses.get(0));
    }

    @Test
    public void disjoint() throws Exception {
        System.out.println("disjoint");
        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,3, 3,-2,1};
        int[] clause2 = {2,3, 4,5,3};
        clauses.addClause(clause1);
        clauses.addClause(clause2);
        assertEquals("Disjoints:\n" +
                "1 : 3 /= -2 /= 1\n" +
                "2 : 4 /= 5 /= 3\n",clauses.toString());

        Model model = new Model(10);
        model.add(-3); model.add(-2);model.add(-1);model.add(4);model.add(5);
        ArrayList<int[]> falseClauses = clauses.falseClauses(model);
        assertEquals(1,falseClauses.size());
        assertEquals(clause2,falseClauses.get(0));
    }

    @Test
    public void equivalent() throws Exception {
        System.out.println("equivalent");
        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,4, 1,2,3};
        int[] clause2 = {2,4, 4,5,6};
        int[] clause3 = {3,4, 7,8,9};
        clauses.addClause(clause1);
        clauses.addClause(clause2);
        clauses.addClause(clause3);
        assertEquals("Equivalences:\n" +
                "1 : 1 = 2 = 3\n" +
                "2 : 4 = 5 = 6\n" +
                "3 : 7 = 8 = 9\n",clauses.toString());

        Model model = new Model(10);
        model.add(1); model.add(2);model.add(3);model.add(-4);model.add(-5);model.add(-6);model.add(7);model.add(-8);model.add(9);
        ArrayList<int[]> falseClauses = clauses.falseClauses(model);
        assertEquals(1,falseClauses.size());
        assertEquals(clause3,falseClauses.get(0));
    }

    @Test
    public void mixed() throws Exception {
        System.out.println("mixed");
        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,0, 1,2,3};
        int[] clause2 = {2,0, 4,5,6};
        int[] clause3 = {3,1, 7,8,9};
        int[] clause4 = {4,1, 1,2,3};
        int[] clause5 = {5,2, 4,5,6};
        int[] clause6 = {6,2, 7,8,9};
        int[] clause7 = {7,3, 1,2,3};
        int[] clause8 = {8,3, 4,5,6};
        int[] clause9 = {9,4, 7,8,9};
        clauses.addClause(clause1);
        clauses.addClause(clause2);
        clauses.addClause(clause3);
        clauses.addClause(clause4);
        clauses.addClause(clause5);
        clauses.addClause(clause6);
        clauses.addClause(clause7);
        clauses.addClause(clause8);
        clauses.addClause(clause9);
        assertEquals("Disjunctions:\n" +
                "1 : 1 | 2 | 3\n" +
                "2 : 4 | 5 | 6\n" +
                "Conjunctions:\n" +
                "3 : 7 & 8 & 9\n" +
                "4 : 1 & 2 & 3\n" +
                "Xor:\n" +
                "5 : 4 x 5 x 6\n" +
                "6 : 7 x 8 x 9\n" +
                "Disjoints:\n" +
                "7 : 1 /= 2 /= 3\n" +
                "8 : 4 /= 5 /= 6\n" +
                "Equivalences:\n" +
                "9 : 7 = 8 = 9\n",clauses.toString());

    }

    @Test
    public void symboltable() throws Exception {
        System.out.println("symboltable");
        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,0, 1,2};
        int[] clause2 = {2,0, 3,-4};
        clauses.addClause(clause1);
        clauses.addClause(clause2);
        Symboltable symboltable = new Symboltable(10);
        symboltable.setName(1,"a");
        symboltable.setName(2,"b");
        symboltable.setName(3,"c");
        symboltable.setName(4,"d");
        clauses.symboltable = symboltable;

        assertEquals("Disjunctions:\n" +
                "1 : a | b\n" +
                "2 : c | -d\n",clauses.toString(true));

    }
    @Test
    public void disjunction() throws Exception {
        System.out.println("disjunction");
        Model m1 = new Model(10);
        int[] clause1 = {1, 0, 1, 2, 3}; // or
        assertFalse(BasicClauseList.disjunctionIsTrue(clause1, m1));
        m1.add(2);
        assertTrue(BasicClauseList.disjunctionIsTrue(clause1, m1));
        m1.add(1); m1.add(3);
        int[] clause2 = {1, 0, -1, -2, -3}; // or
        assertFalse(BasicClauseList.disjunctionIsTrue(clause2, m1));
    }

    @Test
    public void disjunctionIsFalse() throws Exception {
        System.out.println("disjunctionIsFalse");
        Model m1 = new Model(10);
        int[] clause1 = {1, 0, 1, 2, 3}; // or
        assertFalse(BasicClauseList.disjunctionIsFalse(clause1, m1));
        m1.add(-2);
        assertFalse(BasicClauseList.disjunctionIsFalse(clause1, m1));
        m1.add(-1);
        assertFalse(BasicClauseList.disjunctionIsFalse(clause1, m1));
        m1.add(-3);
        assertTrue(BasicClauseList.disjunctionIsFalse(clause1, m1));
        int[] clause2 = {1, 0, 10, 11, -10}; // or
        assertFalse(BasicClauseList.disjunctionIsFalse(clause2, m1));
    }


    @Test
    public void conjunction() throws Exception {
        System.out.println("conjunction");
        Model m1 = new Model(10);
        int[] clause1 = {1, 1, 1, 2, 3}; // and
        assertFalse(BasicClauseList.conjunctionIsTrue(clause1, m1));
        m1.add(2);m1.add(3);
        assertFalse(BasicClauseList.conjunctionIsTrue(clause1, m1));
        m1.add(1);
        assertTrue(BasicClauseList.conjunctionIsTrue(clause1, m1));
    }

    @Test
    public void conjunctionIsFalse() throws Exception {
        System.out.println("conjunctionIsFalse");
        Model m1 = new Model(30);
        int[] clause1 = {1, 1, 1, 2, 3}; // and
        assertFalse(BasicClauseList.conjunctionIsFalse(clause1, m1));
        m1.add(2);m1.add(3);
        assertFalse(BasicClauseList.conjunctionIsFalse(clause1, m1));
        m1.add(-1);
        assertTrue(BasicClauseList.conjunctionIsFalse(clause1, m1));
        int[] clause2 = {1, 1, 10, 11, -10}; // and
        assertTrue(BasicClauseList.conjunctionIsFalse(clause2, m1));
    }


    @Test
    public void xorTest() throws Exception {
        System.out.println("xor");
        Model m1 = new Model(10);
        int[] clause1 = {1, 2, 1, 2, 3}; // xor
        assertFalse(BasicClauseList.xorIsTrue(clause1, m1));
        m1.add(2);
        assertTrue(BasicClauseList.xorIsTrue(clause1, m1));
        m1.add(1);
        assertFalse(BasicClauseList.xorIsTrue(clause1, m1));
    }

    @Test
    public void xorIsFalse() throws Exception {
        System.out.println("xorIsFalse");
        Model m1 = new Model(30);
        int[] clause1 = {1, 2, 1, 2, 3}; // xor
        assertFalse(BasicClauseList.xorIsFalse(clause1, m1));
        m1.add(2);
        assertFalse(BasicClauseList.xorIsFalse(clause1, m1));
        m1.add(1);
        assertTrue(BasicClauseList.xorIsFalse(clause1, m1));
        int[] clause2 = {1, 2, 10, 11, -10,12}; // and
        assertFalse(BasicClauseList.xorIsFalse(clause2, m1));
        m1.add(11);
        m1.add(12);
        assertTrue(BasicClauseList.xorIsFalse(clause2, m1));

    }



    @Test
    public void disjointTest() throws Exception {
        System.out.println("disjoint");
        Model m1 = new Model(10);
        int[] clause1 = {1, 3, 1, 2, 3}; // disjoint
        assertTrue(BasicClauseList.disjointIsTrue(clause1, m1));
        m1.add(2);
        assertTrue(BasicClauseList.disjointIsTrue(clause1, m1));
        m1.add(1);
        assertFalse(BasicClauseList.disjointIsTrue(clause1, m1));
    }
    @Test
    public void disjointIsFalse() throws Exception {
        System.out.println("disjointIsFalse");
        Model m1 = new Model(30);
        int[] clause1 = {1, 3, 1, 2, 3}; // disjoint
        assertFalse(BasicClauseList.disjointIsFalse(clause1, m1));
        m1.add(2);
        assertFalse(BasicClauseList.disjointIsFalse(clause1, m1));
        m1.add(1);
        assertTrue(BasicClauseList.disjointIsFalse(clause1, m1));
        int[] clause2 = {1, 3, 10, 11, -10,12}; // disjoint
        assertFalse(BasicClauseList.disjointIsFalse(clause2, m1));
        m1.add(11);
        m1.add(12);
        assertTrue(BasicClauseList.disjointIsFalse(clause2, m1));

    }


    @Test
    public void equivalenceTest() throws Exception {
        System.out.println("equivalence");
        Model m1 = new Model(10);
        int[] clause1 = {1, 4, 1, 2, 3}; // equivalence
        assertFalse(BasicClauseList.equivalenceIsTrue(clause1, m1));
        m1.add(2);
        assertFalse(BasicClauseList.equivalenceIsTrue(clause1, m1));
        m1.add(1);m1.add(3);
        assertTrue(BasicClauseList.equivalenceIsTrue(clause1, m1));

        m1 = new Model(10);
        m1.add(-1); m1.add(-2); m1.add(-3);
        assertTrue(BasicClauseList.equivalenceIsTrue(clause1, m1));
    }

    @Test
    public void equivalenceIsFalse() throws Exception {
        System.out.println("equivalenceIsFalse");
        Model m1 = new Model(10);
        int[] clause1 = {1, 4, 1, 2, 3}; // equivalence
        assertFalse(BasicClauseList.equivalenceIsFalse(clause1, m1));
        m1.add(2);
        assertFalse(BasicClauseList.equivalenceIsFalse(clause1, m1));
        m1.add(1);m1.add(3);
        assertFalse(BasicClauseList.equivalenceIsFalse(clause1, m1));

        m1 = new Model(20);
        m1.add(1); m1.add(2); m1.add(-3);
        assertTrue(BasicClauseList.equivalenceIsFalse(clause1, m1));

        int[] clause2 = {1, 4, 10, 11, -10}; // equivalence
        assertTrue(BasicClauseList.equivalenceIsFalse(clause2, m1));

    }





    @Test
    public void falseClauses() throws Exception {
        System.out.println("falseClauses");
        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,0, 1,2,3}; // or
        int[] clause2 = {2,0, 4,5,6}; // or
        int[] clause3 = {3,1, 7,8,9}; // and
        int[] clause4 = {4,1, 1,2,3}; // and
        int[] clause5 = {5,2, 4,5,6}; // xor
        int[] clause6 = {6,2, 7,8,9}; // xor
        int[] clause7 = {7,3, 1,2,3}; // dis
        int[] clause8 = {8,3, 4,5,6}; // dis
        int[] clause9 = {9,4, 7,8,9}; // eqv
        clauses.addClauses(clause1,clause2,clause3,clause4,clause5,clause6,clause7,clause8,clause9);
        //System.out.println(clauses.toString());
        Model m1 = new Model(10);
        m1.add(2);
        assertTrue(clauses.disjunctionIsTrue(clause1,m1));
        assertFalse(clauses.disjunctionIsTrue(clause2,m1));
        assertFalse(clauses.conjunctionIsTrue(clause3,m1));
        StringBuilder st = new StringBuilder();
        for(int[] clause : clauses.falseClauses(m1)) {st.append(BasicClauseList.clauseToString(2,clause,null));}
        assertEquals(" 2 : 4 | 5 | 6 3 : 7 & 8 & 9 4 : 1 & 2 & 3 5 : 4 x 5 x 6 6 : 7 x 8 x 9 9 : 7 = 8 = 9",st.toString());
        m1.add(7); m1.add(8);m1.add(9);
        st = new StringBuilder();
        for(int[] clause : clauses.falseClauses(m1)) {st.append(BasicClauseList.clauseToString(2,clause,null));}
        assertEquals(" 2 : 4 | 5 | 6 4 : 1 & 2 & 3 5 : 4 x 5 x 6 6 : 7 x 8 x 9",st.toString());




    }


}