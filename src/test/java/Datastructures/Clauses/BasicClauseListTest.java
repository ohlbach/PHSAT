package Datastructures.Clauses;

import Datastructures.LocalModel;
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
                "1 o: 3,-2,1\n" +
                "2 o: -3,1,2\n",clauses.toString());

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
                "1 a: 3,-2,1\n" +
                "2 a: 4,1,3\n",clauses.toString());

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
                "1 x: 3,-2,1\n" +
                "2 x: 4,1,3\n",clauses.toString());

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
                "1 d: 3,-2,1\n" +
                "2 d: 4,5,3\n",clauses.toString());

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
                "1 e: 1,2,3\n" +
                "2 e: 4,5,6\n" +
                "3 e: 7,8,9\n",clauses.toString());

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
                "1 o: 1,2,3\n" +
                "2 o: 4,5,6\n" +
                "Conjunctions:\n" +
                "3 a: 7,8,9\n" +
                "4 a: 1,2,3\n" +
                "Xor:\n" +
                "5 x: 4,5,6\n" +
                "6 x: 7,8,9\n" +
                "Disjoints:\n" +
                "7 d: 1,2,3\n" +
                "8 d: 4,5,6\n" +
                "Equivalences:\n" +
                "9 e: 7,8,9\n",clauses.toString());

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
                "1 o: a,b\n" +
                "2 o: c,-d\n",clauses.toString(true));

    }

}