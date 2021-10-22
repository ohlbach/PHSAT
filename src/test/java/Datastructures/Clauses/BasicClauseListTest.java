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
    public void falseDisjunctions() throws Exception {
        System.out.println("false disjunctions");
        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,0, 3,-2,1};
        int[] clause2 = {2,0,-3,1,2};
        clauses.addClause(clause1);
        clauses.addClause(clause2);
        assertEquals("Disjunctions:\n" +
                "   1 : 3,-2,1\n" +
                "   2 : -3,1,2\n",clauses.toString());

        Model model = new Model(10,null);
        model.add(-2); model.add(-1); model.add(3);
        ArrayList<int[]> falseClauses = clauses.falseClausesInModel(model);
        assertEquals(1,falseClauses.size());
        assertEquals(clause2,falseClauses.get(0));
    }

    @Test
    public void falseConjunctions() throws Exception {
        System.out.println("false conjunctions");
        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,1, 3,-2,1};
        int[] clause2 = {2,1, 4,1,3};
        clauses.addClause(clause1);
        clauses.addClause(clause2);
        assertEquals("Conjunctions:\n" +
                "   1 : 3&-2&1\n" +
                "   2 : 4&1&3\n",clauses.toString());

        Model model = new Model(10,null);
        model.add(-2); model.add(1); model.add(3);
        ArrayList<int[]> falseClauses = clauses.falseClausesInModel(model);
        assertEquals(1,falseClauses.size());
        assertEquals(clause2,falseClauses.get(0));
    }
    @Test
    public void falseEquivalent() throws Exception {
        System.out.println("false equivalent");
        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,2, 1,2,3};
        int[] clause2 = {2,2, 4,5,6};
        int[] clause3 = {3,2, 7,8,9};
        clauses.addClause(clause1);
        clauses.addClause(clause2);
        clauses.addClause(clause3);
        assertEquals("Equivalences:\n" +
                "   1 : 1=2=3\n" +
                "   2 : 4=5=6\n" +
                "   3 : 7=8=9\n",clauses.toString());
        Model model = new Model(10,null);
        model.add(1,2,3,-4,-5,-6,7,-8,9);
        ArrayList<int[]> falseClauses = clauses.falseClausesInModel(model);
        assertEquals(1,falseClauses.size());
        assertEquals(clause3,falseClauses.get(0));
    }

    @Test
    public void falseAtleast() throws Exception {
        System.out.println("false atleast");
        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,3,2, 3,-2,1};
        int[] clause2 = {2,3,2, 4,1,3};
        clauses.addClauses(clause1,clause2);
        assertEquals("Atleast:\n" +
                "   1 : ATLEAST 2 3,-2,1\n" +
                "   2 : ATLEAST 2 4,1,3\n",clauses.toString());

        Model model = new Model(10,null);
        model.add(-2,1);
        ArrayList<int[]> falseClauses = clauses.falseClausesInModel(model);
        assertEquals(1,falseClauses.size());
        assertEquals(clause2,falseClauses.get(0));
    }

    @Test
    public void falseAtmost() throws Exception {
        System.out.println("false atmost");
        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,4,2, 1,2,3};
        int[] clause2 = {2,4,2, 4,5,6};
        clauses.addClause(clause1);
        clauses.addClause(clause2);
        assertEquals("Atmost:\n" +
                "   1 : ATMOST 2 1,2,3\n" +
                "   2 : ATMOST 2 4,5,6\n",clauses.toString());

        Model model = new Model(10,null);
        model.add(1,2,3,4);
        ArrayList<int[]> falseClauses = clauses.falseClausesInModel(model);
        assertEquals(1,falseClauses.size());
        assertEquals(clause1,falseClauses.get(0));
    }

    @Test
    public void falseExactly() throws Exception {
        System.out.println("false exactly");
        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,5,2, 1,2,3};
        int[] clause2 = {2,5,2, 4,5,6};
        clauses.addClause(clause1);
        clauses.addClause(clause2);
        assertEquals("Exactly:\n" +
                "   1 : EXACTLY 2 1,2,3\n" +
                "   2 : EXACTLY 2 4,5,6\n",clauses.toString());

        Model model = new Model(10,null);
        model.add(1,2,3,4,6);
        ArrayList<int[]> falseClauses = clauses.falseClausesInModel(model);
        assertEquals(1,falseClauses.size());
        assertEquals(clause1,falseClauses.get(0));
    }


}