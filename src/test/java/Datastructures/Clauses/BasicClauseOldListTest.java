package Datastructures.Clauses;

import Datastructures.Theory.Model;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 03.09.2018.
 */
public class BasicClauseOldListTest {

    @Test
    public void removeDoubles()  {
        System.out.println("remove doubles");
        StringBuilder warnings = new StringBuilder();
        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,0, 1,2,3};
        int[] cl1 = clauses.removeDoubles(clause1,"",warnings);
        assertSame(cl1,clause1);
        int[] clause2 = {1,0, 1,3,2,1,3};
        int[] cl2 = clauses.removeDoubles(clause2,"test ",warnings);
        assertEquals("[1, 0, 1, 3, 2]", Arrays.toString(cl2));
        assertEquals("test double literals removed: [1, 3] (may be a typo) new clause: 1 : 1,3,2\n",warnings.toString());
        warnings = new StringBuilder();
        int[] clause3 = {3,3, 1,3,2,1,3};
        int[] cl3 = clauses.removeDoubles(clause3,"test ",warnings);
        assertEquals("[3, 3, 1, 3, 2, 1]", Arrays.toString(cl3));
        assertEquals("test double literals removed: [3] (may be a typo) new clause: 3 : ATLEAST 1 3,2,1\n",warnings.toString());
    }
    @Test
    public void checkSyntax() {
        System.out.println("checkSyntax");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        BasicClauseList clauses = new BasicClauseList();
        clauses.predicates = 5;
        int[] clause1 = {1, 0, 1, 2, 5};
        int[] cl1 = clauses.checkSyntax(clause1,"test: ",errors,warnings);
        assertSame(cl1,clause1);
        int[] clause2 = {2, 0, 1, 2, 6};
        int[] cl2 = clauses.checkSyntax(clause2,"test: ",errors,warnings);
        assertNull(cl2);
        assertEquals("test: Literal 6 is not within the predicate boundaries: 5\n",errors.toString());

        errors = new StringBuilder();
        int[] clause3 = {2, -1, 1, 2, 6};
        int[] cl3 = clauses.checkSyntax(clause3,"test: ",errors,warnings);
        assertNull(cl3);
        assertEquals("test: Clause type :-1 is not between 0 and 5\n",errors.toString());

        errors = new StringBuilder();
        int[] clause4 = {4, 4, 3, 2, 6};
        int[] cl4 = clauses.checkSyntax(clause4,"test: ",errors,warnings);
        assertNull(cl4);
        assertEquals("test: Quantifier: 3 is larger than the clause\n",errors.toString());

        errors = new StringBuilder();
        int[] clause5 = {4, 4, 0, 2, 6};
        int[] cl5 = clauses.checkSyntax(clause5,"test: ",errors,warnings);
        assertNull(cl5);
        assertEquals("test: Quantifier: 0 < 1\n",errors.toString());

        errors = new StringBuilder();
        int[] clause6 = {4, 4, 2, 2, 5};
        int[] cl6 = clauses.checkSyntax(clause6,"test: ",errors,warnings);
        assertSame(cl6,clause6);
    }

    @Test
    public void falseDisjunctions() throws Exception {
        System.out.println("false disjunctions");
        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,0, 3,-2,1};
        int[] clause2 = {2,0,-3,1,2};
        clauses.addClauses(clause1,clause2);
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
        clauses.addClauses(clause1,clause2);
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
        clauses.addClauses(clause1,clause2,clause3);
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
        clauses.addClauses(clause1,clause2);
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
        clauses.addClauses(clause1,clause2);
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