package Datastructures.Theory;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.function.BiConsumer;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 08.12.2019.
 */
public class TransformersTest {

    static BiConsumer<int[],Integer> contradictionHandler = ((clause, literal) -> {
        System.out.println("CL "+ Arrays.toString(clause));
        System.out.println("LI "+ literal);});

    static Symboltable symboltable = null;

    @Test
    public void prepareDisjunctions() throws Exception {
        System.out.println("disjunctions");
        BasicClauseList bcl = new BasicClauseList();
        ArrayList<Clause> clauses = new ArrayList<>();
        int[] c1 = new int[]{1,0,1,2,3};
        int[] c2 = new int[]{2,0,3,4,5};
        int[] c3 = new int[]{3,0,3,4,-3};
        int[] c4 = new int[]{4,0,3,4,3};
        bcl.addClause(c1);
        bcl.addClause(c2);
        bcl.addClause(c3);
        bcl.addClause(c4);
        int[] id = new int[]{0};
        Transformers.prepareDisjunctions(bcl,id,null,(clause->clauses.add(clause)));
        assertEquals("[1:(1,2,3), 2:(3,4,5), 4:(3,4)]",clauses.toString());
    }
    @Test
    public void prepareDisjunctionsEQ() throws Exception {
        System.out.println("disjunctions with equivalences");
        BasicClauseList bcl = new BasicClauseList();
        ArrayList<Clause> clauses = new ArrayList<>();
        int[] c1 = new int[]{1,4,3,2,1};
        int[] c2 = new int[]{2,0,3,4,5};
        int[] c3 = new int[]{3,0,3,4,-1};
        int[] c4 = new int[]{4,0,3,4,3};
        bcl.addClause(c2);
        bcl.addClause(c3);
        bcl.addClause(c4);
        EquivalenceClassesOld eq = new EquivalenceClassesOld(symboltable,null);
        eq.addEquivalenceClass(c1);
        int[] id = new int[]{0};
        Transformers.prepareDisjunctions(bcl,id,eq,(clause->clauses.add(clause)));
        assertEquals("[2:(1,4,5), 4:(1,4)]",clauses.toString());
    }

    @Test
    public void prepareConjunctions() throws Exception {
        System.out.println("conjunctions");
        BasicClauseList bcl = new BasicClauseList();
        ArrayList<Integer> clauses = new ArrayList<>();
        int[] c1 = new int[]{1,1,1,2,3};
        int[] c2 = new int[]{2,1,3,4,5};
        int[] c3 = new int[]{3,1,3,4,-3};
        int[] c4 = new int[]{4,1,3,4,3};
        bcl.addClause(c1);
        bcl.addClause(c2);
        bcl.addClause(c3);
        bcl.addClause(c4);
        Transformers.prepareConjunctions(bcl,null,(clause->clauses.add(clause)));
        assertEquals("[1, 2, 3, 3, 4, 5, 3, 4, -3, 3, 4, 3]",clauses.toString());}

    @Test
    public void prepareConjunctionsEQ() throws Exception {
        System.out.println("conjunctions with equivalence");
        BasicClauseList bcl = new BasicClauseList();
        ArrayList<Integer> clauses = new ArrayList<>();
        int[] c1 = new int[]{1,1,1,2,3};
        int[] c2 = new int[]{2,1,3,4,5};
        int[] c3 = new int[]{3,1,3,4,-3};
        int[] c4 = new int[]{4,1,3,4,3};
        bcl.addClause(c1);
        bcl.addClause(c2);
        bcl.addClause(c3);
        bcl.addClause(c4);
        int[] e1 = new int[]{1,4,1,2,3};
        EquivalenceClassesOld eq = new EquivalenceClassesOld(symboltable,null);
        eq.addEquivalenceClass(e1);
        Transformers.prepareConjunctions(bcl,eq,(clause->clauses.add(clause)));
        assertEquals("[1, 1, 1, 1, 4, 5, 1, 4, -1, 1, 4, 1]",clauses.toString());}


    @Test
    public void prepareXors() throws Exception {
        System.out.println("Xors");
        BasicClauseList bcl = new BasicClauseList();
        ArrayList<Clause> clauses = new ArrayList<>();
        int[] c1 = new int[]{1,2,1,2,3};
        bcl.addClause(c1);
        int[] id = new int[]{0};
        Transformers.prepareXors(bcl,id,null,(clause->clauses.add(clause)));
        assertEquals("[X1:(1,2,3), D1_1:(-1,-2), D1_2:(-1,-3), D1_3:(-2,-3)]",clauses.toString());
    }

    @Test
    public void prepareXorsEq() throws Exception {
        System.out.println("Xors with Equivalences");
        BasicClauseList bcl = new BasicClauseList();
        ArrayList<Clause> clauses = new ArrayList<>();
        int[] c1 = new int[]{1,2,1,2,3};
        bcl.addClause(c1);
        int[] c2 = new int[]{1,4,1,2};
        EquivalenceClassesOld eq = new EquivalenceClassesOld(symboltable,null);
        eq.addEquivalenceClass(c2);
        int[] id = new int[]{0};
        Transformers.prepareXors(bcl,id,eq,(clause->clauses.add(clause)));
        assertEquals("[X1:(1,3), D1_1:(-1), D1_2:(-1,-3), D1_3:(-1,-3)]",clauses.toString());
    }

    @Test
    public void prepareDisjoints() throws Exception {
        System.out.println("Disjoints");
        BasicClauseList bcl = new BasicClauseList();
        ArrayList<Clause> clauses = new ArrayList<>();
        int[] c1 = new int[]{1,3,1,2,3};
        bcl.addClause(c1);
        int[] id = new int[]{0};
        Transformers.prepareDisjoints(bcl,id,null,(clause->clauses.add(clause)));
        assertEquals("[D1_1:(-1,-2), D1_2:(-1,-3), D1_3:(-2,-3)]",clauses.toString());
    }

    @Test
    public void prepareDisjointsEq() throws Exception {
        System.out.println("Disjoints with Equivalences");
        BasicClauseList bcl = new BasicClauseList();
        ArrayList<Clause> clauses = new ArrayList<>();
        int[] c1 = new int[]{1,3,1,2,3};
        bcl.addClause(c1);
        int[] c2 = new int[]{1,4,1,2};
        EquivalenceClassesOld eq = new EquivalenceClassesOld(symboltable,null);
        eq.addEquivalenceClass(c2);
        int[] id = new int[]{0};
        Transformers.prepareDisjoints(bcl,id,eq,(clause->clauses.add(clause)));
        assertEquals("[D1_1:(-1), D1_2:(-1,-3), D1_3:(-1,-3)]",clauses.toString());
    }
    @Test
    public void prepareEquivalences() throws Exception {
        System.out.println("Equivalences");
        BasicClauseList bcl = new BasicClauseList();
        int[] c1 = new int[]{1,4,1,2,3};
        int[] c2 = new int[]{1,4,3,4,-5};
        bcl.addClause(c1);
        bcl.addClause(c2);
        EquivalenceClassesOld eq = Transformers.prepareEquivalences(bcl,null,null);
        assertEquals("Equivalence Classes:\n" +
                "1 = 2 = 3 = 4 = -5\n" +
                "Replacements:\n" +
                "-2 -> -1\n" +
                "2 -> 1\n" +
                "-3 -> -1\n" +
                "3 -> 1\n" +
                "-4 -> -1\n" +
                "4 -> 1\n" +
                "-5 -> 1\n" +
                "5 -> -1\n",eq.toString());

    }

}