package Datastructures.Theory;

import org.junit.Test;
import org.omg.CORBA.IMP_LIMIT;

import java.util.Arrays;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 26.09.2018.
 */
public class ImplicationDAGTest {

    @Test
    public void addImplication() throws Exception {
        System.out.println("addImplication");
        ImplicationDAG id = new ImplicationDAG();
        id.addImplication(1,2);
        assertEquals("1 -> 2\n",id.toString());
        id.addImplication(1,3);
        assertEquals("1 -> 2,3\n",id.toString());
        id.addImplication(2,4);
        assertEquals("1 -> 2,3\n" +
                "     2 -> 4\n",id.toString());
        id.addImplication(3,4);
        assertEquals("1 -> 2,3\n" +
                "     2 -> 4\n" +
                "     3 -> 4\n",id.toString());
        id.addImplication(4,5);
        assertEquals("1 -> 2,3\n" +
                "     2 -> 4\n" +
                "          4 -> 5\n" +
                "     3 -> 4\n" +
                "          4 -> 5\n",id.toString());
        id.addImplication(1,5);
        assertEquals("1 -> 2,3\n" +
                "     2 -> 4\n" +
                "          4 -> 5\n" +
                "     3 -> 4\n" +
                "          4 -> 5\n",id.toString());
    }
    @Test
    public void addClause() throws Exception {
        System.out.println("addClause");
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(1,2);
        assertEquals("-2 -> 1\n" +
                "-1 -> 2\n",id.toString());
        id.addClause(-2,3);
        assertEquals("-3 -> -2\n" +
                "     -2 -> 1\n" +
                "-1 -> 2\n" +
                "     2 -> 3\n",id.toString());
        id.addClause(-1,4);
        assertEquals("-4 -> -1\n" +
                "     -1 -> 2\n" +
                "          2 -> 3\n" +
                "-3 -> -2\n" +
                "     -2 -> 1\n" +
                "          1 -> 4\n",id.toString());
    }

    @Test
    public void newUnits1() throws Exception {
        System.out.println("new Units1");
        ImplicationDAG id = new ImplicationDAG();
        StringBuilder st = new StringBuilder();
        id.addTrueLiteralObserver(lit-> st.append(lit+" "));
        id.addClause(1, 2);
        id.addClause(1,-2);
        assertTrue(id.isEmpty());

        StringBuilder st1 = new StringBuilder();
        id.addTrueLiteralObserver(lit-> st1.append(lit+" "));
        id.addClause(-3,4);
        id.addClause(-4,5);
        id.addClause(-5,6);
        id.addClause(-4,-5);
        assertEquals("-6 -> -5\n" +
                "5 -> 6\n",id.toString());
        assertEquals("-4 -3 ",st1.toString());
    }

    @Test
    public void newUnits2() throws Exception {
        System.out.println("new Units2");
        ImplicationDAG id = new ImplicationDAG();
        StringBuilder st = new StringBuilder();
        id.addTrueLiteralObserver(lit -> st.append(lit + " "));
        id.addClause(-1, 2);
        id.addClause(-2, 3);
        id.addClause(-3, 4);
        id.addClause(-2, 5);
        id.addClause(-5, 6);
        assertEquals("-6 -> -5\n" +
                "     -5 -> -2\n" +
                "          -2 -> -1\n" +
                "-4 -> -3\n" +
                "     -3 -> -2\n" +
                "          -2 -> -1\n" +
                "1 -> 2\n" +
                "     2 -> 3,5\n" +
                "          3 -> 4\n" +
                "          5 -> 6\n",id.toString());
        id.addClause(-2, -4);
        assertEquals("-6 -> -5\n" +
                "-4 -> -3\n" +
                "3 -> 4\n" +
                "5 -> 6\n",id.toString());
        assertEquals("-2 -1 ",st.toString());
            }

    @Test
    public void equivalence1() throws Exception {
        System.out.println("equivalence1");
        ImplicationDAG id = new ImplicationDAG();
        StringBuilder st = new StringBuilder();
        id.addEquivalenceObserver(lits -> st.append(Arrays.toString(lits)));
        id.addClause(-1,2);
        id.addClause(-2,1);
        assertEquals("[1, 2]",st.toString());
        assertTrue(id.isEmpty());
    }
    @Test
    public void equivalence2() throws Exception {
        System.out.println("equivalence2");
        ImplicationDAG id = new ImplicationDAG();
        StringBuilder st = new StringBuilder();
        id.addEquivalenceObserver(lits -> st.append(Arrays.toString(lits)));
        id.addClause(-1, 3);
        id.addClause(-2, 3);
        id.addClause(-3, 4);
        id.addClause(-4, 5);
        id.addClause(-5, 6);
        id.addClause(-5, 7);
        id.addClause(-8, 4);
        id.addClause(-5, 3);
        assertEquals("[3, 4, 5]",st.toString());
        assertEquals("-7 -> -3\n" +
                "     -3 -> -1,-2,-8\n" +
                "-6 -> -3\n" +
                "     -3 -> -1,-2,-8\n" +
                "1 -> 3\n" +
                "     3 -> 6,7\n" +
                "2 -> 3\n" +
                "     3 -> 6,7\n" +
                "8 -> 3\n" +
                "     3 -> 6,7\n",id.toString());
    }


    @Test
    public void implies() throws Exception {
        System.out.println("implies");
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1, 3);
        id.addClause(-2, 3);
        id.addClause(-3, 4);
        id.addClause(-4, 5);
        id.addClause(-5, 6);
        id.addClause(-5, 7);
        id.addClause(-8, 4);
        assertTrue(id.implies(1,6));
        assertFalse(id.implies(6,1));
        assertTrue(id.implies(-6,-1));
        assertFalse(id.implies(8,1));
    }

    @Test
    public void roots() throws Exception {
        System.out.println("roots");
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1, 3);
        id.addClause(-2, 3);
        id.addClause(-3, 4);
        id.addClause(-4, 5);
        id.addClause(-5, 6);
        id.addClause(-5, 7);
        id.addClause(-8, 4);
        StringBuilder st1 = new StringBuilder();
        id.applyToRoots((l -> st1.append(l).append(" ")));
        assertEquals("-7 -6 1 2 8 ",st1.toString());

    }




    @Test
    public void newTrueLiteral() throws Exception {
        System.out.println("newTrueLiteral");
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1, 3);
        id.addClause(-2, 3);
        id.addClause(-3, 4);
        id.addClause(-4, 5);
        id.addClause(-5, 6);
        id.addClause(-5, 7);
        id.addClause(-8, 4);
        id.newTrueLiteral(5);
        assertEquals("-4 -> -3,-8\n" +
                "     -3 -> -1,-2\n" +
                "1 -> 3\n" +
                "     3 -> 4\n" +
                "2 -> 3\n" +
                "     3 -> 4\n" +
                "8 -> 4\n",id.toString());

    }

    @Test
    public void apply() throws Exception {
        System.out.println("apply");
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1, 3);
        id.addClause(-2, 3);
        id.addClause(-3, 4);
        id.addClause(-4, 5);
        id.addClause(-5, 6);
        id.addClause(-5, 7);
        id.addClause(-8, 4);
        StringBuilder st1 = new StringBuilder();
        id.apply(1,true,(l -> st1.append(l).append(" ")));
        assertEquals("1 3 4 5 6 7 ",st1.toString());
        StringBuilder st2 = new StringBuilder();
        id.apply(6,false,(l -> st2.append(l).append(" ")));
        assertEquals("6 5 4 3 1 2 8 ",st2.toString());
        StringBuilder st3 = new StringBuilder();
        id.apply(-4,true,(l -> st3.append(l).append(" ")));
        assertEquals("-4 -3 -1 -2 -8 ",st3.toString());
    }

    @Test
    public void applyToImplications() throws Exception {
        System.out.println("applyToImplications");
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1, 3);
        id.addClause(-2, 3);
        id.addClause(-3, 4);
        id.addClause(-4, 5);
        id.addClause(-5, 6);
        id.addClause(-5, 7);
        id.addClause(-8, 4);
        StringBuilder st1 = new StringBuilder();
        id.applyToImplications(4,5,((l1,l2) -> st1.append("("+l1 + "," + l2+")")));
        assertEquals("(4,5)(4,6)(4,7)(3,5)(3,6)(3,7)(1,5)(1,6)(1,7)(2,5)(2,6)(2,7)(8,5)(8,6)(8,7)",st1.toString());
    }
    @Test
    public void implicationObserver() throws Exception {
        System.out.println("implication observer");
        ImplicationDAG id = new ImplicationDAG();
        StringBuilder st = new StringBuilder();
        id.addImplicationObserver(((l1,l2) -> st.append("("+l1 + "," + l2+")")));
        id.addClause(-1, 3);
        id.addClause(-2, 3);
        id.addClause(-3, 4);
        id.addClause(-4, 5);
        id.addClause(-5, 6);
        id.addClause(-5, 7);
        id.addClause(-8, 4);
        assertEquals("(1,3)(-3,-1)(2,3)(-3,-2)(3,4)(-4,-3)(4,5)(-5,-4)(5,6)(-6,-5)(5,7)(-7,-5)(8,4)(-4,-8)",st.toString());

    }

    @Test
    public void rootLiterals() throws Exception {
        System.out.println("rootLiterals");
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1, 3);
        id.addClause(-2, 3);
        id.addClause(-3, 4);
        id.addClause(-4, 5);
        id.addClause(-5, 6);
        id.addClause(-5, 7);
        id.addClause(-8, 4);
        assertEquals("[1, 2, 8]",id.rootLiterals(6).toString());

    }
    @Test
    public void cloneTest() throws Exception {
        System.out.println("clone");
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1, 3);
        id.addClause(-2, 3);
        id.addClause(-3, 4);
        id.addClause(-4, 5);
        id.addClause(-5, 6);
        id.addClause(-5, 7);
        id.addClause(-8, 4);
        ImplicationDAG id1 = id.clone();
        assertEquals(id.toString(),id1.toString());
    }
    @Test
    public void completeModel() throws Exception {
        System.out.println("completeModel");
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1, 3);
        id.addClause(-2, 3);
        id.addClause(-3, 4);
        id.addClause(-4, 5);
        id.addClause(-5, 6);
        id.addClause(-5, 7);
        id.addClause(-8, 4);
        Model model = new Model(10);
        id.completeModel(model);
        assertEquals("[4, 5, 6, 7, 3]",model.toString());
    }

    @Test
    public void completeModel1() throws Exception {
        System.out.println("completeModel1");
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1, 3);
        id.addClause(-2, 5);
        id.addClause(-3, 4);
        id.addClause(-4, 5);
        id.addClause(-5, 6);
        id.addClause(-5, 7);
        id.addClause(-8, 4);
        Model model = new Model(10);
        id.completeModel(model);
        assertEquals("[4, 5, 6, 7, 3]",model.toString());}
    }