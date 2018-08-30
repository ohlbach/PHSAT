package Datastructures.Theory;

import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 30.08.2018.
 */
public class ImplicationGraphTest {
    @Test
    public void addClause1() throws Exception {
        System.out.println("add single clause");
        ImplicationGraph ig = new ImplicationGraph(10);
        ig.addClause(1,2);
        assertEquals("[2]",ig.getImplicants(-1).toString());
        assertEquals("[1]",ig.getImplicants(-2).toString());
        assertNull(ig.getImplicants(1));
        assertNull(ig.getImplicants(2));
        //System.out.println(ig);
    }
    @Test
    public void addClause2() throws Exception {
        System.out.println("add two clauses");
        ImplicationGraph ig = new ImplicationGraph(1000);
        ig.addClause(200, 300);
        ig.addClause(200, 5);
        assertEquals("[200]",ig.getImplicants(-5).toString());
        assertEquals("[300, 5]",ig.getImplicants(-200).toString());
        assertEquals("[200]",ig.getImplicants(-300).toString());
        //System.out.println(ig);
    }

    @Test
    public void addClause3() throws Exception {
        System.out.println("add  single sequence");
        ImplicationGraph ig = new ImplicationGraph(10);
        ig.addClause(1,2);
        ig.addClause(-2,3);
        //System.out.println(ig);
        assertEquals("[2, 3]",ig.getImplicants(-1).toString());
        assertEquals("[3]",ig.getImplicants(2).toString());
        assertEquals("[1]",ig.getImplicants(-2).toString());
        assertEquals("[-2, 1]",ig.getImplicants(-3).toString());
        assertNull(ig.getImplicants(1));
        assertNull(ig.getImplicants(3));
    }

    @Test
    public void addClause4() throws Exception {
        System.out.println("add two sequences");
        ImplicationGraph ig = new ImplicationGraph(10);
        ig.addClause(1,2);
        ig.addClause(-2,3);
        ig.addClause(-2,4);
        //System.out.println(ig);
        assertEquals("[2, 3, 4]",ig.getImplicants(-1).toString());
        assertEquals("[3, 4]",ig.getImplicants(2).toString());
        assertEquals("[1]",ig.getImplicants(-2).toString());
        assertEquals("[-2, 1]",ig.getImplicants(-3).toString());
        assertEquals("[-2, 1]",ig.getImplicants(-4).toString());
        assertNull(ig.getImplicants(1));
        assertNull(ig.getImplicants(3));
    }

    @Test
    public void addClause5() throws Exception {
        System.out.println("add contradiction");
        ImplicationGraph ig = new ImplicationGraph(10);
        assertNull(ig.addClause(1,2));
        assertEquals("[1]",ig.addClause(1,-2).toString());
        //System.out.println(ig);
        assertNull(ig.getImplicants(1));
        assertNull(ig.getImplicants(-1));
    }

    @Test
    public void addClause6() throws Exception {
        System.out.println("add contradiction succ 1");
        ImplicationGraph ig = new ImplicationGraph(10);
        assertNull(ig.addClause(1,2));
        assertEquals("[2]",ig.addClause(2,-1).toString());
        System.out.println(ig);
        assertNull(ig.getImplicants(1));
        assertNull(ig.getImplicants(-1));
    }

    @Test
    public void addClause7() throws Exception {
        System.out.println("add contradiction succ 2");
        ImplicationGraph ig = new ImplicationGraph(10);
        assertNull(ig.addClause(1,2));
        assertEquals("[2]",ig.addClause(-1,2).toString());
        System.out.println(ig);
        assertNull(ig.getImplicants(1));
        assertNull(ig.getImplicants(-1));
    }

    @Test
    public void addClause8() throws Exception {
        System.out.println("add contradiction succ 3");
        ImplicationGraph ig = new ImplicationGraph(10);
        assertNull(ig.addClause(1,2));
        assertNull(ig.addClause(-2,3));
        assertNull(ig.addClause(-2,4));
        assertEquals("[-2, 1]",ig.addClause(-4,-2).toString());
        System.out.println(ig);
        assertNull(ig.getImplicants(1));
        assertNull(ig.getImplicants(-1));
    }

}