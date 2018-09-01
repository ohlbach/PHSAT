package Datastructures.Theory;

import org.junit.Test;

import java.util.ArrayList;

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
        System.out.println("add single sequence");
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
        assertNull(ig.getImplicants(1));
        assertNull(ig.getImplicants(-1));
    }

    @Test
    public void addClause7() throws Exception {
        System.out.println("add contradiction succ 2");
        ImplicationGraph ig = new ImplicationGraph(10);
        assertNull(ig.addClause(1,2));
        assertEquals("[2]",ig.addClause(-1,2).toString());
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
        assertNull(ig.getImplicants(1));
        assertNull(ig.getImplicants(-1));
    }
    @Test
    public void addClause9() throws Exception {
        System.out.println("large numbers");
        ImplicationGraph ig = new ImplicationGraph(50000);
        assertNull(ig.addClause(1000,2000));
        assertNull(ig.addClause(-2000,3000));
        assertNull(ig.addClause(-2000,4000));
        assertEquals("[1000, -2000]",ig.addClause(-4000,-2000).toString());
        assertNull(ig.getImplicants(1000));
        assertNull(ig.getImplicants(-1000));
    }

    @Test
    public void addClause10() throws Exception {
        System.out.println("back");
        ImplicationGraph ig = new ImplicationGraph(10);
        assertNull(ig.addClause(1,3));
        assertNull(ig.addClause(2,3));
        assertNull(ig.addClause(-3,4));
        assertEquals("[3, 4]",ig.getImplicants(-1).toString());
        assertEquals("[3, 4]",ig.getImplicants(-2).toString());
        assertEquals("[4]",ig.getImplicants(3).toString());
        assertEquals("[1, 2]",ig.getImplicants(-3).toString());
        assertEquals("[-3, 1, 2]",ig.getImplicants(-4).toString());
    }

    @Test
    public void addClause11() throws Exception {
        System.out.println("back unit 1");
        ImplicationGraph ig = new ImplicationGraph(10);
        assertNull(ig.addClause(1,3));
        assertNull(ig.addClause(2,3));
        assertNull(ig.addClause(-3,4));
        assertNull(ig.addClause(-4,5));
        assertNull(ig.addClause(-1,6));
        assertEquals("[1, 6]",ig.addClause(-4,1).toString());

        assertEquals("[3, 4, 5]",ig.getImplicants(-2).toString());
        assertNull(ig.getImplicants(1));
        assertNull(ig.getImplicants(-1));
        assertNull(ig.getImplicants(6));
        assertNull(ig.getImplicants(-6));
        assertEquals("[-3, 2]",ig.getImplicants(-4).toString());
    }

    @Test
    public void addClause12() throws Exception {
        System.out.println("back unit 2");
        ImplicationGraph ig = new ImplicationGraph(10);
        assertNull(ig.addClause(-1,2));
        assertNull(ig.addClause(-3,4));
        assertNull(ig.addClause(-2,5));
        assertNull(ig.addClause(-4,5));
        assertNull(ig.addClause(-6,7));
        assertNull(ig.addClause(-7,8));
        assertNull(ig.addClause(-7,-1));
        assertNull(ig.addClause(-8,-3));
        assertNull(ig.addClause(1,9));
        assertEquals("[-1, -3, 9]",ig.addClause(-5,6).toString());
        assertEquals("[5, 6, 7, 8]",ig.getImplicants(2).toString());
        assertNull(ig.getImplicants(1));
        assertNull(ig.getImplicants(-1));
        assertNull(ig.getImplicants(3));
        assertNull(ig.getImplicants(-3));
    }

    @Test
    public void removeLiteral() throws Exception {
        System.out.println("remove literal");
        ImplicationGraph ig = new ImplicationGraph(10);
        ig.addClause(-1, 2);
        ig.addClause(-2, 3);
        ig.addClause(-3, 4);
        ig.addClause(-4, 5);
        assertEquals("[2, 3, 4, 5]",ig.getImplicants(1).toString());
        ig.removeLiteral(3);
        assertEquals("[2, 4, 5]",ig.getImplicants(1).toString());
    }

}