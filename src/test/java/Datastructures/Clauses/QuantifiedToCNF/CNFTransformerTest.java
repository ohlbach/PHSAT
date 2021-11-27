package Datastructures.Clauses.QuantifiedToCNF;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.Connective;
import Datastructures.Results.Result;
import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.*;

public class CNFTransformerTest {

    int atL = Connective.ATLEAST.ordinal();
    int atM = Connective.ATMOST.ordinal();
    int ex = Connective.EXACTLY.ordinal();
    int intV = Connective.INTERVAL.ordinal();


    @Test
    public void experiment() throws Result {
        System.out.println("experiment");
        int[] ids = new int[]{0};
        CNFTransformer ct = new CNFTransformer(true, (() -> ++ids[0]));
        Clause c1 = new Clause(new int[]{1, intV, 2,3, 1,2,3,4});
        long t1 = System.nanoTime();
        ArrayList<Clause> cnf1 = ct.intervalToCNF(c1);
        t1 = System.nanoTime()-t1;
        System.out.println("Interval Time " + t1);
/*
        ids[0] = 0;
        long t2 = System.nanoTime();
        ArrayList<Clause> cnf2 = ct.toCNF(c1);
        t2 = System.nanoTime()-t2;
        System.out.println("Exactly Time " + t2);*/

        for(Clause clause : cnf1) System.out.println(clause.toNumbers());
    }

    @Test
    public void intervalToCNF1() throws Result {
        System.out.println("interval to CNF atleast");
        int[] ids = new int[]{0};
        CNFTransformer ct = new CNFTransformer(true, (() -> ++ids[0]));
        Clause c1 = new Clause(new int[]{1, intV, 2, 3, 1, 2, 3});
        ArrayList<Clause> cnf = ct.intervalToCNF(c1);
        assertEquals("[1: 1,2, 2: 1,3, 3: 2,3]",cnf.toString());
        //for(Clause clause : cnf) System.out.println(clause.toNumbers());
    }

    @Test
    public void intervalToCNF2() throws Result {
        System.out.println("interval to CNF exactly");
        int[] ids = new int[]{0};
        CNFTransformer ct = new CNFTransformer(true, (() -> ++ids[0]));
        Clause c1 = new Clause(new int[]{1, ex, 2, 1, 2, 3});
        //ArrayList<Clause> cnf = ct.toCNF(c1);
        ArrayList<Clause> cnf = ct.intervalToCNF(c1);
        assertEquals("[1: -1,-2,-3, 2: 1,2, 3: 1,3, 4: 2,3]",cnf.toString());
        //for(Clause clause : cnf) System.out.println(clause.toNumbers());

    }

    @Test
    public void intervalToCNF3() throws Result {
        System.out.println("interval to CNF atmost");
        int[] ids = new int[]{0};
        CNFTransformer ct = new CNFTransformer(true, (() -> ++ids[0]));
        Clause c1 = new Clause(new int[]{1, atM, 2, 1, 2, 3});
        //ArrayList<Clause> cnf = ct.toCNF(c1);
        ArrayList<Clause> cnf = ct.intervalToCNF(c1);
        assertEquals("[1: -1,-2,-3]",cnf.toString());
        //for(Clause clause : cnf) System.out.println(clause.toNumbers());
    }

    @Test
    public void intervalToCNF4() throws Result {
        System.out.println("interval to CNF atmost 2");
        int[] ids = new int[]{0};
        CNFTransformer ct = new CNFTransformer(true, (() -> ++ids[0]));
        Clause c1 = new Clause(new int[]{1, atM, 2, 1, 2, 3, 4});
        //ArrayList<Clause> cnf = ct.toCNF(c1);
        ArrayList<Clause> cnf = ct.intervalToCNF(c1);
        assertEquals("[1: -1,-3,-4, 2: -2,-3,-4, 3: -1,-2,-4, 4: -1,-2,-3]",cnf.toString());
        //for(Clause clause : cnf) System.out.println(clause.toNumbers());
    }

    @Test
    public void intervalToCNF5() throws Result {
        System.out.println("interval to CNF atleast 2");
        int[] ids = new int[]{0};
        CNFTransformer ct = new CNFTransformer(true, (() -> ++ids[0]));
        Clause c1 = new Clause(new int[]{1, atL, 2, 1, 2, 3, 4});
        //ArrayList<Clause> cnf = ct.toCNF(c1);
        ArrayList<Clause> cnf = ct.intervalToCNF(c1);
        assertEquals("[1: 1,2,3, 2: 1,2,4, 3: 1,3,4, 4: 2,3,4]",cnf.toString());
        //for(Clause clause : cnf) System.out.println(clause.toNumbers());
    }

    @Test
    public void intervalToCNF6() throws Result {
        System.out.println("interval to CNF interval 2");
        int[] ids = new int[]{0};
        CNFTransformer ct = new CNFTransformer(true, (() -> ++ids[0]));
        Clause c1 = new Clause(new int[]{1, intV, 2, 3, 1, 2, 3, 4});
        ArrayList<Clause> cnf = ct.intervalToCNF(c1);
        assertEquals("[1: -1,-2,-3,-4, 2: 1,2,3, 3: 1,2,4, 4: 1,3,4, 5: 2,3,4]",cnf.toString());
        //for(Clause clause : cnf) System.out.println(clause.toNumbers());
    }
    @Test
    public void intervalToCNF7() throws Result {
        System.out.println("interval to CNF interval mixed");
        int[] ids = new int[]{0};
        CNFTransformer ct = new CNFTransformer(true, (() -> ++ids[0]));
        Clause c1 = new Clause(new int[]{1, intV, 2, 3, 1, 2, -1,-2,3,4,3});
        ArrayList<Clause> cnf = ct.intervalToCNF(c1);
        assertEquals("[1: -3]",cnf.toString());
        //for(Clause clause : cnf) System.out.println(clause.toNumbers());
    }

    @Test
    public void intervalToCNF8() throws Result {
        System.out.println("interval to CNF large");
        int[] ids = new int[]{0};
        CNFTransformer ct = new CNFTransformer(true, (() -> ++ids[0]));
        Clause c1 = new Clause(new int[]{1, ex, 5, 1,2,3,4,5,6,7,8,9,10});
        long t1 = System.nanoTime();
        ArrayList<Clause> cnf1 = ct.intervalToCNF(c1);
        t1 = System.nanoTime()-t1;
        System.out.println("Interval Time " + t1);
        long t2 = System.nanoTime();
        ArrayList<Clause> cnf2 = ct.toCNF(c1);
        t2 = System.nanoTime()-t2;
        System.out.println("Exactly Time " + t2);

        //assertEquals("[1: -3]",cnf.toString());
        //for(Clause clause : cnf1) System.out.println(clause.toNumbers());
    }



    @Test
    public void atleastToCNF() throws Result {
        System.out.println("atleast to CNF");
        int[] ids = new int[]{0};
        CNFTransformer ct = new CNFTransformer(true,(()->++ids[0]));
        Clause c1 = new Clause( new int[]{1,atL,3,1,2,3,4,5});
        ArrayList<Clause> cnf = ct.toCNF(c1);
        assertEquals("[1: 1,2,3, 2: 1,2,4, 3: 1,2,5, 4: 1,3,4, 5: 1,3,5, 6: 1,4,5, 7: 2,3,4, 8: 2,3,5, 9: 2,4,5, 10: 3,4,5]",cnf.toString());
        Clause c2 = new Clause( new int[]{2,atL,2,1,2,3,4,5,6});
        ids[0] = 0;
        cnf = ct.toCNF(c2);
        assertEquals("[1: 1,2,3,4,5, 2: 1,2,3,4,6, 3: 1,2,3,5,6, 4: 1,2,4,5,6, 5: 1,3,4,5,6, 6: 2,3,4,5,6]",cnf.toString());
        /*
        for(Clause c : cnf) {
            System.out.println(c.toString(0, null));
            System.out.println(c.inferenceStep.toString());}
        */
        Clause c3 = new Clause( new int[]{3,atL,2, 1,2,3,2,1});
        ids[0] = 0;
        cnf = ct.toCNF(c3);
        assertEquals("[1: 1,2]",cnf.toString());}


    @Test
    public void atmostToCNF()  throws Result {
        System.out.println("atmost to CNF");
        int[] ids = new int[]{0};
        CNFTransformer ct = new CNFTransformer(true,(()->++ids[0]));
        Clause c1 = new Clause( new int[]{1,atM,2,1,2,3});
        ArrayList<Clause> cnf = ct.toCNF(c1);
        assertEquals("[1: -1,-2,-3]",cnf.toString());

        c1 = new Clause( new int[]{2,atM,2,1,2,3,4});
        cnf = ct.toCNF(c1);
        assertEquals("[2: -1,-2,-3, 3: -1,-2,-4, 4: -1,-3,-4, 5: -2,-3,-4]",cnf.toString());

        c1 = new Clause( new int[]{3,atM,2,1,2,3,3});
        cnf = ct.toCNF(c1);
        assertEquals("[6: -1,-3, 7: -2,-3]",cnf.toString());

        c1 = new Clause( new int[]{4,atM,3,1,2,3,4,5,6});
        cnf = ct.toCNF(c1);
        assertEquals("[8: -1,-2,-3,-4, 9: -1,-2,-3,-5, 10: -1,-2,-3,-6, 11: -1,-2,-4,-5, 12: -1,-2,-4,-6, 13: -1,-2,-5,-6, 14: -1,-3,-4,-5, 15: -1,-3,-4,-6, 16: -1,-3,-5,-6, 17: -1,-4,-5,-6, 18: -2,-3,-4,-5, 19: -2,-3,-4,-6, 20: -2,-3,-5,-6, 21: -2,-4,-5,-6, 22: -3,-4,-5,-6]",cnf.toString());

        /*for(Clause c:cnf) {
            System.out.println(c.toNumbers());
            System.out.println(c.inferenceStep.toString(null));
        } */
    }


    @Test
    public void exactlyToCNF()  throws Result {
        System.out.println("exactly to CNF");
        int[] ids = new int[]{0};
        CNFTransformer ct = new CNFTransformer(true,(()->++ids[0]));
        Clause c1 = new Clause( new int[]{1,ex,2,1,2,3});
        ArrayList<Clause> cnf = ct.toCNF(c1);
        assertEquals("[1: 1,2, 2: 1,3, 3: 2,3, 4: -1,-2,-3]",cnf.toString());

        c1 = new Clause( new int[]{1,ex,2,1,2,2});
        cnf = ct.toCNF(c1);
        assertEquals("[5: 2, 6: -1,-2]",cnf.toString());

        c1 = new Clause( new int[]{1,ex,2,1,2,3,4});
        cnf = ct.toCNF(c1);
        assertEquals("[7: 1,2,3, 8: 1,2,4, 9: 1,3,4, 10: 2,3,4, 11: -1,-2,-3, 12: -1,-2,-4, 13: -1,-3,-4, 14: -2,-3,-4]",cnf.toString());

        c1 = new Clause( new int[]{1,ex,3,1,2,3,4,5,6,7});
        long t1 = System.nanoTime();
        cnf = ct.toCNF(c1);
        System.out.println(System.nanoTime()-t1);
        System.out.println(cnf.size());
        /*
        for(Clause c : cnf) {System.out.println(c.toNumbers());
            System.out.println(c.inferenceStep.toString(null));} */
    }
}