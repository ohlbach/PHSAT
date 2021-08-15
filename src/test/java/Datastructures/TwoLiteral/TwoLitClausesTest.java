package Datastructures.TwoLiteral;

import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Management.Monitor;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.*;

public class TwoLitClausesTest {

    StringBuffer errors = new StringBuffer();
    StringBuffer warnings = new StringBuffer();
    boolean monitoring = true;

    int type = ClauseType.OR.ordinal();
    int typeEQ = ClauseType.EQUIV.ordinal();


    @Test
    public void addBasicClause1() {
        System.out.println("addBasicClause plain");
        Monitor monitor = !monitoring ? null : new Monitor(null,"mixed",errors,warnings);
        Symboltable symboltable = new Symboltable(10);
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        Model model = new Model(10,symboltable);
        EquivalenceClasses eqClasses = new EquivalenceClasses(model,"test",monitor);
        DisjointnessClasses dClasses = new DisjointnessClasses(model,eqClasses,"test",monitor);

        TwoLitClauses clauses = new TwoLitClauses(model,eqClasses,dClasses,"test",monitor);
        int[] clause1 = new int[]{1,type,2,3};
        int[] clause2 = new int[]{1,type,3,5};
        try{clauses.integrateBasicClause(clause1,null);
            clauses.integrateBasicClause(clause2,null);}
        catch(Unsatisfiable uns) {}
        assertEquals("Two-Literal clauses of problem test:\n" +
                "  2-1: q,r\n" +
                "  2-2: r,5",clauses.toString());
        //System.out.println(clauses.infoString(symboltable));
    }

    @Test
    public void addBasicClause2() {
        System.out.println("addBasicClause tautology and subsumed");
        Monitor monitor = !monitoring ? null : new Monitor(null,"mixed",errors,warnings);
        Symboltable symboltable = new Symboltable(10);
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        Model model = new Model(10,symboltable);
        EquivalenceClasses eqClasses = new EquivalenceClasses(model,"test",monitor);
        DisjointnessClasses dClasses = new DisjointnessClasses(model,eqClasses,"test",monitor);

        TwoLitClauses clauses = new TwoLitClauses(model,eqClasses,dClasses,"test",monitor);
        int[] clause1 = new int[]{1,type,2,3};
        int[] clause2 = new int[]{2,type,3,-3};
        int[] clause3 = new int[]{3,type,3,2};

        try{clauses.integrateBasicClause(clause1,null);
            clauses.integrateBasicClause(clause2,null);
            clauses.integrateBasicClause(clause3,null);}
        catch(Unsatisfiable uns) {}
        assertEquals("Two-Literal clauses of problem test:\n" +
                "  2-1: q,r",clauses.toString());
        //System.out.println(clauses.infoString(symboltable));
    }
    @Test
    public void addBasicClause3() {
        System.out.println("addBasicClause with model");
        Monitor monitor = !monitoring ? null : new Monitor(null,"mixed",errors,warnings);
        Symboltable symboltable = new Symboltable(10);
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"s");
        Model model = new Model(10,symboltable);
        ArrayList<Object> observed = new ArrayList<>();
        model.addObserver(Thread.currentThread(),
                ((literal, originals) -> {
                    observed.add(literal);
                    observed.add(originals);
                }));
        IntArrayList origins = new IntArrayList();
        origins.add(20);
        model.addImmediately(2,origins);
        EquivalenceClasses eqClasses = new EquivalenceClasses(model,"test",monitor);
        DisjointnessClasses dClasses = new DisjointnessClasses(model,eqClasses,"test",monitor);

        TwoLitClauses clauses = new TwoLitClauses(model,eqClasses,dClasses,"test",monitor);
        int[] clause1 = new int[]{1,type,2,3};
        int[] clause2 = new int[]{2,type,-2,4};
        int[] clause3 = new int[]{3,type,5,-2};

        try{clauses.integrateBasicClause(clause1,null);
            clauses.integrateBasicClause(clause2,null);
            clauses.integrateBasicClause(clause3,null);}
        catch(Unsatisfiable uns) {}
        assertTrue(clauses.isEmpty());
        assertEquals("[4, [2, 20], 5, [3, 20]]",observed.toString());
        //System.out.println(clauses.infoString(symboltable));
    }

    @Test
    public void addBasicClause4() {
        System.out.println("addBasicClause with equivalence");
        Monitor monitor = !monitoring ? null : new Monitor(null,"mixed",errors,warnings);
        Model model = new Model(10,null);
        EquivalenceClasses eqClasses = new EquivalenceClasses(model,"test",monitor);
        DisjointnessClasses dClasses = new DisjointnessClasses(model,eqClasses,"test",monitor);

        int[] clauseeq = new int[]{1,typeEQ,1,2};
        try{eqClasses.addBasicEquivalenceClause(clauseeq);} catch(Unsatisfiable uns) {}

        TwoLitClauses clauses = new TwoLitClauses(model,eqClasses,dClasses,"test",monitor);
        int[] clause1 = new int[]{2,type,2,3};
        int[] clause2 = new int[]{3,type,-2,4};
        int[] clause3 = new int[]{4,type,5,-2};

        try{clauses.integrateBasicClause(clause1,null);
            clauses.integrateBasicClause(clause2,null);
            clauses.integrateBasicClause(clause3,null);}
        catch(Unsatisfiable uns) {}
        assertEquals("Two-Literal clauses of problem test:\n" +
                "  2-1: 1,3\n" +
                "  2-2: -1,4\n" +
                "  2-3: 5,-1",clauses.toString());
        //System.out.println(clauses.infoString(null));
    }

    @Test
    public void threadtest1() {
        System.out.println("Thread ");
        Monitor monitor = !monitoring ? null : new Monitor(null,"mixed",errors,warnings);
        Model model = new Model(10, null);
        ArrayList<Object> observed = new ArrayList<>();
        model.addObserver(Thread.currentThread(),
                ((literal, originals) -> {
                    observed.add(literal);
                    observed.add(originals);
                }));
        EquivalenceClasses eqClasses = new EquivalenceClasses(model, "test", monitor);
        DisjointnessClasses dClasses = new DisjointnessClasses(model, eqClasses, "test", monitor);
        TwoLitClauses clauses = new TwoLitClauses(model,eqClasses,dClasses,"test",monitor);

        Thread eqthread = new Thread(()->eqClasses.run());
        Thread twothread = new Thread(()->clauses.run());
        eqthread.start();
        twothread.start();

        int[] clause1 = new int[]{1,type,2,3};
        int[] clause2 = new int[]{2,type,4,-3};
        int[] clause3 = new int[]{3,type,-2,3};
        int[] clause4 = new int[]{3,type,5,6};
        clauses.addBasicClause(clause1);
        clauses.addBasicClause(clause2);
        clauses.addBasicClause(clause3);
        clauses.addBasicClause(clause4);

        try{Thread.sleep(100);}catch(Exception ex) {}
        eqthread.interrupt(); twothread.interrupt();
        try{eqthread.join();twothread.join();} catch(Exception ex) {}
        assertEquals("Two-Literal clauses of problem test:\n" +
                "  2-4: 5,6",clauses.toString());
        assertEquals("[3, [1, 3], 4, [1, 2, 3]]",observed.toString());
        assertEquals("3,4",model.toNumbers());
        //System.out.println(dClasses.infoString(null));
        //System.out.println(model.infoString(false));
    }

    @Test
    public void threadtest2() {
        System.out.println("Thread with Unsatifiability");
        Monitor monitor = !monitoring ? null : new Monitor(null,"mixed",errors,warnings);
        Model model = new Model(10, null);
        EquivalenceClasses eqClasses = new EquivalenceClasses(model, "test", monitor);
        DisjointnessClasses dClasses = new DisjointnessClasses(model, eqClasses, "test", monitor);
        TwoLitClauses clauses = new TwoLitClauses(model,eqClasses,dClasses,"test",monitor);

        Thread eqthread = new Thread(()->eqClasses.run());
        Thread twothread = new Thread(()->clauses.run());
        eqthread.start();
        twothread.start();

        int[] clause1 = new int[]{1,type,1,2};
        int[] clause2 = new int[]{2,type,1,-2};
        int[] clause3 = new int[]{3,type,-1,2};
        int[] clause4 = new int[]{3,type,-1,-2};
        clauses.addBasicClause(clause1);
        clauses.addBasicClause(clause2);
        clauses.addBasicClause(clause3);
        clauses.addBasicClause(clause4);

        try{Thread.sleep(100);}catch(Exception ex) {}
        eqthread.interrupt(); twothread.interrupt();
        try{eqthread.join();twothread.join();} catch(Exception ex) {}
        System.out.println(clauses.result.toString());
    }

    @Test
    public void threadtest3() {
        System.out.println("Thread with Equivalences");
        Monitor monitor = !monitoring ? null : new Monitor(null,"mixed",errors,warnings);
        Model model = new Model(10, null);
        EquivalenceClasses eqClasses = new EquivalenceClasses(model, "test", monitor);
        DisjointnessClasses dClasses = new DisjointnessClasses(model, eqClasses, "test", monitor);
        TwoLitClauses clauses = new TwoLitClauses(model,eqClasses,dClasses,"test",monitor);

        Thread eqthread = new Thread(()->eqClasses.run());
        Thread dthread  = new Thread(()->dClasses.run());
        Thread twothread = new Thread(()->clauses.run());
        eqthread.start();
        twothread.start();
        dthread.start();

        int[] clause1 = new int[]{1,type,-1,2};
        int[] clause2 = new int[]{2,type,1,-2};
        clauses.addBasicClause(clause1);
        clauses.addBasicClause(clause2);

        try{Thread.sleep(100);}catch(Exception ex) {}
        eqthread.interrupt(); dthread.interrupt(); twothread.interrupt();
        try{eqthread.join();dthread.join();twothread.join();} catch(Exception ex) {}
        assertEquals("E-1: 1 = 2", eqClasses.toString());
        assertTrue(clauses.isEmpty());
    }

    @Test
    public void threadtest4() {
        System.out.println("Thread with Disjointnesses");
        Monitor monitor = !monitoring ? null : new Monitor(null,"mixed",errors,warnings);
        Model model = new Model(10, null);
        EquivalenceClasses eqClasses = new EquivalenceClasses(model, "test", monitor);
        DisjointnessClasses dClasses = new DisjointnessClasses(model, eqClasses, "test", monitor);
        TwoLitClauses clauses = new TwoLitClauses(model,eqClasses,dClasses,"test",monitor);

        Thread eqthread = new Thread(()->eqClasses.run());
        Thread dthread  = new Thread(()->dClasses.run());
        Thread twothread = new Thread(()->clauses.run());
        eqthread.start();
        twothread.start();
        dthread.start();

        int[] clause1 = new int[]{1,type,-1,-2};
        int[] clause2 = new int[]{2,type,-1,-3};
        int[] clause3 = new int[]{2,type,-2,-3};
        clauses.addBasicClause(clause1);
        clauses.addBasicClause(clause2);
        clauses.addBasicClause(clause3);

        try{Thread.sleep(100);}catch(Exception ex) {}
        eqthread.interrupt(); dthread.interrupt(); twothread.interrupt();
        try{eqthread.join();dthread.join();twothread.join();} catch(Exception ex) {}
        assertEquals("Disjointness Classes:\n" +
                "D-1: 2 != 3 != 1", dClasses.toString());
        assertEquals("Two-Literal clauses of problem test:\n" +
                "  2-1: -1,-2\n" +
                "  2-2: -1,-3\n" +
                "  2-3: -2,-3",clauses.toString());
    }
}