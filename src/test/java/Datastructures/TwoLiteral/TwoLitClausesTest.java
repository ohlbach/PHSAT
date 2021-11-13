package Datastructures.TwoLiteral;

import Datastructures.Clauses.Connective;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceTest;
import Management.Controller;
import Management.GlobalParameters;
import Management.Monitor;
import Management.ProblemSupervisor;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;

import static org.junit.Assert.*;

public class TwoLitClausesTest {

    StringBuilder errors = new StringBuilder();
    StringBuilder warnings = new StringBuilder();
    boolean monitoring = false;
    
    int type = Connective.OR.ordinal();
    int typeEQ = Connective.EQUIV.ordinal();



    private TwoLitClauses prepare(boolean monitoring, boolean withSymboltable) {
        Controller controller = new Controller(null,null,null);
        GlobalParameters globalParameters=new GlobalParameters();
        globalParameters.monitor = !monitoring ? null : new Monitor(null,"mixed",errors,warnings);
        HashMap<String,Object> problemParameters = new HashMap<>();
        problemParameters.put("name","test");
        ProblemSupervisor problemSupervisor = new ProblemSupervisor(controller,globalParameters,problemParameters,null);
        Symboltable symboltable = null;
        if(withSymboltable) {
            symboltable =  new Symboltable(10);
            symboltable.setName(1,"p");
            symboltable.setName(2,"q");
            symboltable.setName(3,"r");
            symboltable.setName(4,"a");
            symboltable.setName(5,"b");
            symboltable.setName(6,"c");}
        problemSupervisor.model = new Model(20,symboltable);
        problemSupervisor.equivalenceClasses = new EquivalenceClasses(problemSupervisor);
        problemSupervisor.disjointnessClasses =  new DisjointnessClasses(problemSupervisor);
        return new TwoLitClauses(problemSupervisor);
    }
    
    public TwoLitClause make(TwoLitClauses clauses,  int literal1, int literal2) {
        return new TwoLitClause(clauses.problemSupervisor.nextClauseId(),literal1,literal2);}

    public TwoLitClause make (int[] basicClause) {
        return new TwoLitClause(basicClause[0],basicClause[2],basicClause[3]);}



    @Test
    public void removeDoubles() throws Unsatisfiable{
        System.out.println("remove doubles");
        TwoLitClauses clauses = prepare(monitoring,true);
        TwoLitClause clause1 = make(clauses,1,2);
        assertEquals("2-1: 1,2",clauses.removeDoubles(clause1).toString());
        TwoLitClause clause2 = make(clauses,1,-1);
        clause2.inferenceStep = new InferenceTest("my test 1");
        assertNull(clauses.removeDoubles(clause2));
        TwoLitClause clause3 = make(clauses,1,1);
        clause3.inferenceStep = new InferenceTest("my test 2");
        assertNull(clauses.removeDoubles(clause3));
        assertEquals("Model:\n1",clauses.model.toNumbers());
        TwoLitClause clause4 = make(clauses,-1,-1);
        clause4.inferenceStep = new InferenceTest("my test 3");
        try{assertNull(clauses.removeDoubles(clause4));}
        catch(Unsatisfiable uns) {
            System.out.println(uns.toString());}
    }
    @Test
    public void replaceEquivalentLiterals() throws Unsatisfiable {
        System.out.println("replaceEquivalentLiterals");
        TwoLitClauses clauses = prepare(monitoring, true);
        int[] clauseeq = new int[]{1,typeEQ,1,2,3};
        clauses.equivalenceClasses.addBasicEquivalenceClause(clauseeq);
        TwoLitClause clause1 = make(clauses,2,3);
        assertEquals("2-2: 1,1",clauses.replaceEquivalentLiterals(clause1).toString());
        TwoLitClause clause2 = make(clauses,2,4);
        assertEquals("2-4: 1,4",clauses.replaceEquivalentLiterals(clause2).toString());
    }

    @Test
    public void replaceTruthValues() throws Unsatisfiable {
        System.out.println("replace truth values");
        TwoLitClauses clauses = prepare(monitoring, true);
        clauses.model.add(1,new InferenceTest("may test 1"),null);
        TwoLitClause clause1 = make(clauses,2,3);
        assertEquals("2-1: 2,3",clauses.replaceTruthValues(clause1).toString());
        TwoLitClause clause2 = make(clauses,2,1);
        assertNull(clauses.replaceTruthValues(clause2));
        TwoLitClause clause3 = make(clauses,1,2);
        assertNull(clauses.replaceTruthValues(clause3));
        TwoLitClause clause4 = make(clauses,1,-1);
        assertNull(clauses.replaceTruthValues(clause4));
        TwoLitClause clause5 = make(clauses,-1,2);
        clause5.inferenceStep = new InferenceTest("may test 2");
        assertNull(clauses.replaceTruthValues(clause5));
        assertEquals("Model:\n1,2",clauses.model.toNumbers());
    }

    @Test
    public void normalize() throws Unsatisfiable {
        System.out.println("normalize");
        TwoLitClauses clauses = prepare(monitoring, true);
        TwoLitClause clause1 = make(clauses,1,2);
        assertEquals("2-1: 1,2",clauses.normalizeClause(clause1).toString());
        int[] clauseeq = new int[]{2,typeEQ,1,2,3};
        clauses.equivalenceClasses.addBasicEquivalenceClause(clauseeq);
        TwoLitClause clause2 = make(clauses,2,-3);
        assertNull(clauses.normalizeClause(clause2));
        clauses.model.add(1,new InferenceTest("may test 1"),null);
        TwoLitClause clause3 = make(clauses,-3,4);
        assertNull(clauses.normalizeClause(clause3));
        assertEquals("Model:\n1,4",clauses.model.toNumbers());
    }

    @Test
    public void findEquivalence() throws Exception {
        System.out.println("find equivalence");
        TwoLitClauses clauses = prepare(monitoring, true);
        TwoLitClause clause1 = make(clauses,1,-2);
        clauses.integrateClause(clause1,false);
        TwoLitClause clause2 = make(clauses,1,2);
        assertTrue(clauses.findEquivalences(clause2));
        TwoLitClause clause3 = make(clauses,-1,2);
        assertFalse(clauses.findEquivalences(clause3));
        assertTrue(clauses.isEmpty());

        Thread thread = new Thread(clauses.equivalenceClasses::run);
        thread.start(); Thread.sleep(100);
        thread.interrupt();
       // System.out.println(clauses.equivalenceClasses.toString());
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-4: 1=2",clauses.equivalenceClasses.toString());
    }

    @Test
    public void findDisjointnesses1() throws Exception {
        System.out.println("find disjointnesses 3");
        TwoLitClauses clauses = prepare(monitoring, true);
        TwoLitClause clause1 = make(clauses, -1, -2);
        clauses.integrateClause(clause1, false);
        TwoLitClause clause2 = make(clauses, -1, -3);
        clauses.integrateClause(clause2, false);
        assertNull(clauses.findDisjointnesses(-1));
        assertNull(clauses.findDisjointnesses(-2));
        assertNull(clauses.findDisjointnesses(1));
        TwoLitClause clause3 = make(clauses, -2, -3);
        clauses.integrateClause(clause3, false);
        assertEquals("[1, 2, 3]",clauses.findDisjointnesses(-1).toString());
    }

    @Test
    public void findDisjointnesses2() throws Exception {
        System.out.println("find disjointnesses 5");
        TwoLitClauses clauses = prepare(monitoring, true);
        TwoLitClause clause1 = make(clauses, -1, -2);
        clauses.integrateClause(clause1, false);
        TwoLitClause clause2 = make(clauses, -1, -3);
        clauses.integrateClause(clause2, false);
        TwoLitClause clause3 = make(clauses, -1, -4);
        clauses.integrateClause(clause3, false);
        TwoLitClause clause4 = make(clauses, -1, -5);
        clauses.integrateClause(clause4, false);
        TwoLitClause clause5 = make(clauses, -2, -3);
        clauses.integrateClause(clause5, false);
        TwoLitClause clause6 = make(clauses, -2, -4);
        clauses.integrateClause(clause6, false);
        TwoLitClause clause7 = make(clauses, -2, -5);
        clauses.integrateClause(clause7, false);
        TwoLitClause clause8 = make(clauses, -3, -4);
        clauses.integrateClause(clause8, false);
        TwoLitClause clause9 = make(clauses, -3, -5);
        clauses.integrateClause(clause9, false);
        TwoLitClause clause10 = make(clauses, -4, -5);
        clauses.integrateClause(clause10, false);
        assertEquals("[1, 2, 3, 4, 5]",clauses.findDisjointnesses(-1).toString());
        assertEquals("[5, 1, 2, 3, 4]",clauses.findDisjointnesses(-5).toString());

        Thread thread = new Thread(clauses.disjointnessClasses::run);
        thread.start(); Thread.sleep(100);
        thread.interrupt();
        assertEquals("Disjointness Clauses of Problem test:\n" +
                "D-11: 1!=2!=3!=4!=5",clauses.disjointnessClasses.toString());
    }

    @Test
    public void findAllDisjointnesses() throws Exception {
        System.out.println("find all disjointnesses");
        TwoLitClauses clauses = prepare(monitoring, true);
        TwoLitClause clause1 = make(clauses, -1, -2);
        clauses.integrateClause(clause1, false);
        TwoLitClause clause2 = make(clauses, -1, -3);
        clauses.integrateClause(clause2, false);
        TwoLitClause clause3 = make(clauses, -1, -4);
        clauses.integrateClause(clause3, false);
        TwoLitClause clause4 = make(clauses, -1, -5);
        clauses.integrateClause(clause4, false);
        TwoLitClause clause5 = make(clauses, -2, -3);
        clauses.integrateClause(clause5, false);
        TwoLitClause clause6 = make(clauses, -2, -4);
        clauses.integrateClause(clause6, false);
        TwoLitClause clause7 = make(clauses, -2, -5);
        clauses.integrateClause(clause7, false);
        TwoLitClause clause8 = make(clauses, -3, -4);
        clauses.integrateClause(clause8, false);
        TwoLitClause clause9 = make(clauses, -3, -5);
        clauses.integrateClause(clause9, false);
        TwoLitClause clause10 = make(clauses, -4, -5);
        clauses.integrateClause(clause10, false);
        clauses.findAllDisjointnesses();

        Thread thread = new Thread(clauses.disjointnessClasses::run);
        thread.start(); Thread.sleep(10);
        thread.interrupt();
        assertEquals("Disjointness Clauses of Problem test:\n" +
                "D-11: 1!=2!=3!=4!=5",clauses.disjointnessClasses.toString());
    }


    @Test
    public void addBasicClause1() throws Unsatisfiable{
        System.out.println("addBasicClause plain");
        TwoLitClauses clauses = prepare(monitoring,true);

        int[] clause1 = new int[]{1,type,2,3};
        int[] clause2 = new int[]{2,type,3,5};
        clauses.integrateClause(make(clause1),false);
        clauses.integrateClause(make(clause2),false);
        assertEquals("Two-Literal clauses of problem test:\n" +
                "  2-1: q,r\n" +
                "  2-2: r,b",clauses.toString());
        //System.out.println(clauses.infoString(symboltable));
    }

    @Test
    public void addBasicClause2() throws Unsatisfiable{
        System.out.println("addBasicClause tautology and subsumed");
        TwoLitClauses clauses = prepare(monitoring,true);

        int[] clause1 = new int[]{1,type,2,3};
        int[] clause2 = new int[]{2,type,3,-3};
        int[] clause3 = new int[]{3,type,3,2};

        clauses.integrateClause(make(clause1),false);
        clauses.integrateClause(make(clause2),false);
        clauses.integrateClause(make(clause3),false);
        assertEquals("Two-Literal clauses of problem test:\n" +
                "  2-1: q,r",clauses.toString());
        //System.out.println(clauses.infoString(symboltable));
    }
    @Test
    public void addBasicClause3() throws Unsatisfiable{
        System.out.println("addBasicClause with model");
        TwoLitClauses clauses = prepare(monitoring,true);
        ArrayList<Object> observed = new ArrayList<>();
        clauses.model.addObserver(Thread.currentThread(),
                ((literal, originals) -> observed.add(literal)));
        clauses.model.add(2,null,null);

        int[] clause1 = new int[]{1,type,2,3};
        int[] clause2 = new int[]{2,type,-2,4};
        int[] clause3 = new int[]{3,type,5,-2};

        clauses.integrateClause(make(clause1),false);
        clauses.integrateClause(make(clause2),false);
        clauses.integrateClause(make(clause3),false);
        assertTrue(clauses.isEmpty());
        assertEquals("[2, 4, 5]",observed.toString());
        //System.out.println(clauses.infoString(symboltable));
    }

    @Test
    public void addBasicClause4() throws Exception {
        System.out.println("addBasicClause with equivalence");
        TwoLitClauses clauses = prepare(monitoring,true);
        clauses.configure();

        int[] clauseeq = new int[]{1,typeEQ,1,2};
        clauses.equivalenceClasses.addBasicEquivalenceClause(clauseeq);

        int[] clause1 = new int[]{2,type,2,3};
        int[] clause2 = new int[]{3,type,-2,4};
        int[] clause3 = new int[]{4,type,5,-2};
        clauses.model.symboltable = null;

        clauses.addBasicClause(clause1);
        clauses.addBasicClause(clause2);
        clauses.addBasicClause(clause3);

        Thread thread = new Thread(clauses::run);
        thread.start();
        Thread.sleep(100);
        thread.interrupt();

        assertEquals("Two-Literal clauses of problem test:\n" +
                "  2-1: 1,3\n" +
                "  2-2: -1,4\n" +
                "  2-3: 4,3\n" +
                "  2-4: 5,-1\n" +
                "  2-5: 5,3",clauses.toString());
        //System.out.println(clauses.infoString(null));
    }

    @Test
    public void threadtest1() throws Exception{
        System.out.println("Thread ");
        TwoLitClauses clauses = prepare(monitoring,true);
        clauses.configure();
        ArrayList<Object> observed = new ArrayList<>();
        clauses.model.addObserver(Thread.currentThread(),
                ((literal, originals) -> observed.add(literal)));
        clauses.model.symboltable = null;

        Thread eqthread = new Thread(clauses.equivalenceClasses::run);
        Thread twothread = new Thread(clauses::run);
        eqthread.start();
        twothread.start();

        int[] clause1 = new int[]{1,type,2,3};
        int[] clause2 = new int[]{2,type,4,-3};
        int[] clause3 = new int[]{3,type,-2,3};
        int[] clause4 = new int[]{4,type,5,6};
        clauses.addBasicClause(clause1);
        clauses.addBasicClause(clause2);
        clauses.addBasicClause(clause3);
        clauses.addBasicClause(clause4);

        Thread.sleep(100);
        eqthread.interrupt(); twothread.interrupt();
        eqthread.join();twothread.join();
        assertEquals("Two-Literal clauses of problem test:\n" +
                "  2-4: 5,6",clauses.toString());
        assertEquals("[3, 4]",observed.toString());
        assertEquals("Model:\n3,4",clauses.model.toNumbers());
        System.out.println(clauses.disjointnessClasses.infoString(null));
        System.out.println(clauses.model.infoString(false));
    }

    @Test
    public void threadtest2() throws Exception {
        System.out.println("Thread with Unsatifiability");
        TwoLitClauses clauses = prepare(monitoring,true);
        clauses.configure();
        //globalParameters.monitor.addPrintId("test-TwoLit");
        clauses.model.symboltable = null;
        Thread eqthread = new Thread(clauses.equivalenceClasses::run);
        Thread twothread = new Thread(clauses::run);
        eqthread.start();
        twothread.start();

        int[] clause1 = new int[]{1,type,1,2};
        int[] clause2 = new int[]{4,type,1,-2};
        int[] clause3 = new int[]{3,type,-1,2};
        int[] clause4 = new int[]{2,type,-1,-2};
        clauses.addBasicClause(clause1);
        clauses.addBasicClause(clause2);
        clauses.addBasicClause(clause3);
        clauses.addBasicClause(clause4);

        try{ Thread.sleep(20);} catch(Exception ex) {}
        eqthread.interrupt(); twothread.interrupt();
        eqthread.join();twothread.join();
        System.out.println(clauses.problemSupervisor.result.toString());
    }

    @Test
    public void threadtest3() throws Exception {
        System.out.println("Thread with Equivalences");
        TwoLitClauses clauses = prepare(monitoring,true);

        Thread eqthread = new Thread(clauses.equivalenceClasses::run);
        Thread dthread  = new Thread(clauses.disjointnessClasses::run);
        Thread twothread = new Thread(clauses::run);
        eqthread.start();
        twothread.start();
        dthread.start();

        int[] clause1 = new int[]{1,type,-1,2};
        int[] clause2 = new int[]{2,type,1,-2};
        clauses.addBasicClause(clause1);
        clauses.addBasicClause(clause2);

        try{ Thread.sleep(20);} catch(Exception ex) {}
        eqthread.interrupt(); dthread.interrupt(); twothread.interrupt();
        eqthread.join();dthread.join();twothread.join();
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-1: 1=2", clauses.equivalenceClasses.toString());
        assertTrue(clauses.isEmpty());
    }

    @Test
    public void threadtest4() throws Exception{
        System.out.println("Thread with Disjointnesses");
        TwoLitClauses clauses = prepare(monitoring,false);

        Thread eqthread = new Thread(clauses.equivalenceClasses::run);
        Thread dthread  = new Thread(clauses.disjointnessClasses::run);
        Thread twothread = new Thread(clauses::run);
        eqthread.start();
        twothread.start();
        dthread.start();

        int[] clause1 = new int[]{1,type,-1,-2};
        int[] clause2 = new int[]{2,type,-1,-3};
        int[] clause3 = new int[]{3,type,-2,-3};
        clauses.addBasicClause(clause1);
        clauses.addBasicClause(clause2);
        clauses.addBasicClause(clause3);
        clauses.addDisjointnessFinder();

        try{ Thread.sleep(1000);} catch(Exception ex) {}
        eqthread.interrupt(); dthread.interrupt(); twothread.interrupt();
        eqthread.join();dthread.join();twothread.join();
        assertEquals("Disjointness Clauses of Problem test:\n" +
                "D-1: 1!=2!=3", clauses.disjointnessClasses.toString());
        assertEquals("Two-Literal clauses of problem test:\n" +
                "  2-1: -1,-2\n" +
                "  2-2: -1,-3\n" +
                "  2-3: -2,-3",clauses.toString());
    }
}