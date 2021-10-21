package Datastructures.Theory;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceTest;
import Management.Controller;
import Management.GlobalParameters;
import Management.Monitor;
import Management.ProblemSupervisor;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;

import static org.junit.Assert.*;

public class DisjointnessClassesTest {

    StringBuffer errors = new StringBuffer();
    StringBuffer warnings = new StringBuffer();
    boolean monitoring = false;

    int type = ClauseType.ATMOST.ordinal();
    int typeEQ = ClauseType.EQUIV.ordinal();


    private DisjointnessClasses prepare(boolean monitoring, boolean withSymboltable) {
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
        return new DisjointnessClasses(problemSupervisor);
    }

    private Clause make(int id,int... literals) {
        return new Clause(id,ClauseType.ATMOST, IntArrayList.wrap(literals));
    }


    @Test
    public void removeDouble() throws Unsatisfiable {
        System.out.println("Remove Double and Inconsistency");
        DisjointnessClasses dClasses = prepare(monitoring, false);
        Clause c1 = make(1, 1, 2, 3);
        Clause c2 = dClasses.removeDoubles(c1);
        assertSame(c1, c2);
        assertEquals("D-1: 1!=2!=3", c1.toString());

        Clause c3 = make(2, 1, 2, 3, 2);
        Clause c4 = dClasses.removeDoubles(c3);
        assertEquals("D-1: 1!=3", c4.toString());
        assertEquals("Model:\n-2", dClasses.model.toNumbers());


        Clause c5 = make(3, 1, 2, 3, 2,3,4);
        Clause c6 = dClasses.removeDoubles(c5);
        assertEquals("D-3: 1!=4", c6.toString());
        assertEquals("Model:\n-2,-3", dClasses.model.toNumbers());

        Clause c7 = make(4, 5,6,5,7,8,-7);
        Clause c8 = dClasses.removeDoubles(c7);
        assertNull(c8);
        assertEquals("Model:\n-2,-3,-5,-6,-8", dClasses.model.toNumbers());

        Clause c9 = make(5, 9,10,-9,10,11);
        Clause c10 = dClasses.removeDoubles(c9);
        assertNull(c10);
        assertEquals("Model:\n-2,-3,-5,-6,-8,-10,-11", dClasses.model.toNumbers());

        try{
            Clause c11 = make(6, 9,9,-9,10,-10);
            dClasses.removeDoubles(c11);}
        catch(Unsatisfiable uns) {
            if(monitoring) System.out.println(uns);
        else System.out.println("UNSATISFIABLE");}}


    @Test
    public void replaceTruthValues() throws Unsatisfiable {
        System.out.println("replaceTruthValues");
        DisjointnessClasses dClasses = prepare(monitoring, false);
        InferenceTest inf = new InferenceTest("my test 1");
        Clause c1 = make(1, 1, 2, 3);
        Clause c2 = dClasses.replaceTruthValues(c1);
        assertSame(c1,c2);
        dClasses.model.add(2,inf,null);
        dClasses.replaceTruthValues(c1);
        assertEquals("Model:\n-1,2,-3",dClasses.model.toNumbers());

        Clause c3 = make(2, 4,5,6,7);
        dClasses.model.add(-5,inf,null);
        dClasses.model.add(-7,inf,null);
        Clause c4 = dClasses.replaceTruthValues(c3);
        assertEquals("D-2: 4!=6",c4.toString());
    }

    @Test
    public void normalizeClause() throws Unsatisfiable {
        System.out.println("normalizeClause");
        DisjointnessClasses dClasses = prepare(monitoring, false);
        int[] c1 = new int[]{1,ClauseType.EQUIV.ordinal(),3,2,1};
        dClasses.equivalenceClasses.addBasicEquivalenceClause(c1);
        Clause c2 = make(2, 2,3,4,5);
        Clause c3 = dClasses.normalizeClause(c2);
        assertEquals("D-3: 4!=5",c3.toString());
        assertEquals("Model:\n-1",dClasses.model.toNumbers());
    }
    @Test
    public void integrateDerivedDisjoints() throws Exception{
        System.out.println("integrateDerivedDisjoints");
        DisjointnessClasses dClasses = prepare(monitoring,true);
        IntArrayList literals = new IntArrayList();
        literals.add(1); literals.add(3);literals.add(2);
        dClasses.addDerivedDisjoints(literals,null);
        Thread thread = new Thread(dClasses::run);
        thread.start(); Thread.sleep(20);
        thread.interrupt();
        assertEquals("Disjointness Clauses of Problem test:\n" +
                "D-1: p!=q!=r",dClasses.toString(dClasses.symboltable));
        //System.out.println(dClasses.infoString(dClasses.symboltable));
    }

    @Test
    public void integrateDisjointnessClause1() throws Unsatisfiable {
        System.out.println("integrateDisjointnessClause1");
        DisjointnessClasses dClasses = prepare(monitoring,true);
        int[] clause = new int[]{1,type,1,2,3};
        dClasses.integrateDisjointnessClause(new Clause(1,clause));
        assertEquals("Disjointness Clauses of Problem test:\n" +
                "D-1: p!=q!=r",dClasses.toString(dClasses.symboltable));
        //System.out.println(dClasses.infoString(dClasses.symboltable));
        assertNotNull(dClasses.areDisjoint(1, 3));
        assertNull(dClasses.areDisjoint(1,-3));
        assertNull(dClasses.areDisjoint(1,4));
        //assertEquals("[1]",dClasses.getOrigins(1,2).toString());

        clause = new int[]{2,type,4,2,-4};
        dClasses.integrateDisjointnessClause(new Clause(2,clause));
        assertEquals("Disjointness Clauses of Problem test:\n" +
                "D-1: p!=q!=r",dClasses.toString(dClasses.symboltable));
        assertEquals("Model:\n-2",dClasses.model.toNumbers());
        //System.out.println(dClasses.infoString(null));
        clause = new int[]{2,type,5,3,5};
        dClasses.integrateDisjointnessClause(new Clause(3,clause));
        assertEquals("Model:\n-2,-5",dClasses.model.toNumbers());
        assertEquals("Disjointness Clauses of Problem test:\n" +
                "D-1: p!=q!=r",dClasses.toString(dClasses.symboltable));
    }

    @Test
    public void integrateDisjointnessClause2() throws Unsatisfiable{
        System.out.println("integrateDisjointnessClause with model");
        DisjointnessClasses dClasses = prepare(monitoring,true);
        ArrayList<Object> observed = new ArrayList<>();
        dClasses.model.addObserver(Thread.currentThread(),
                ((literal, originals) -> {
                    observed.add(literal);
                    observed.add(originals);
                }));
        dClasses.model.add(2,null,null);
        int[] clause = new int[]{1, type, 1, 2, 3};
        dClasses.integrateDisjointnessClause(new Clause(1,clause));
        assertEquals("[2, null, -1, Disjointness True Literals:\n" +
                "D-1: 1!=2!=3 and true(2) -> -1, -3, Disjointness True Literals:\n" +
                "D-1: 1!=2!=3 and true(2) -> -3]",observed.toString());
    }

    @Test
    public void integrateDisjointnessClause3() throws Exception{
        System.out.println("integrateDisjointnessClause with equivalences");
        DisjointnessClasses dClasses = prepare(monitoring,true);
        int[] clause = new int[]{1,typeEQ,3,2,1};
        dClasses.equivalenceClasses.addBasicEquivalenceClause(clause);
        clause = new int[]{2,type,4,2,5};
        dClasses.addDisjointnessClause(clause);
        Thread thread = new Thread(dClasses::run);
        thread.start(); Thread.sleep(10);
        thread.interrupt();
        assertEquals("Disjointness Clauses of Problem test:\n" +
                "D-2: p!=a!=b",dClasses.toString(dClasses.symboltable));
        //System.out.println(dClasses.infoString(dClasses.symboltable));
        }

    @Test
    public void forwardSubsumption() throws Unsatisfiable {
        System.out.println("forwardSubsumption");
        DisjointnessClasses dClasses = prepare(monitoring,true);
        int[] clause1 = new int[]{1, type, 1, 2, 3, 4};
        int[] clause2 = new int[]{2, type, 3,2,1};
        dClasses.integrateDisjointnessClause(new Clause(1,clause1));
        dClasses.integrateDisjointnessClause(new Clause(1,clause2));
        assertEquals("Disjointness Clauses of Problem test:\n" +
                "D-1: 1!=2!=3!=4",dClasses.toString());
        //System.out.println(dClasses.infoString(dClasses.symboltable));

        int[] clause = new int[]{3,typeEQ,1,5};
        dClasses.equivalenceClasses.addBasicEquivalenceClause(clause);
        clause2 = new int[]{4, type, 3,2,5};
        dClasses.integrateDisjointnessClause(new Clause(4,clause2));
        assertEquals("Disjointness Clauses of Problem test:\n" +
                "D-1: 1!=2!=3!=4",dClasses.toString());
    }
    @Test
    public void backwardSubsumption() throws Unsatisfiable {
        System.out.println("backwardSubsumption");
        DisjointnessClasses dClasses = prepare(monitoring,true);
        int[] clause1 = new int[]{1, type, 1, 2, 3, 4};
        int[] clause2 = new int[]{2, type, 3,2,1};
        dClasses.integrateDisjointnessClause(new Clause(1,clause1));
        dClasses.integrateDisjointnessClause(new Clause(2,clause2));
        assertEquals("Disjointness Clauses of Problem test:\n" +
                "D-1: 1!=2!=3!=4",dClasses.toString());
        //System.out.println(dClasses.infoString(dClasses.symboltable));

        int[] clause = new int[]{3,typeEQ,1,5};
        dClasses.equivalenceClasses.addBasicEquivalenceClause(clause);
        clause2 = new int[]{4, type, 3,2,5,4,6};
        
        dClasses.integrateDisjointnessClause(new Clause(4,clause2));
        assertEquals("Disjointness Clauses of Problem test:\n" +
                "D-1: 1!=2!=3!=4!=6",dClasses.toString());
        //System.out.println(dClasses.infoString(null));
    }

    @Test
    public void resolve() throws Exception {
        System.out.println("resolve");
        DisjointnessClasses dClasses = prepare(monitoring,false);
        ArrayList<Object> observed = new ArrayList<>();
        dClasses.model.addObserver(Thread.currentThread(),
                ((literal, originals) -> {
                    observed.add(literal);
                    observed.add(originals);
                }));
        dClasses.configure();
        int[] clause1 = new int[]{dClasses.problemSupervisor.nextClauseId(), type, 1, 2, 3, 4};
        int[] clause2 = new int[]{dClasses.problemSupervisor.nextClauseId(), type, 3, 2, -1,5};
            dClasses.integrateDisjointnessClause(new Clause(1,clause1));
            dClasses.integrateDisjointnessClause(new Clause(2,clause2));
        Thread thread = new Thread(dClasses::run);
        thread.start(); Thread.sleep(20);
        thread.interrupt();

        assertEquals("[-3, Disjointness Double:\n" +
                "D-3: 3!=2!=5!=2!=3!=4 -> -3, -2, Disjointness Double:\n" +
                "D-4: 2!=5!=2!=4 -> -2]",observed.toString());
        assertEquals("Disjointness Clauses of Problem test:\n" +
                "D-5: 4!=5\n" +
                "D-8: 1!=4\n" +
                "D-9: -1!=5",dClasses.toString(null));
       // System.out.println(dClasses.infoString(dClasses.symboltable));
    }
    @Test
    public void extendNormalizedClause() throws Unsatisfiable {
        System.out.println("extendNormalizedClause");
        DisjointnessClasses dClasses = prepare(monitoring,true);
        int[] clause1 = new int[]{1, type, 1, 2, 3};
        int[] clause2 = new int[]{2, type, 1, 3, 4};
        int[] clause3 = new int[]{3, type, 2,3,4};

        dClasses.integrateDisjointnessClause(new Clause(1,clause1));
        dClasses.integrateDisjointnessClause(new Clause(2,clause2));
        dClasses.integrateDisjointnessClause(new Clause(3,clause3));
        assertEquals("Disjointness Clauses of Problem test:\n" +
                "D-1: 1!=2!=3!=4",dClasses.toString());
     //   System.out.println(dClasses.infoString(null));


    }
        @Test
    public void integrateTrueLiteral() throws Exception{
            System.out.println("integrateTrueLiteral");
            DisjointnessClasses dClasses = prepare(monitoring,true);
            ArrayList<Object> observed = new ArrayList<>();
            dClasses.model.addObserver(Thread.currentThread(),
                    ((literal, originals) -> {
                        observed.add(literal);
                        observed.add(originals);
                    }));
            int[] clause1 = new int[]{1, type, 1, 2, 3};
            int[] clause2 = new int[]{2, type, 1, 3, 4};
            int[] clause3 = new int[]{3, type, 6, -3, 5,7};
                dClasses.integrateDisjointnessClause(new Clause(1,clause1));
                dClasses.integrateDisjointnessClause(new Clause(2,clause2));
                dClasses.integrateDisjointnessClause(new Clause(3,clause3));
                dClasses.integrateTrueLiteral(3,null);

            Thread thread = new Thread(dClasses::run);
            thread.start(); Thread.sleep(10);
            thread.interrupt();

            assertEquals("[-1, Disjointness True Literals:\n" +
                    "D-1: 1!=2!=3 and true(3) -> -1, -2, Disjointness True Literals:\n" +
                    "D-1: 1!=2!=3 and true(3) -> -2, -4, Disjointness True Literals:\n" +
                    "D-2: 1!=3!=4 and true(3) -> -4]",observed.toString());
            assertEquals("Disjointness Clauses of Problem test:\n" +
                    "D-3: 5!=6!=7",dClasses.toString());
    }

    @Test
    public void integrateEquivalence1() {
        System.out.println("integrateTrueLiteral unsatisfiable");
        DisjointnessClasses dClasses = prepare(monitoring,true);
        int[] clause1 = new int[]{1, type, 1, 2, 3};
        try {
            dClasses.integrateDisjointnessClause(new Clause(1,clause1));
            dClasses.integrateEquivalence(new Clause(1,ClauseType.EQUIV,2,3));
        } catch (Unsatisfiable uns) {
            if(monitoring) System.out.println(uns);
            else System.out.println("UNSATISFIABLE");}}

    @Test
    public void integrateEquivalence2() throws Exception {
        System.out.println("integrateTrueLiteral satisfiable");
        DisjointnessClasses dClasses = prepare(monitoring,false);
        ArrayList<Clause> observed = new ArrayList<>();
        dClasses.addObserver(observed::add);
        int[] clause1 = new int[]{dClasses.problemSupervisor.nextClauseId(), type, 1, 2, 3};
        int[] clause2 = new int[]{dClasses.problemSupervisor.nextClauseId(), type, 4,5,6};
        Clause eqClause = new Clause(3,ClauseType.EQUIV,3,6);
        dClasses.integrateDisjointnessClause(new Clause(1,clause1));
        dClasses.integrateDisjointnessClause(new Clause(2,clause2));
        dClasses.equivalenceClasses.integrateEquivalence(eqClause,false);
        dClasses.integrateEquivalence(eqClause);

        Thread thread = new Thread(dClasses::run);
        thread.start(); Thread.sleep(100);
        thread.interrupt();

        assertEquals("Disjointness Clauses of Problem test:\n" +
                "D-1: 1!=2!=3\n" +
                "D-3: 3!=4!=5",dClasses.toString());
        assertEquals("[D-1: 1!=2!=3, D-2: 4!=5!=6, D-3: 3!=4!=5]", observed.toString());
        //System.out.println(dClasses.infoString(null));
        }

    @Test
    public void threadtest() throws Unsatisfiable{
        System.out.println("Thread ");
        DisjointnessClasses dClasses = prepare(monitoring,true);
        dClasses.configure();
        ArrayList<Object> observed = new ArrayList<>();
        dClasses.model.addObserver(Thread.currentThread(),
                ((literal, originals) -> {
                    observed.add(literal);
                    observed.add(originals);
                }));
        dClasses.model.symboltable = null;

        Thread thread1 = new Thread(dClasses::run);
        thread1.start();

        int[] clause1 = new int[]{1,type,2,3,4};
        dClasses.addDisjointnessClause(clause1);

        IntArrayList literals = new IntArrayList();  literals.add(5); literals.add(6);
        dClasses.addDerivedDisjoints(literals,null);
        Clause eqClause = new Clause(1,ClauseType.EQUIV,1,4);
        dClasses.equivalenceClasses.integrateEquivalence(eqClause,true);
        dClasses.model.add(6,null,null);
        //dClasses.addTrueLiteral(6,origins);

        try{Thread.sleep(100);}catch(Exception ex) {}
        thread1.interrupt();
        try{thread1.join();} catch(Exception ex) {}

        assertEquals("Disjointness Clauses of Problem test:\n" +
                "D-3: 1!=2!=3",dClasses.toString());
        assertEquals("[6, null, -5, Disjointness True Literals:\n" +
                "D-2: 5!=6 and true(6) -> -5]",observed.toString());
        //System.out.println(dClasses.infoString(null));
        //System.out.println(dClasses.model.infoString(false));
    }

}