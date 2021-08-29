package Datastructures.Theory;

import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
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
    boolean monitoring = true;

    int type = ClauseType.DISJOINT.ordinal();
    int typeEQ = ClauseType.EQUIV.ordinal();

    GlobalParameters globalParameters = new GlobalParameters();
    Controller controller = new Controller(null,null,null);
    ProblemSupervisor problemSupervisor;
    Symboltable symboltable; 
    EquivalenceClasses eqClasses;
    DisjointnessClasses dClasses;
    Model model;

    private void prepare() {
        globalParameters.monitor = !monitoring ? null : new Monitor(null,"mixed",errors,warnings);
        HashMap<String,Object> problemParameters = new HashMap<>();
        problemParameters.put("name","test");
        problemSupervisor = new ProblemSupervisor(controller,globalParameters,problemParameters,null);
        symboltable = new Symboltable(10);
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"a");
        symboltable.setName(5,"b");
        symboltable.setName(6,"c");
        model = new Model(10,symboltable);
        problemSupervisor.model = model;
        eqClasses = new EquivalenceClasses(problemSupervisor);
        problemSupervisor.equivalenceClasses = eqClasses;
        dClasses = new DisjointnessClasses(problemSupervisor);
    }

    @Test
    public void integrateDerivedDisjoints() {
        System.out.println("integrateDerivedDisjoints");
        prepare();
        IntArrayList literals = new IntArrayList();
        literals.add(1); literals.add(3);literals.add(2);
        IntArrayList origins = new IntArrayList();
        origins.add(20); origins.add(30);
        try{
        dClasses.integrateDerivedDisjoints(literals,origins);}
        catch(Unsatisfiable uns){}
        assertEquals("Disjointness Classes of Problem test:\n" +
                "D-1: p != r != q",dClasses.toString("",symboltable));
        //System.out.println(dClasses.infoString(symboltable));
    }

    @Test
    public void integrateDisjointnessClause1() {
        System.out.println("integrateDisjointnessClause1");
        prepare();
        int[] clause = new int[]{1,type,1,2,3};
        try{
        dClasses.integrateDisjointnessClause(clause,null);}
        catch(Unsatisfiable uns) {}
        assertEquals("Disjointness Classes of Problem test:\n" +
                "D-1: p != q != r",dClasses.toString("",symboltable));
        //System.out.println(dClasses.infoString(symboltable));
        assertTrue(dClasses.areDisjoint(1,3));
        assertFalse(dClasses.areDisjoint(1,-3));
        assertFalse(dClasses.areDisjoint(1,4));
        assertEquals("[1]",dClasses.getOrigins(1,2).toString());

        clause = new int[]{2,type,4,2,-4};
        try{
            dClasses.integrateDisjointnessClause(clause,null);}
        catch(Unsatisfiable uns) {}
        assertEquals("Disjointness Classes of Problem test:\n" +
                "D-1: p != q != r\n" +
                "D-2: a != q",dClasses.toString("",symboltable));
        System.out.println(dClasses.infoString(null));
        clause = new int[]{2,type,5,3,5};
        try{
            dClasses.integrateDisjointnessClause(clause,null);}
        catch(Unsatisfiable uns) {
            System.out.println(uns.toString());
        }
    }

    @Test
    public void integrateDisjointnessClause2() {
        System.out.println("integrateDisjointnessClause with model");
        prepare();
        ArrayList<Object> observed = new ArrayList<>();
        model.addObserver(null,
                ((literal, originals) -> {
                    observed.add(literal);
                    observed.add(originals);
                }));
        IntArrayList origins = new IntArrayList();
        origins.add(20);
        origins.add(30);
        model.addImmediately(2, origins);
        EquivalenceClasses eqClasses = new EquivalenceClasses(problemSupervisor);
        DisjointnessClasses dClasses = new DisjointnessClasses(problemSupervisor);
        int[] clause = new int[]{1, type, 1, 2, 3};
        try {dClasses.integrateDisjointnessClause(clause, null);
        } catch (Unsatisfiable uns) {}
        assertEquals("[-1, [1, 20, 30], -3, [1, 20, 30]]",observed.toString());
    }

    @Test
    public void integrateDisjointnessClause3() {
        System.out.println("integrateDisjointnessClause with equivalences");
        prepare();
        int[] clause = new int[]{1,typeEQ,3,2,1};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        clause = new int[]{2,type,4,2,5};
        DisjointnessClasses dClasses = new DisjointnessClasses(problemSupervisor);
        try {dClasses.integrateDisjointnessClause(clause, null);
        } catch (Unsatisfiable uns) {}
        assertEquals("Disjointness Classes of Problem test:\n" +
                "D-1: a != p != b",dClasses.toString("",symboltable));
        //System.out.println(dClasses.infoString(symboltable));
        }

    @Test
    public void forwardSubsumption() {
        System.out.println("forwardSubsumption");
        prepare();
        int[] clause1 = new int[]{1, type, 1, 2, 3, 4};
        int[] clause2 = new int[]{2, type, 3,2,1};
        try {
            dClasses.integrateDisjointnessClause(clause1, null);
            dClasses.integrateDisjointnessClause(clause2, null);
        } catch (Unsatisfiable uns) {}
        assertEquals("Disjointness Classes of Problem test:\n" +
                "D-1: 1 != 2 != 3 != 4",dClasses.toString());
        //System.out.println(dClasses.infoString(symboltable));

        int[] clause = new int[]{3,typeEQ,1,5};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        clause2 = new int[]{4, type, 3,2,5};
        try {
            dClasses.integrateDisjointnessClause(clause2, null);
        } catch (Unsatisfiable uns) {}
        assertEquals("Disjointness Classes of Problem test:\n" +
                "D-1: 1 != 2 != 3 != 4",dClasses.toString());
    }
    @Test
    public void backwardSubsumption() {
        System.out.println("backwardSubsumption");
        prepare();
        int[] clause1 = new int[]{1, type, 1, 2, 3, 4};
        int[] clause2 = new int[]{2, type, 3,2,1};
        try {
            dClasses.integrateDisjointnessClause(clause2, null);
            dClasses.integrateDisjointnessClause(clause1, null);
        } catch (Unsatisfiable uns) {}
        assertEquals("Disjointness Classes of Problem test:\n" +
                "D-2: 1 != 2 != 3 != 4",dClasses.toString());
        //System.out.println(dClasses.infoString(symboltable));

        int[] clause = new int[]{3,typeEQ,1,5};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        clause2 = new int[]{4, type, 3,2,5,4,6};
        try {
            dClasses.integrateDisjointnessClause(clause2, null);
        } catch (Unsatisfiable uns) {}
        assertEquals("Disjointness Classes of Problem test:\n" +
                "D-3: 3 != 2 != 1 != 4 != 6",dClasses.toString());
        //System.out.println(dClasses.infoString(null));
    }

    @Test
    public void resolve() {
        System.out.println("resolve");
        prepare();
        ArrayList<Object> observed = new ArrayList<>();
        model.addObserver(Thread.currentThread(),
                ((literal, originals) -> {
                    observed.add(literal);
                    observed.add(originals);
                }));
        int[] clause1 = new int[]{1, type, 1, 2, 3, 4};
        int[] clause2 = new int[]{2, type, 3, 2, -1,5};
        try {
            dClasses.integrateDisjointnessClause(clause1, null);
            dClasses.integrateDisjointnessClause(clause2, null);
        } catch (Unsatisfiable uns) {
        }
        assertEquals("[-3, [1, 2], -2, [1, 2]]",observed.toString());
        assertEquals("Disjointness Classes of Problem test:\n" +
                "D-1: 1 != 2 != 3 != 4\n" +
                "D-2: -1 != 5",dClasses.toString("",null));
       // System.out.println(dClasses.infoString(symboltable));
    }
    @Test
    public void extendNormalizedClause() {
        System.out.println("extendNormalizedClause");
        prepare();
        int[] clause1 = new int[]{1, type, 1, 2, 3};
        int[] clause2 = new int[]{2, type, 1, 3, 4};
        int[] clause3 = new int[]{3, type, 2,3,4};
        try {
            dClasses.integrateDisjointnessClause(clause1, null);
            dClasses.integrateDisjointnessClause(clause2, null);
            dClasses.integrateDisjointnessClause(clause3, null);
        } catch (Unsatisfiable uns) {
        }
        assertEquals("Disjointness Classes of Problem test:\n" +
                "D-3: 2 != 3 != 4 != 1",dClasses.toString());
     //   System.out.println(dClasses.infoString(null));


    }
        @Test
    public void integrateTrueLiteral() {
            System.out.println("integrateTrueLiteral");
            prepare();
            ArrayList<Object> observed = new ArrayList<>();
            model.addObserver(null, //Thread.currentThread(),
                    ((literal, originals) -> {
                        observed.add(literal);
                        observed.add(originals);
                    }));
            int[] clause1 = new int[]{1, type, 1, 2, 3};
            int[] clause2 = new int[]{2, type, 1, 3, 4};
            int[] clause3 = new int[]{2, type, 6, -3, 5,7};
            IntArrayList originals = new IntArrayList();
            originals.add(20);
            try {
                dClasses.integrateDisjointnessClause(clause1, null);
                dClasses.integrateDisjointnessClause(clause2, null);
                dClasses.integrateDisjointnessClause(clause3, null);
                dClasses.integrateTrueLiteral(3,originals);
            } catch (Unsatisfiable uns) {}
            assertEquals("[-1, [1, 20], -2, [1, 20], -4, [2, 20]]",observed.toString());
            assertEquals("Disjointness Classes of Problem test:\n" +
                    "D-3: 6 != 5 != 7",dClasses.toString());
    }

    @Test
    public void integrateEquivalence1() {
        System.out.println("integrateTrueLiteral unsatisfiable");
        prepare();
        int[] clause1 = new int[]{1, type, 1, 2, 3};
        IntArrayList originals = new IntArrayList();
        originals.add(20);
        try {
            dClasses.integrateDisjointnessClause(clause1, null);
            dClasses.integrateEquivalence(2,3,originals);
        } catch (Unsatisfiable uns) {
            System.out.println(uns.toString());}}

    @Test
    public void integrateEquivalence2() {
        System.out.println("integrateTrueLiteral satisfiable");
        prepare();
        ArrayList<DisjointnessClass> observed = new ArrayList<>();
        dClasses.addObserver(dClass -> observed.add(dClass));
        int[] clause1 = new int[]{1, type, 1, 2, 3};
        int[] clause2 = new int[]{1, type, 4,5,6};
        IntArrayList originals = new IntArrayList();
        originals.add(20);
        try {
            dClasses.integrateDisjointnessClause(clause1, null);
            dClasses.integrateDisjointnessClause(clause2, null);
            dClasses.integrateEquivalence(3,6,originals);
        } catch (Unsatisfiable uns) {}
        assertEquals("Disjointness Classes of Problem test:\n" +
                "D-1: 1 != 2 != 3\n" +
                "D-2: 4 != 5 != 3",dClasses.toString());
        assertEquals("[D-2: 4 != 5 != 3]", observed.toString());
        //System.out.println(dClasses.infoString(null));
        }

    @Test
    public void threadtest() throws Unsatisfiable{
        System.out.println("Thread ");
        prepare();
        ArrayList<Object> observed = new ArrayList<>();
        model.addObserver(null, //Thread.currentThread(),
                ((literal, originals) -> {
                    observed.add(literal);
                    observed.add(originals);
                }));
        model.symboltable = null;

        Thread thread1 = new Thread(()->dClasses.run());
        thread1.start();

        int[] clause1 = new int[]{1,type,2,3,4};
        dClasses.addDisjointnessClause(clause1);

        IntArrayList origins = new IntArrayList(); origins.add(10);
        IntArrayList literals = new IntArrayList();  literals.add(5); literals.add(6);
        dClasses.addDerivedDisjoints(literals,origins);
        origins = new IntArrayList(); origins.add(30);
        dClasses.addEquivalence(1,4,origins);
        origins = new IntArrayList(); origins.add(40);
        model.add(6,origins,null);
        //dClasses.addTrueLiteral(6,origins);

        try{Thread.sleep(100);}catch(Exception ex) {}
        thread1.interrupt();
        try{thread1.join();} catch(Exception ex) {}
        assertEquals("Disjointness Classes of Problem test:\n" +
                "D-1: 2 != 3 != 1",dClasses.toString());
        assertEquals("[-5, [10, 40]]",observed.toString());
        //System.out.println(dClasses.infoString(null));
        //System.out.println(model.infoString(false));
    }

}