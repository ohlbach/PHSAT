package Datastructures.Theory;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.Connective;
import Datastructures.Results.Result;
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


public class EquivalenceClassesTest {

    static StringBuilder errors=new StringBuilder();
    static StringBuilder warnings=new StringBuilder();

    boolean monitoring = false;

    int type= Connective.EQUIV.ordinal();


    EquivalenceClasses prepare(boolean monitoring, boolean withSymboltable) {
        GlobalParameters globalParameters=new GlobalParameters();
        globalParameters.monitor=!monitoring ? null : new Monitor(null,"mixed",errors,warnings);
        HashMap<String,Object> problemParameters=new HashMap<>();
        problemParameters.put("name","test");

        Controller controller=new Controller(null,null,null);
        ProblemSupervisor problemSupervisor=new ProblemSupervisor(controller,globalParameters,problemParameters,null);
        Symboltable symboltable= null;
        if(withSymboltable) {
            symboltable = new Symboltable(10);
            symboltable.setName(1,"p");
            symboltable.setName(2,"q");
            symboltable.setName(3,"r");
            symboltable.setName(4,"a");
            symboltable.setName(5,"b");
            symboltable.setName(6,"c");}
        problemSupervisor.model=new Model(20,symboltable);
        return new EquivalenceClasses(problemSupervisor);
    }

    private Clause make(int id, int... literals) {
        return new Clause(id, Connective.EQUIV,(short)-1, IntArrayList.wrap(literals));
    }

    @Test
    public void removeDouble() throws Result {
        System.out.println("Remove Double and Inconsistency");
        EquivalenceClasses eqClasses=prepare(monitoring,true);
        Clause c1 = make(1,1,2,3);
        Clause c2 = eqClasses.removeDoublesAndInconsistencies(c1);
        assertSame(c1, c2);
        assertEquals("E-1: 1=2=3",c1.toString());

        Clause c3 = make(2,1,2,3,2);
        Clause c4 = eqClasses.removeDoublesAndInconsistencies(c3);
        assertEquals("E-2: 1=2=3",c4.toString());


        Clause c5 = make(3,1,-2,3,-2,3);
        Clause c6 = eqClasses.removeDoublesAndInconsistencies(c5);
        assertEquals("E-3: 1=-2=3",c6.toString());

        try{
            Clause c7 = make(4,1,2,3,2,-3);
            eqClasses.removeDoublesAndInconsistencies(c7);}
        catch(Unsatisfiable uns) {
            if(monitoring) {
                System.out.println(uns);
                System.out.println(uns.toString());}
            else System.out.println("Unsatisfiable");}}


    @Test
    public void replaceTruthValues() throws Result {
        System.out.println("Replace Truth Values");
        EquivalenceClasses eqClasses=prepare(monitoring,true);
        InferenceTest inf = new InferenceTest("My Test");
        Clause c1 = make(1,1,2,3);
        Clause c2 = eqClasses.replaceTruthValues(c1);
        assertSame(c1, c2);
        eqClasses.model.add(2,inf,null);
        eqClasses.replaceTruthValues(c1);
        assertEquals("Model:\n1,2,3",eqClasses.model.toNumbers());
        //System.out.println(eqClasses.model.infoString(false));
        eqClasses.model.add(5,inf,null);
        Clause c4 = make(2,4,-5,-6);
        assertNull(eqClasses.replaceTruthValues(c4));
        assertEquals("Model:\n1,2,3,-4,5,6",eqClasses.model.toNumbers());

        Clause c6 = make(3,4,5);
        try{eqClasses.replaceTruthValues(c6);}
        catch(Unsatisfiable uns) {
            if(monitoring) System.out.println(uns);
            else System.out.println("Unsatisfiable");}
    }

    @Test
    public void replaceEquivalences1() throws Result{
        System.out.println("Replace Equivalences 1");
        EquivalenceClasses eqClasses=prepare(monitoring,true);
        Clause c1 = make(1,3,2,1);
        eqClasses.integrateEquivalence(c1,false);
        //System.out.println(eqClasses.toString());
        Clause c2 = make(2,4,5,3);
        Clause c3 = eqClasses.replaceEquivalences(c2);
        assertEquals("E-1: 4=5=1",c3.toString());
        Clause c4 = make(3,4,5,-3);
        Clause c5 = eqClasses.replaceEquivalences(c4);
        assertEquals("E-2: 4=5=-1",c5.toString());
    }
    @Test
    public void replaceEquivalences2() throws Result{
        System.out.println("Replace Equivalences 2");
        EquivalenceClasses eqClasses=prepare(monitoring,true);
        Clause c1 = make(1,2,1);
        eqClasses.integrateEquivalence(c1,false);
        Clause c2 = make(2,4,5);
        eqClasses.integrateEquivalence(c2,false);

        //System.out.println(eqClasses);

        Clause c3 = make(2,1,2,3,4,-5);
        Clause c4 = eqClasses.replaceEquivalences(c3);
        assertEquals("E-2: 1=1=3=4=-4",c4.toString());
    }
    @Test
    public void joinClause() throws Result{
        System.out.println("Join Clause");
        EquivalenceClasses eqClasses = prepare(monitoring,false);
        Clause c1 = make(1, 1,2,3);
        eqClasses.integrateEquivalence(c1, false);
        Clause c2 = make(2, 2,4,5);
        Clause c3 = eqClasses.joinClause(c2);
        assertEquals("E-1: 2=4=5=1=3",c3.toString());

        eqClasses.integrateEquivalence(c1, false);
        Clause c4 = make(3, 5,-2,7);
        Clause c5 = eqClasses.joinClause(c4);
        assertEquals("E-2: 5=-2=7=-1=-3",c5.toString());

        eqClasses.integrateEquivalence(c1, false);
        Clause c6 = make(4, 5,6,-7);
        eqClasses.integrateEquivalence(c6, false);

        Clause c7 = make(5, 3,6,8,2);
        Clause c8 = eqClasses.joinClause(c7);
        assertEquals("E-4: 3=6=8=2=1=5=-7",c8.toString());
        try{
            eqClasses.integrateEquivalence(c1, false);
            Clause c9 = make(6, 3,4,-1);
            eqClasses.joinClause(c9);}
        catch(Unsatisfiable uns) {
            if(monitoring) System.out.println(uns);
            else System.out.println("Unsatisfiable"); }
    }
    @Test
    public void normalizeClause() throws Result {
        System.out.println("Normalize Clause");
        EquivalenceClasses eqClasses = prepare(monitoring, false);
        Clause c1 = make(1, 1, 2, 3);
        eqClasses.integrateEquivalence(c1, false);
        InferenceTest inf = new InferenceTest("My Test");
        eqClasses.model.add(1,inf,null);
        Clause c2 = make(2, 3,5,4,3);
        eqClasses.normalizeClause(c2);
        assertEquals("Model:\n" +
                "1,4,5",eqClasses.model.toNumbers());}


    @Test
    public void addBasicEquivalenceClause1() throws Result {
        System.out.println("Add Basic Equivalence Clause no overlaps");
        EquivalenceClasses eqClasses=prepare(monitoring,true);
        int[] clause=new int[]{1,type,1,2,3};
        eqClasses.addBasicEquivalenceClause(clause);
        assertEquals("Equivalence Classes of Problem test:\nE-1: 1=2=3",eqClasses.toString());
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-1: 1=2=3 [1]\n" +
                "Literal Index:\n" +
                " 1@1,2@1,3@1,",eqClasses.infoString(null));
        assertEquals("Equivalence Classes of Problem test:\nE-1: p=q=r",eqClasses.toString(eqClasses.symboltable));
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-1: p=q=r [1]\n" +
                "Literal Index:\n" +
                " p@1,q@1,r@1,",eqClasses.infoString(eqClasses.symboltable));

        clause=new int[]{2,type,4,-5,-6};
        eqClasses.addBasicEquivalenceClause(clause);

        assertEquals("Equivalence Classes of Problem test:\nE-1: 1=2=3\n" +
                "E-2: 4=-5=-6",eqClasses.toString());
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-1: 1=2=3 [1]\n" +
                "E-2: 4=-5=-6 [2]\n" +
                "Literal Index:\n" +
                " 1@1,2@1,3@1,4@2,-5@2,-6@2,",eqClasses.infoString(null));
        assertEquals("Equivalence Classes of Problem test:\nE-1: p=q=r\n" +
                "E-2: a=-b=-c",eqClasses.toString(eqClasses.symboltable));
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-1: p=q=r [1]\n" +
                "E-2: a=-b=-c [2]\n" +
                "Literal Index:\n" +
                " p@1,q@1,r@1,a@2,-b@2,-c@2,",eqClasses.infoString(eqClasses.symboltable));

        assertEquals(1,eqClasses.getRepresentative(2));
        assertEquals(-1,eqClasses.getRepresentative(-2));
        assertEquals(-4,eqClasses.getRepresentative(5));
        assertEquals(4,eqClasses.getRepresentative(-5));

    }

    @Test
    public void addBasicEquivalenceClause2() throws Result {
        System.out.println("Add Basic Equivalence Clause with overlaps");
        EquivalenceClasses eqClasses=prepare(monitoring,true);
        int[] clause=new int[]{1,type,2,3,4};
        eqClasses.addBasicEquivalenceClause(clause);

        clause=new int[]{2,type,-5,6,2}; // 2 overlaps
        eqClasses.addBasicEquivalenceClause(clause);
        assertEquals("Equivalence Classes of Problem test:\nE-1: 2=3=4=-5=6",eqClasses.toString());
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-1: 2=3=4=-5=6 [1, 2]\n" +
                "Literal Index:\n" +
                " 2@1,3@1,4@1,-5@1,6@1,",eqClasses.infoString(null));

        clause=new int[]{3,type,6,7,1}; // 2 overlaps
        eqClasses.addBasicEquivalenceClause(clause);
        assertEquals("Equivalence Classes of Problem test:\nE-3: 1=2=3=4=-5=6=7",eqClasses.toString());
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-3: 1=2=3=4=-5=6=7 [1, 2, 3]\n" +
                "Literal Index:\n" +
                " 1@3,2@3,3@3,4@3,-5@3,6@3,7@3,",eqClasses.infoString(null));
    }

    @Test
    public void addBasicEquivalenceClause3() throws Result {
        System.out.println("Add Basic Equivalence Clause with model ");
        EquivalenceClasses eqClasses =  prepare(monitoring,true);
        Model model=eqClasses.model;
        InferenceTest inf1 = new InferenceTest("INF TEST 1");
        model.add(2, inf1, null);
        InferenceTest inf2 = new InferenceTest("INF TEST 2");
        model.add(-3,inf2,null);
        ArrayList<Object> observed=new ArrayList<>();
        model.addObserver(Thread.currentThread(),
                ((literal, step) -> {
                    observed.add(literal);
                    observed.add(step);}));

        int[] clause=new int[]{1,type,5,2,4};
        eqClasses.addBasicEquivalenceClause(clause);
        assertEquals("Equivalence Classes of Problem test:\n",eqClasses.toString());
        assertEquals("[5, Equivalent True Literals:\n" +
                "E-1: 5=2=4 and true(2) -> true(5), 4, Equivalent True Literals:\n" +
                "E-1: 5=2=4 and true(2) -> true(4)]",observed.toString());
        observed.clear();
        clause=new int[]{2,type,6,7,-4};
        eqClasses.addBasicEquivalenceClause(clause);
        assertEquals("[-6, Equivalent True Literals:\n" +
                "E-2: 6=7=-4 and true(4) -> true(-6), -7, Equivalent True Literals:\n" +
                "E-2: 6=7=-4 and true(4) -> true(-7)]",observed.toString());
        observed.clear();
        clause=new int[]{3,type,8,-6,-2};
        try{
            eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {
            if(monitoring) System.out.println(uns);
            else System.out.println("Unsatisfiable");}}

    @Test
    public void addBasicEquivalenceClause4() throws Result{
        System.out.println("Add Basic Equivalence Clause internal error ");
        EquivalenceClasses eqClasses=prepare(monitoring,true);
        int[] clause=new int[]{1,type,2,3,4,3};
        eqClasses.addBasicEquivalenceClause(clause);
        assertEquals("Equivalence Classes of Problem test:\nE-1: 2=3=4",eqClasses.toString());
        clause=new int[]{2,type,2,3,4};
        eqClasses.addBasicEquivalenceClause(clause);
        assertEquals("Equivalence Classes of Problem test:\nE-1: 2=3=4",eqClasses.toString());

        clause=new int[]{3,type,5,3,4};
        eqClasses.addBasicEquivalenceClause(clause);
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-5: 2=3=4=5 [1, 3]\n" +
                "Literal Index:\n" +
                " 2@5,3@5,4@5,5@5,",eqClasses.infoString(null));
        clause=new int[]{4,type,6,3,-4};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {
            if(monitoring) System.out.println(uns);
            else System.out.println("Unsatisfiable");}}

    @Test
    public void integrateTrueLiteral() throws Result {
        System.out.println("Integrate True Literal ");
        EquivalenceClasses eqClasses=prepare(monitoring,true);
        Model model=eqClasses.model;
        ArrayList<Object> observed=new ArrayList<>();
        model.addObserver(Thread.currentThread(),
                ((literal, originals) -> {
                    observed.add(literal);
                    observed.add(originals);}));
        int[] clause=new int[]{1,type,2,3,4};
        eqClasses.addBasicEquivalenceClause(clause);
        clause=new int[]{2,type,5,-6};
        eqClasses.addBasicEquivalenceClause(clause);
        try{eqClasses.integrateTrueLiteral(6,null);}
        catch(Unsatisfiable uns) {
            if(monitoring) System.out.println(uns);
            else System.out.println("Unsatisfiable");}
        assertEquals("[-5, Equivalent True Literals:\n" +
                "E-2: 5=-6 and true(6) -> true(-5)]",observed.toString());
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-1: 2=3=4 [1]\n" +
                "Literal Index:\n" +
                " 2@1,3@1,4@1,",eqClasses.infoString(null));
        assertEquals("Model:\n" +
                "-5\n" +
                "Equivalent True Literals:\n" +
                "E-2: 5=-6 and true(6) -> true(-5)\n" +
                "Origins: [2]\n" +
                "Inference Steps: Clause Copy,Equivalent True Literals,",model.infoString(false));}


    public String info(ArrayList<Clause>clauses) {
        StringBuilder st = new StringBuilder();
        for(Clause clause : clauses) st.append("["+clause.infoString(0,null)).append("]");
        return st.toString(); }

    @Test
    public void addDerived1() throws Result{
        System.out.println("add derived equivalence empty ");
        EquivalenceClasses eqClasses=prepare(monitoring,true);
        ArrayList<Clause> observed=new ArrayList<>();
        eqClasses.addObserver(observed::add);
        eqClasses.integrateEquivalence(make(1,5,-3),true);
        assertEquals("Equivalence Classes of Problem test:\nE-1: 3=-5",eqClasses.toString());
        assertEquals("[E-1: 3=-5]",info(observed));
    }

    @Test
    public void addDerived2() throws Result {
        System.out.println("add derived equivalence join one ");
        EquivalenceClasses eqClasses=prepare(monitoring,true);
        ArrayList<Clause> observed=new ArrayList<>();
        eqClasses.addObserver(observed::add);

        int[] clause=new int[]{1,type,3,4,5};
        eqClasses.addBasicEquivalenceClause(clause);

        eqClasses.integrateEquivalence(make(2,-5,6),true);

        assertEquals("Equivalence Classes of Problem test:\nE-2: 3=4=5=-6",eqClasses.toString(null));
        assertEquals("[E-2: 3=4=5=-6 [1]]",info(observed));
    }
    @Test
    public void addDerived3() throws Result {
        System.out.println("add derived equivalence join two  ");
        EquivalenceClasses eqClasses=prepare(monitoring,true);
        ArrayList<Clause> observed=new ArrayList<>();
        eqClasses.addObserver(observed::add);

        int[] clause1=new int[]{1,type,3,4,5};
        int[] clause2=new int[]{2,type,6,7};
        eqClasses.addBasicEquivalenceClause(clause1);
        eqClasses.addBasicEquivalenceClause(clause2);

        eqClasses.integrateEquivalence(make(2,5,-7),true);

        assertEquals("Equivalence Classes of Problem test:\nE-4: 3=4=5=-6=-7",eqClasses.toString());
        assertEquals("[E-4: 3=4=5=-6=-7 [1, 2]]",info(observed));
    }

    @Test
    public void addDerived4() throws Result {
        System.out.println("add derived equivalence join three  ");
        EquivalenceClasses eqClasses=prepare(monitoring,true);
        eqClasses.model.symboltable = null;
        ArrayList<Clause> observed=new ArrayList<>();
        eqClasses.addObserver(observed::add);

        int[] clause1=new int[]{1,type,1,2,3};
        int[] clause2=new int[]{2,type,7,8,9};
        int[] clause3=new int[]{3,type,4,5,6};


        eqClasses.addBasicEquivalenceClause(clause1);
        eqClasses.addBasicEquivalenceClause(clause2);
        eqClasses.addBasicEquivalenceClause(clause3);
        eqClasses.integrateEquivalence(new Clause(4, null,new int[]{4,type,3,6,-9}),true);

        assertEquals("Equivalence Classes of Problem test:\nE-6: 1=2=3=4=5=6=-7=-8=-9",eqClasses.toString());
        assertEquals("[E-6: 1=2=3=4=5=6=-7=-8=-9 [1, 2, 3]]",info(observed));
    }



    @Test
    public void threadtest() throws Result {
        System.out.println("Thread ");
        EquivalenceClasses eqClasses=prepare(monitoring,true);
        Model model=eqClasses.model;
        model.symboltable = null;
        int[] clause1=new int[]{1,type,2,3,4};
        int[] clause2=new int[]{2,type,5,6,7};
        eqClasses.addBasicEquivalenceClause(clause1);
        eqClasses.addBasicEquivalenceClause(clause2);
        eqClasses.configure();

        Thread thread1=new Thread(()->eqClasses.run());
        thread1.start();

        eqClasses.addDerivedEquivalence(2,-5,null);
        model.add(2, null, null);

        try{Thread.sleep(20);}catch(Exception ex) {}
        thread1.interrupt();
        try{thread1.join();} catch(Exception ex) {}

        if(monitoring) {
            System.out.println("Result: " + eqClasses.problemSupervisor.result);
            System.out.println("CLASS " + eqClasses);
            System.out.println("MODEL " + model.toNumbers());
            System.out.println("MODEL " + model);}
        assertEquals("Model:\n2,3,4,-5,-6,-7",model.toNumbers());

    }

    @Test
    public void completeModel () throws Result {
        System.out.println("complete Model ");
        EquivalenceClasses eqClasses = prepare(monitoring, false);
        Model model = eqClasses.model;
        int[] clause1 = new int[]{1, type, 1, 2, 3};
        eqClasses.addBasicEquivalenceClause(clause1);
        model.add(2, null, null);
        eqClasses.completeModel();
        assertEquals("Model:\n" +
                "1,2,3", model.toNumbers());

        int[] clause2 = new int[]{1, type, 4, -5, 6};
        eqClasses.addBasicEquivalenceClause(clause2);
        model.add(5, null, null);
        model.add(6, null, null);
        try {eqClasses.completeModel();}
        catch (Unsatisfiable uns) {
            if(monitoring) System.out.println(uns);
            else System.out.println("Unsatisfiable");}
    }

    }