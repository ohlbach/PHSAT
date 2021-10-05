package Datastructures.Theory;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import Datastructures.Literals.CLiteral;
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

    static StringBuffer errors=new StringBuffer();
    static StringBuffer warnings=new StringBuffer();
    boolean monitoring = true;

    int type=ClauseType.EQUIV.ordinal();


    EquivalenceClasses prepare(boolean monitoring) {
        GlobalParameters globalParameters=new GlobalParameters();
        globalParameters.monitor=!monitoring ? null : new Monitor(null,"mixed",errors,warnings);
        HashMap<String,Object> problemParameters=new HashMap<>();
        problemParameters.put("name","test");

        Controller controller=new Controller(null,null,null);
        ProblemSupervisor problemSupervisor=new ProblemSupervisor(controller,globalParameters,problemParameters,null);
        Symboltable symboltable=new Symboltable(10);
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"a");
        symboltable.setName(5,"b");
        symboltable.setName(6,"c");
        problemSupervisor.model=new Model(20,symboltable);
        return new EquivalenceClasses(problemSupervisor);
    }

    private Clause make(int id,int... literals) {
        return new Clause(id,ClauseType.EQUIV, IntArrayList.wrap(literals));
    }

    @Test
    public void removeDouble() throws Unsatisfiable {
        System.out.println("Remove Double and Inconsistency");
        EquivalenceClasses eqClasses=prepare(monitoring);
        Clause c1 = make(1,1,2,3);
        Clause c2 = eqClasses.removeDoublesAndInconsistencies(c1);
        assertTrue(c1 == c2);
        assertEquals("E-1: 1=2=3",c1.toString());

        Clause c3 = make(2,1,2,3,2);
        Clause c4 = eqClasses.removeDoublesAndInconsistencies(c3);
        assertEquals("E-2: 1=2=3",c4.toString());


        Clause c5 = make(3,1,-2,3,-2,3);
        Clause c6 = eqClasses.removeDoublesAndInconsistencies(c5);
        assertEquals("E-3: 1=-2=3",c6.toString());

        try{
            Clause c7 = make(4,1,2,3,2,-3);
            Clause c8 = eqClasses.removeDoublesAndInconsistencies(c7);}
        catch(Unsatisfiable uns) {
            System.out.println(uns);
            System.out.println(uns.toString(eqClasses.symboltable));}}


    @Test
    public void replaceTruthValues() throws Unsatisfiable {
        System.out.println("Replace Truth Values");
        EquivalenceClasses eqClasses=prepare(monitoring);
        InferenceTest inf = new InferenceTest("My Test");
        Clause c1 = make(1,1,2,3);
        Clause c2 = eqClasses.replaceTruthValues(c1);
        assertTrue(c1 == c2);
        eqClasses.model.add(2,inf,null);
        Clause c3 = eqClasses.replaceTruthValues(c1);
        assertEquals("Model:\n1,2,3",eqClasses.model.toNumbers());
        //System.out.println(eqClasses.model.infoString(false));
        eqClasses.model.add(5,inf,null);
        Clause c4 = make(2,4,-5,-6);
        assertNull(eqClasses.replaceTruthValues(c4));
        assertEquals("Model:\n1,2,3,-4,5,6",eqClasses.model.toNumbers());

        Clause c6 = make(3,4,5);
        try{eqClasses.replaceTruthValues(c6);}
        catch(Unsatisfiable uns) {
            System.out.println(uns.toString(eqClasses.symboltable));}
    }

    @Test
    public void replaceEquivalences1() throws Unsatisfiable{
        System.out.println("Replace Equivalences 1");
        EquivalenceClasses eqClasses=prepare(monitoring);
        InferenceTest inf = new InferenceTest("My Test");
        Clause c1 = make(1,3,2,1);
        eqClasses.integrateEquivalence(c1,false);
        //System.out.println(eqClasses.toString());
        Clause c2 = make(2,4,5,3);
        Clause c3 = EquivalenceClasses.replaceEquivalences(eqClasses,c2);
        assertEquals("E-0: 4=5=1",c3.toString());
        Clause c4 = make(3,4,5,-3);
        Clause c5 = EquivalenceClasses.replaceEquivalences(eqClasses,c4);
        assertEquals("E-1: 4=5=-1",c5.toString());
    }
    @Test
    public void replaceEquivalences2() throws Unsatisfiable{
        System.out.println("Replace Equivalences 2");
        EquivalenceClasses eqClasses=prepare(monitoring);
        InferenceTest inf = new InferenceTest("My Test");
        Clause c1 = make(1,2,1);
        eqClasses.integrateEquivalence(c1,false);
        Clause c2 = make(2,4,5);
        eqClasses.integrateEquivalence(c2,false);

        System.out.println(eqClasses.toString());

        Clause c3 = make(2,1,2,3,4,-5);
        Clause c4 = EquivalenceClasses.replaceEquivalences(eqClasses,c3);
        assertEquals("E-1: 1=1=3=4=-4",c4.toString());
    }



    @Test
    public void addBasicEquivalenceClause1() {
        System.out.println("Add Basic Equivalence Clause no overlaps");
        EquivalenceClasses eqClasses=prepare(true);
        int[] clause=new int[]{1,type,1,2,3};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("Equivalence Classes of Problem test:\nE-1: 1=2=3",eqClasses.toString());
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-1: 1=2=3 [1]\n" +
                "Literal Index:\n" +
                "  1@1\n" +
                "  2@1\n" +
                "  3@1\n",eqClasses.infoString(null));
        assertEquals("Equivalence Classes of Problem test:\nE-1: p=q=r",eqClasses.toString(eqClasses.symboltable));
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-1: p=q=r [1]\n" +
                "Literal Index:\n" +
                "  p@1\n" +
                "  q@1\n" +
                "  r@1\n",eqClasses.infoString(eqClasses.symboltable));

        clause=new int[]{2,type,4,-5,-6};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}

        assertEquals("Equivalence Classes of Problem test:\nE-1: 1=2=3\n" +
                "E-2: 4=-5=-6",eqClasses.toString());
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-1: 1=2=3 [1]\n" +
                "E-2: 4=-5=-6 [2]\n" +
                "Literal Index:\n" +
                "  1@1\n" +
                "  2@1\n" +
                "  3@1\n" +
                "  4@2\n" +
                "  -5@2\n" +
                "  -6@2\n",eqClasses.infoString(null));
        assertEquals("Equivalence Classes of Problem test:\nE-1: p=q=r\n" +
                "E-2: a=-b=-c",eqClasses.toString(eqClasses.symboltable));
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-1: p=q=r [1]\n" +
                "E-2: a=-b=-c [2]\n" +
                "Literal Index:\n" +
                "  p@1\n" +
                "  q@1\n" +
                "  r@1\n" +
                "  a@2\n" +
                "  -b@2\n" +
                "  -c@2\n",eqClasses.infoString(eqClasses.symboltable));

        assertEquals(1,eqClasses.getRepresentative(2));
        assertEquals(-1,eqClasses.getRepresentative(-2));
        assertEquals(-4,eqClasses.getRepresentative(5));
        assertEquals(4,eqClasses.getRepresentative(-5));

    }

    @Test
    public void addBasicEquivalenceClause2() {
        System.out.println("Add Basic Equivalence Clause with overlaps");
        EquivalenceClasses eqClasses=prepare(true);
        int[] clause=new int[]{1,type,2,3,4};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}

        clause=new int[]{2,type,-5,6,2}; // 2 overlaps
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("Equivalence Classes of Problem test:\nE-2: 2=3=4=-5=6",eqClasses.toString());
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-2: 2=3=4=-5=6 [1, 2]\n" +
                "Literal Index:\n" +
                "  2@2\n" +
                "  3@2\n" +
                "  4@2\n" +
                "  -5@2\n" +
                "  6@2\n",eqClasses.infoString(null));

        clause=new int[]{3,type,6,7,1}; // 2 overlaps
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("Equivalence Classes of Problem test:\nE-3: 1=2=3=4=-5=6=7",eqClasses.toString());
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-3: 1=2=3=4=-5=6=7 [1, 2, 3]\n" +
                "Literal Index:\n" +
                "  1@3\n" +
                "  2@3\n" +
                "  3@3\n" +
                "  4@3\n" +
                "  -5@3\n" +
                "  6@3\n" +
                "  7@3\n",eqClasses.infoString(null));
    }

    @Test
    public void addBasicEquivalenceClause3() {
        System.out.println("Add Basic Equivalence Clause with model ");
        EquivalenceClasses eqClasses =  prepare(true);
        IntArrayList orig1=new IntArrayList();
        orig1.add(20);
        IntArrayList orig2=new IntArrayList();
        orig2.add(30);
        Model model=eqClasses.model;
        model.addImmediately(2);
        model.addImmediately(-3);
        ArrayList<Object> observed=new ArrayList<>();
        model.addObserver(Thread.currentThread(),
                ((literal, originals) -> {
                    observed.add(literal);
                    observed.add(originals);}));

        int[] clause=new int[]{1,type,5,2,4};
        try{
            eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("Equivalence Classes of Problem test:\n",eqClasses.toString());
        assertEquals("[5, [1, 20], 4, [1, 20]]",observed.toString());
        observed.clear();
        clause=new int[]{2,type,6,7,-4};
        try{
            eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("[-6, [1, 2, 20], -7, [1, 2, 20]]",observed.toString());
        observed.clear();
        clause=new int[]{3,type,8,-6,-2};
        try{
            eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {
            System.out.println(uns.toString());}}

    @Test
    public void addBasicEquivalenceClause4() {
        System.out.println("Add Basic Equivalence Clause internal error ");
        EquivalenceClasses eqClasses=prepare(true);
        int[] clause=new int[]{1,type,2,3,4,3};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("Equivalence Classes of Problem test:\nE-1: 2=3=4",eqClasses.toString());
        clause=new int[]{2,type,2,3,4};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("Equivalence Classes of Problem test:\nE-1: 2=3=4",eqClasses.toString());

        clause=new int[]{3,type,5,3,4};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-3: 2=3=4=5 [1, 3]\n" +
                "Literal Index:\n" +
                "  2@3\n" +
                "  3@3\n" +
                "  4@3\n" +
                "  5@3\n",eqClasses.infoString(null));
        clause=new int[]{4,type,6,3,-4};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {
            System.out.println(uns.toString());}}

    @Test
    public void integrateTrueLiteral() {
        System.out.println("Integrate True Literal ");
        EquivalenceClasses eqClasses=prepare(true);
        Model model=eqClasses.model;
        ArrayList<Object> observed=new ArrayList<>();
        model.addObserver(Thread.currentThread(),
                ((literal, originals) -> {
                    observed.add(literal);
                    observed.add(originals);}));
        int[] clause=new int[]{1,type,2,3,4};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        clause=new int[]{2,type,5,-6};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        try{eqClasses.integrateTrueLiteral(6,null);}
        catch(Unsatisfiable uns) {if(uns != null) System.out.println(uns.toString());}
        assertEquals("[-5, [2, 20]]",observed.toString());
        assertEquals("Equivalence Classes of Problem test:\n" +
                "E-1: 2=3=4 [1]\n" +
                "Literal Index:\n" +
                "  2@1\n" +
                "  3@1\n" +
                "  4@1\n",eqClasses.infoString(null));
        assertEquals("-5 @ [2, 20]",model.infoString(false));}


    public Clause make(int id, int literal1, int literal2, IntArrayList origins) {
        return new Clause(id,ClauseType.EQUIV,literal1,literal2);
    }

    public String info(ArrayList<Clause>clauses) {
        StringBuilder st = new StringBuilder();
        for(Clause clause : clauses) st.append("["+clause.infoString(0,null)).append("]");
        return st.toString(); }

    @Test
    public void addDerived1() {
        System.out.println("add derived equivalence empty ");
        EquivalenceClasses eqClasses=prepare(true);
        Model model=eqClasses.model;
        ArrayList<Clause> observed=new ArrayList<>();
        eqClasses.addObserver(clause  -> observed.add(clause));
        IntArrayList orig=new IntArrayList();
        orig.add(20);
        try{
            eqClasses.integrateEquivalence(make(1,5,-3,orig),true);}
        catch(Unsatisfiable uns) {}
        assertEquals("Equivalence Classes of Problem test:\nE-1: 3=-5",eqClasses.toString());
        assertEquals("[1: 3=-5 [20]]",info(observed));
    }

    @Test
    public void addDerived2() {
        System.out.println("add derived equivalence join one ");
        EquivalenceClasses eqClasses=prepare(true);
        ArrayList<Clause> observed=new ArrayList<>();
        eqClasses.addObserver(clause -> observed.add(clause));

        int[] clause=new int[]{1,type,3,4,5};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}

        IntArrayList orig=new IntArrayList();
        orig.add(20);
        try{
            eqClasses.integrateEquivalence(make(2,-5,6,orig),true);}
        catch(Unsatisfiable uns) {}

        assertEquals("Equivalence Classes of Problem test:\nE-2: 3=4=5=-6",eqClasses.toString(null));
        assertEquals("[2: 3=4=5=-6 [1, 20]]",info(observed));
    }
    @Test
    public void addDerived3() {
        System.out.println("add derived equivalence join two  ");
        EquivalenceClasses eqClasses=prepare(true);
        ArrayList<Clause> observed=new ArrayList<>();
        eqClasses.addObserver(observed::add);

        int[] clause1=new int[]{1,type,3,4,5};
        int[] clause2=new int[]{2,type,6,7};
        try{eqClasses.addBasicEquivalenceClause(clause1);
            eqClasses.addBasicEquivalenceClause(clause2);}
        catch(Unsatisfiable uns) {}

        IntArrayList orig=new IntArrayList();
        orig.add(20);
        try{
            eqClasses.integrateEquivalence(make(2,5,-7,orig),true);}
        catch(Unsatisfiable uns) {}

        assertEquals("Equivalence Classes of Problem test:\nE-2: 3=4=5=-6=-7",eqClasses.toString());
        assertEquals("[2: 3=4=5=-6=-7 [1, 2, 20]]",info(observed));
    }

    @Test
    public void addDerived4() {
        System.out.println("add derived equivalence join three  ");
        EquivalenceClasses eqClasses=prepare(true);
        eqClasses.model.symboltable = null;
        ArrayList<Clause> observed=new ArrayList<>();
        eqClasses.addObserver(observed::add);

        int[] clause1=new int[]{1,type,1,2,3};
        int[] clause2=new int[]{2,type,7,8,9};
        int[] clause3=new int[]{3,type,4,5,6};


        try{eqClasses.addBasicEquivalenceClause(clause1);
            eqClasses.addBasicEquivalenceClause(clause2);
            eqClasses.addBasicEquivalenceClause(clause3);
            eqClasses.integrateEquivalence(new Clause(4,new int[]{4,type,3,6,-9}),true);}
        catch(Unsatisfiable uns) {}



        assertEquals("Equivalence Classes of Problem test:\nE-4: 1=2=3=4=5=6=-7=-8=-9",eqClasses.toString());
        assertEquals("[4: 1=2=3=4=5=6=-7=-8=-9 [1, 2, 3, 4]]",info(observed));
    }



    @Test
    public void threadtest() {
        System.out.println("Thread ");
        EquivalenceClasses eqClasses=prepare(true);
        Model model=eqClasses.model;
        model.symboltable = null;
        int[] clause1=new int[]{1,type,2,3,4};
        int[] clause2=new int[]{2,type,5,6,7};
        try{eqClasses.addBasicEquivalenceClause(clause1);
            eqClasses.addBasicEquivalenceClause(clause2);}
            catch(Unsatisfiable uns) {}
        eqClasses.configure();

        Thread thread1=new Thread(()->eqClasses.run());
        thread1.start();

        IntArrayList origins=new IntArrayList(); origins.add(10);
        eqClasses.addDerivedEquivalence(2,-5,null);
        try {
            model.add(2, null, null);
        }catch(Unsatisfiable uns) {}

        try{Thread.sleep(20);}catch(Exception ex) {}
        thread1.interrupt();
        try{thread1.join();} catch(Exception ex) {}

        System.out.println("Result: " + eqClasses.problemSupervisor.result);
        System.out.println("CLASS " + eqClasses.toString());
        System.out.println("MODEL " + model.toNumbers());
        System.out.println("MODEL " + model.toString());
        assertEquals("2,3,4,-5,-6,-7,",model.toNumbers());

    }
    }