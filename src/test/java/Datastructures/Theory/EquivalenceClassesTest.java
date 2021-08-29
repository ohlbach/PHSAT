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


public class EquivalenceClassesTest {

    StringBuffer errors = new StringBuffer();
    StringBuffer warnings = new StringBuffer();
    boolean monitoring = false;

    int type = ClauseType.EQUIV.ordinal();

    GlobalParameters globalParameters = new GlobalParameters();
    Controller controller = new Controller(null,null,null);
    ProblemSupervisor problemSupervisor;
    Symboltable symboltable;

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
        problemSupervisor.model = new Model(10,symboltable);
    }


    @Test
    public void addBasicEquivalenceClause1() {
        System.out.println("Add Basic Equivalence Clause no overlaps");
        prepare();
        EquivalenceClasses eqClasses = new EquivalenceClasses(problemSupervisor);
        int[] clause = new int[]{1,type,1,2,3};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("Equivalence Classes of Problem test:\nE-1: 1 = 2 = 3",eqClasses.toString());
        assertEquals("Equivalence Classes of Problem test:\nE-1: 1 = 2 = 3 [1]",eqClasses.infoString(null));
        assertEquals("Equivalence Classes of Problem test:\nE-1: p = q = r",eqClasses.toString("",symboltable));
        assertEquals("Equivalence Classes of Problem test:\nE-1: p = q = r [1]",eqClasses.infoString(symboltable));

        clause = new int[]{2,type,4,-5,-6};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}

        assertEquals("Equivalence Classes of Problem test:\nE-1: 1 = 2 = 3\n" +
                "E-2: 4 = -5 = -6",eqClasses.toString());
        assertEquals("Equivalence Classes of Problem test:\nE-1: 1 = 2 = 3 [1]\n" +
                "E-2: 4 = -5 = -6 [2]",eqClasses.infoString(null));
        assertEquals("Equivalence Classes of Problem test:\nE-1: p = q = r\n" +
                "E-2: a = -b = -c",eqClasses.toString("",symboltable));
        assertEquals("Equivalence Classes of Problem test:\nE-1: p = q = r [1]\n" +
                "E-2: a = -b = -c [2]",eqClasses.infoString(symboltable));

        assertEquals(1,eqClasses.getRepresentative(2));
        assertEquals(-1,eqClasses.getRepresentative(-2));
        assertEquals(-4,eqClasses.getRepresentative(5));
        assertEquals(4,eqClasses.getRepresentative(-5));

        assertEquals("[1]",eqClasses.getOrigins(3).toString());
        assertEquals("[2]",eqClasses.getOrigins(5).toString());
    }

    @Test
    public void addBasicEquivalenceClause2() {
        System.out.println("Add Basic Equivalence Clause with overlaps");
        prepare();
        EquivalenceClasses eqClasses = new EquivalenceClasses(problemSupervisor);
        int[] clause = new int[]{1,type,2,3,4};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}

        clause = new int[]{2,type,-5,6,2}; // 2 overlaps
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("Equivalence Classes of Problem test:\nE-1: 2 = 3 = 4 = -5 = 6",eqClasses.toString());
        assertEquals("Equivalence Classes of Problem test:\nE-1: 2 = 3 = 4 = -5 = 6 [1, 2]",eqClasses.infoString(null));

        clause = new int[]{3,type,6,7,1}; // 2 overlaps
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("Equivalence Classes of Problem test:\nE-3: 1 = 2 = 7 = 3 = 4 = -5 = 6",eqClasses.toString());
        assertEquals("Equivalence Classes of Problem test:\nE-3: 1 = 2 = 7 = 3 = 4 = -5 = 6 [1, 2, 3]",eqClasses.infoString(null));
    }

    @Test
    public void addBasicEquivalenceClause3() {
        System.out.println("Add Basic Equivalence Clause with model ");
        prepare();
        IntArrayList orig1 = new IntArrayList();
        orig1.add(20);
        IntArrayList orig2 = new IntArrayList();
        orig2.add(30);
        Model model = problemSupervisor.model;
        model.addImmediately(2, orig1);
        model.addImmediately(-3, orig2);
        ArrayList<Object> observed = new ArrayList<>();
        model.addObserver(null,
                ((literal, originals) -> {
                    observed.add(literal);
                    observed.add(originals);}));
        EquivalenceClasses eqClasses = new EquivalenceClasses(problemSupervisor);

        int[] clause = new int[]{1,type,5,2,4};
        try{
            eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("Equivalence Classes of Problem test:\n",eqClasses.toString());
        assertEquals("[5, [1], 4, [1]]",observed.toString());
        observed.clear();
        clause = new int[]{2,type,6,7,-4};
        try{
            eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("[-6, [2], -7, [2]]",observed.toString());
        observed.clear();
        clause = new int[]{3,type,8,-6,-2};
        try{
            eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {
            System.out.println(uns.toString());}}

    @Test
    public void addBasicEquivalenceClause4() {
        System.out.println("Add Basic Equivalence Clause internal error ");
        prepare();
        EquivalenceClasses eqClasses = new EquivalenceClasses(problemSupervisor);
        int[] clause = new int[]{1,type,2,3,4,3};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("Equivalence Classes of Problem test:\nE-1: 2 = 3 = 4",eqClasses.toString());
        clause = new int[]{2,type,2,3,4};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("Equivalence Classes of Problem test:\nE-1: 2 = 3 = 4",eqClasses.toString());

        clause = new int[]{3,type,5,3,4};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        assertEquals("Equivalence Classes of Problem test:\nE-1: 2 = 3 = 4 = 5 [1, 3]",eqClasses.infoString(null));
        clause = new int[]{4,type,6,3,-4};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {
            System.out.println(uns.toString());}}

    @Test
    public void integrateTrueLiteral() {
        System.out.println("Integrate True Literal ");
        prepare();
        Model model = problemSupervisor.model;
        ArrayList<Object> observed = new ArrayList<>();
        model.addObserver(null,
                ((literal, originals) -> {
                    observed.add(literal);
                    observed.add(originals);}));
        EquivalenceClasses eqClasses = new EquivalenceClasses(problemSupervisor);
        int[] clause = new int[]{1,type,2,3,4};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        clause = new int[]{2,type,5,-6};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}
        IntArrayList origins = new IntArrayList(); origins.add(20);
        try{eqClasses.integrateTrueLiteral(6,origins);}
        catch(Unsatisfiable uns) {if(uns != null) System.out.println(uns.toString());}
        assertEquals("[-5, [2, 20]]",observed.toString());
        assertEquals("Equivalence Classes of Problem test:\nE-1: 2 = 3 = 4 [1]",eqClasses.infoString(null));
        assertEquals("-5 @ [2, 20]",model.infoString(false));}



    @Test
    public void addDerived1() {
        System.out.println("add derived equivalence empty ");
        prepare();
        Model model = problemSupervisor.model;
        EquivalenceClasses eqClasses = new EquivalenceClasses(problemSupervisor);
        ArrayList<Object> observed = new ArrayList<>();
        eqClasses.addObserver((representative, literal, origins) ->
            {observed.add(representative); observed.add(literal);observed.add(origins);});
        IntArrayList orig = new IntArrayList();
        orig.add(20);
        try{
            eqClasses.integrateEquivalence(5,-3,orig);}
        catch(Unsatisfiable uns) {}
        assertEquals("Equivalence Classes of Problem test:\nE-1: 3 = -5",eqClasses.toString());
        assertEquals("[3, -5, [20]]",observed.toString());
    }

    @Test
    public void addDerived2() {
        System.out.println("add derived equivalence join one ");
        prepare();
        EquivalenceClasses eqClasses = new EquivalenceClasses(problemSupervisor);
        ArrayList<Object> observed = new ArrayList<>();
        eqClasses.addObserver((representative, literal, origins) ->
        {observed.add(representative); observed.add(literal);observed.add(origins);});

        int[] clause = new int[]{1,type,3,4,5};
        try{eqClasses.addBasicEquivalenceClause(clause);}
        catch(Unsatisfiable uns) {}

        IntArrayList orig = new IntArrayList();
        orig.add(20);
        try{
            eqClasses.integrateEquivalence(-5,6,orig);}
        catch(Unsatisfiable uns) {}

        assertEquals("Equivalence Classes of Problem test:\nE-1: 3 = 4 = 5 = -6",eqClasses.toString());
        assertEquals("[3, -6, [20]]",observed.toString());
    }
    @Test
    public void addDerived3() {
        System.out.println("add derived equivalence join two  ");
        prepare();
        EquivalenceClasses eqClasses = new EquivalenceClasses(problemSupervisor);
        ArrayList<Object> observed = new ArrayList<>();
        eqClasses.addObserver((representative, literal, origins) ->
        {observed.add(representative); observed.add(literal);observed.add(origins);});

        int[] clause1 = new int[]{1,type,3,4,5};
        int[] clause2 = new int[]{2,type,6,7};
        try{eqClasses.addBasicEquivalenceClause(clause1);
            eqClasses.addBasicEquivalenceClause(clause2);}
        catch(Unsatisfiable uns) {}

        IntArrayList orig = new IntArrayList();
        orig.add(20);
        try{
            eqClasses.integrateEquivalence(5,-7,orig);}
        catch(Unsatisfiable uns) {}

        assertEquals("Equivalence Classes of Problem test:\nE-1: 3 = 4 = 5 = -6 = -7",eqClasses.toString());
        assertEquals("[3, -6, [20]]",observed.toString());
    }




    @Test
    public void threadtest() {
        System.out.println("Thread ");
        prepare();
        Model model = problemSupervisor.model;
        EquivalenceClasses eqClasses = new EquivalenceClasses(problemSupervisor);
        int[] clause1 = new int[]{1,type,2,3,4};
        int[] clause2 = new int[]{2,type,5,6,7};
        try{eqClasses.addBasicEquivalenceClause(clause1);
            eqClasses.addBasicEquivalenceClause(clause2);}
            catch(Unsatisfiable uns) {}

        Thread thread1 = new Thread(()->eqClasses.run());
        thread1.start();

        IntArrayList origins = new IntArrayList(); origins.add(10);
        eqClasses.addDerivedEquivalence(2,-5,origins);
        try {
            model.add(2, null, null);
        }catch(Unsatisfiable uns) {}

        try{Thread.sleep(100);}catch(Exception ex) {}
        thread1.interrupt();
        try{thread1.join();} catch(Exception ex) {}
        System.out.println(problemSupervisor.result);
        System.out.println("CLASS " + eqClasses.toString());
        System.out.println("MODEL " + model.toNumbers());

    }
    }