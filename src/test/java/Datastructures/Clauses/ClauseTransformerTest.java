package Datastructures.Clauses;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceTest;
import Management.Controller;
import Management.GlobalParameters;
import Management.Monitor;
import Management.ProblemSupervisor;
import org.junit.Test;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import Utilities.Utilities;

import static org.junit.Assert.*;

public class ClauseTransformerTest {

    boolean monitoring = false;

    private ProblemSupervisor prepare() {
        HashMap<String,Object> problemParameters = new HashMap<>();
        Controller controller = new Controller(null,null,null);
        GlobalParameters globalParameters=new GlobalParameters();
        globalParameters.trackReasoning = true;
        problemParameters.put("name","test");
        ProblemSupervisor problemSupervisor = new ProblemSupervisor(controller,globalParameters,problemParameters,null);
        problemSupervisor.clauseCounter = 10;
        Symboltable symboltable = new Symboltable(10);
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"s");
        symboltable.setName(5,"t");
        symboltable.setName(6,"u");
        symboltable.setName(7,"v");
        symboltable.setName(8,"w");
        symboltable.setName(9,"x");
        problemSupervisor.model = new Model(10,symboltable);
        problemSupervisor.equivalenceClasses = new EquivalenceClasses(problemSupervisor);
        return problemSupervisor;}

    StringBuilder errors = new StringBuilder();
    StringBuilder warnings = new StringBuilder();
    int ctA = ClauseType.AND.ordinal();
    int ctO = ClauseType.OR.ordinal();
    int ctE = ClauseType.EQUIV.ordinal();
    int ctL = ClauseType.ATLEAST.ordinal();
    int ctM = ClauseType.ATMOST.ordinal();
    int ctEx = ClauseType.EXACTLY.ordinal();

    @Test
    public void analyseAnd() throws Unsatisfiable {
        System.out.println("analyse And");
        ProblemSupervisor problemSupervisor = prepare();
        Model model = problemSupervisor.model;
        Monitor monitor = monitoring ? new Monitor(null,"true",errors,warnings) : null;
        ClauseTransformer ct = new ClauseTransformer(problemSupervisor,monitor);
        Clause c1 = new Clause( new int[]{1,ctA,1,2,3});
        assertNull(ct.analyseAnd(c1, "test", null));
        assertEquals("Model:\n" +
                "p,q,r",model.toString());

        model.clear();
        c1 = new Clause( new int[]{2,ctA,1,2,1});
        assertNull(ct.analyseAnd(c1,"test",null));
        assertEquals("Model:\n" +
                "p,q",problemSupervisor.model.toString());

        model.clear();
        try {c1 = new Clause( new int[]{3,ctA,1,2,-1});
            assertNull(ct.analyseAnd(c1,"test",null));}
        catch(Unsatisfiable uns) {
            if(monitoring) System.out.println(uns);}
    }

    @Test
    public void analyseOr() throws Unsatisfiable {
        System.out.println("analyse Or");
        ProblemSupervisor problemSupervisor = prepare();
        Model model = problemSupervisor.model;
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null,"true",errors,warnings) : null;
        ClauseTransformer ct = new ClauseTransformer(problemSupervisor,monitor);
        Clause c1 = new Clause( new int[]{1,ctO,1});
        assertNull(ct.analyseOr(c1, "test", null));
        assertEquals("Model:\np", model.toString());

        model.clear();
        c1 = new Clause( new int[]{2,ctO,2,-2});
        assertNull(ct.analyseOr(c1, "test", null));
        assertEquals("", model.toString());

        c1 = new Clause( new int[]{3,ctO,2,2});
        assertNull(ct.analyseOr(c1, "test", null));
        assertEquals("Model:\nq", model.toString());

        c1 = new Clause( new int[]{4,ctO,1,2,3});
        assertNull(ct.analyseOr(c1, "test", null));

        model.add(4,new InferenceTest("IT"),null);
        c1 = new Clause( new int[]{5,ctO,1,-2,3,-4});
        Clause c2  = ct.analyseOr(c1, "test", null);
        assertEquals("11: 1,3",c2.toString());

        model.clear();
        c1 = new Clause( new int[]{6,ctO,1,2,-3,4,1,-3,4,1});
        c2 = ct.analyseOr(c1, "test", null);
        assertEquals("6: 1,2,-3,4",c2.toString());
        assertEquals("6: p,q,-r,s",c2.toString(0,model.symboltable));

        model.clear();
        eqc.addBasicEquivalenceClause(new int[]{7,ctE,1,2,3});
        c1 = new Clause( new int[]{7,ctO,1,2,3});
        assertNull(ct.analyseOr(c1, "test", null));
        assertEquals("Model:\np", model.toString());

        model.clear();
        c1 = new Clause( new int[]{8,ctO,1,2,-3});
        assertNull(ct.analyseOr(c1, "test", null));
        assertTrue(model.isEmpty());
    }


    @Test
    public void atleastToCNF()  {
        System.out.println("atleast to CNF");
        ProblemSupervisor problemSupervisor = prepare();
        Model model = problemSupervisor.model;
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null, "true", errors, warnings) : null;
        ClauseTransformer ct = new ClauseTransformer(problemSupervisor, monitor);
        Clause c1 = new Clause( new int[]{1,ctL,3,1,2,3,4,2});
        ArrayList<Clause> cnf = ct.atLeastToCNF(c1,"test 1");
        assertEquals("[11: 1,2, 12: 1,3,4, 13: 2,3, 14: 2,4]",cnf.toString());
        Clause c2 = new Clause( new int[]{2,ctL,2,1,2,3,4,5,6});
        cnf = ct.atLeastToCNF(c2,"test 1");
        for(Clause c : cnf) System.out.println(c.toString(0, model.symboltable));
        System.out.println(Utilities.over(4,3));}


    @Test
    public void analyseEquiv() throws Unsatisfiable{
        System.out.println("analyse Equiv");
        ProblemSupervisor problemSupervisor = prepare();
        Model model = problemSupervisor.model;
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null,"true",errors,warnings) : null;
        ClauseTransformer ct = new ClauseTransformer(problemSupervisor,monitor);
        Clause c1 = new Clause( new int[]{1,ctE,1});
        assertNull(ct.analyseEquiv(c1, "test 1", null));

        c1 = new Clause( new int[]{2,ctE,1,2,1,2,3,1});
        Clause c2 = ct.analyseEquiv(c1, "test 2", null);
        assertEquals("E-2: p=q=r",c2.toString(0,model.symboltable));

        c1 = new Clause( new int[]{3,ctE,1,2,1,-2,3,1});
        try{ct.analyseEquiv(c1, "test 3", null);}
        catch(Unsatisfiable uns) {if(monitoring) System.out.println(uns);}

        model.add(2,new InferenceTest("IT 1"),null);
        c1 = new Clause( new int[]{4,ctE,1,2,-3});
        assertNull(ct.analyseEquiv(c1, "test 4", null));
        assertEquals("Model:\np,q,-r",model.toString());

        model.clear();
        model.add(2,new InferenceTest("IT 2"),null);
        c1 = new Clause( new int[]{5,ctE,1,-2,-3});
        assertNull(ct.analyseEquiv(c1, "test 5", null));
        assertEquals("Model:\n-p,q,r",model.toString());

        model.clear();
        model.add(2,new InferenceTest("IT 2"),null);
        model.add(3,new InferenceTest("IT 3"),null);
        c1 = new Clause( new int[]{5,ctE,1,2,-3});
        try{ct.analyseEquiv(c1, "test 6", null);}
        catch(Unsatisfiable uns) {if(monitoring) System.out.println(uns);}

        model.clear();
        eqc.addBasicEquivalenceClause(new int[]{7,ctE,1,2,3});

        c1 = new Clause( new int[]{8,ctE,2,3,4});
        c2 = ct.analyseEquiv(c1, "test 7", null);
        assertEquals("E-11: 1=4",c2.toNumbers());

        c1 = new Clause( new int[]{9,ctE,2,3,1});
        assertNull( ct.analyseEquiv(c1, "test 8", null));
    }

    @Test
    public void analyseAtleast() throws Unsatisfiable{
        System.out.println("analyse Atleast");
        ProblemSupervisor problemSupervisor = prepare();
        Model model = problemSupervisor.model;
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null,"true",errors,warnings) : null;
        ClauseTransformer ct = new ClauseTransformer(problemSupervisor,monitor);
        Clause c1 = new Clause( new int[]{1,ctL,0});
        assertNull(ct.analyseAtleast(c1, "test 1", null));

        c1 = new Clause( new int[]{2,ctL,1,2,3,4});
        Clause c2 = ct.analyseAtleast(c1, "test 2", null);
        assertEquals("2: 2,3,4",c2.toNumbers());

        c1 = new Clause( new int[]{3,ctL,4,1,2,3});
        assertNull(ct.analyseAtleast(c1, "test 3", null));
        assertEquals("Model:\np,q,r",model.toString());

        model.clear();
        model.add(2,new InferenceTest("Test 4"),null);
        c1 = new Clause( new int[]{4,ctL,3,1,2,3,-4});
        c2 = ct.analyseAtleast(c1, "test 4", null);
        assertEquals("L-12: ATLEAST 2: 1,3,-4",c2.toNumbers());

        model.clear();
        model.add(2,new InferenceTest("Test 5"),null);
        model.add(4,new InferenceTest("Test 6"),null);
        c1 = new Clause( new int[]{5,ctL,3,1,2,3,4,5});
        c2 = ct.analyseAtleast(c1, "test 5", null);
        assertEquals("13: 1,3,5",c2.toNumbers());

        model.clear();
        c1 = new Clause( new int[]{6,ctL,3,2,3,4,-2,5});
        c2 = ct.analyseAtleast(c1, "test 7", null);
        assertEquals("L-14: ATLEAST 2: 3,4,5",c2.toNumbers());


        c1 = new Clause( new int[]{7,ctL,4,2,3,4,-2,5,-3,6});
        c2 = ct.analyseAtleast(c1, "test 8", null);
        assertEquals("L-16: ATLEAST 2: 4,5,6",c2.toNumbers());

        c1 = new Clause( new int[]{8,ctL,3,2,3,4,-2,5,-3,6});
        c2 = ct.analyseAtleast(c1, "test 9", null);
        assertEquals("18: 4,5,6",c2.toNumbers());

        eqc.addBasicEquivalenceClause(new int[]{100,ctE,1,2,3});
        c1 = new Clause( new int[]{9,ctL,3,3,4,-3,5,6});
        c2 = ct.analyseAtleast(c1, "test 10", null);
        assertEquals("L-20: ATLEAST 2: 4,5,6",c2.toNumbers());

        model.clear();
        c1 = new Clause( new int[]{10,ctL,2,5,6,5,5});
        assertNull(ct.analyseAtleast(c1, "test 11", null));
        assertEquals("Model:\n5",model.toNumbers());

        model.clear();
        eqc.addBasicEquivalenceClause(new int[]{101,ctE,5,6,7,8});
        c1 = new Clause( new int[]{11,ctL,2,7,8,9,6});
        assertNull(ct.analyseAtleast(c1, "test 12", null));
        assertEquals("Model:\n5",model.toNumbers());
    }

    @Test
    public void analyseAtmost() throws Unsatisfiable {
        System.out.println("analyse Atmost");
        ProblemSupervisor problemSupervisor = prepare();
        Model model = problemSupervisor.model;
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null, "true", errors, warnings) : null;
        ClauseTransformer ct = new ClauseTransformer(problemSupervisor, monitor);

        Clause c1 = new Clause(new int[]{1, ctM, 0, 1, 2});
        assertNull(ct.analyseAtmost(c1, "test 1", null));
        assertEquals("Model:\n-1,-2", model.toNumbers());
        model.clear();

        c1 = new Clause(new int[]{2, ctM, 3, 1, 2, -1, 3, -2, 4, 5});
        Clause c2 = ct.analyseAtmost(c1, "test 1", null);
        assertEquals("M-12: ATMOST 1: 3,4,5", c2.toNumbers());

        model.add(2,new InferenceTest("test 2"),null);
        model.add(3,new InferenceTest("test 3"),null);

        c1 = new Clause(new int[]{3, ctM, 3, 1,2,-3,4,5});
        c2 = ct.analyseAtmost(c1, "test 2", null);
        assertEquals("M-13: ATMOST 2: 1,4,5",c2.toNumbers());

        c1 = new Clause(new int[]{4, ctM, 1, 1,2,-3,4,5});
        assertNull(ct.analyseAtmost(c1, "test 3", null));
        assertEquals("Model:\n-1,2,3,-4,-5",model.toNumbers());

        model.clear();
        c1 = new Clause(new int[]{5, ctM, 2, 1,2,3,1,2,3,1,2,3,4,5,6,5});
        c2 = ct.analyseAtmost(c1, "test 4", null);
        assertEquals("M-15: ATMOST 2: 4,5,6,5",c2.toNumbers());
        assertEquals("Model:\n-1,-2,-3",model.toNumbers());

        model.clear();
        c1 = new Clause(new int[]{6, ctM, 2, 1,2,3,1,2,3,1,2,3,4});
        assertNull(ct.analyseAtmost(c1, "test 5", null));
        assertEquals("Model:\n-1,-2,-3",model.toNumbers());
    }

    @Test
    public void atmostToCNF()  {
        System.out.println("atmost to CNF");
        ProblemSupervisor problemSupervisor = prepare();
        Model model = problemSupervisor.model;
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null, "true", errors, warnings) : null;
        ClauseTransformer ct = new ClauseTransformer(problemSupervisor, monitor);
        Clause c1 = new Clause( new int[]{1,ctM,2,1,2,3});
        ArrayList<Clause> cnf = ct.atmostToCNF(c1,"test 1");
        assertEquals("[11: -1,-2,-3]",cnf.toString());

        c1 = new Clause( new int[]{2,ctM,2,1,2,3,4});
        cnf = ct.atmostToCNF(c1,"test 2");
        assertEquals("[12: -1,-2,-3, 13: -1,-2,-4, 14: -1,-3,-4, 15: -2,-3,-4]",cnf.toString());

        c1 = new Clause( new int[]{3,ctM,2,1,2,3,3});
        cnf = ct.atmostToCNF(c1,"test 3");
        assertEquals("[16: -1,-3, 17: -2,-3]",cnf.toString());

        c1 = new Clause( new int[]{4,ctM,3,1,2,3,4,5,6});
        cnf = ct.atmostToCNF(c1,"test 4");
        assertEquals("[18: -1,-2,-3,-4, 19: -1,-2,-3,-5, 20: -1,-2,-3,-6, 21: -1,-2,-4,-5, 22: -1,-2,-4,-6, 23: -1,-2,-5,-6, 24: -1,-3,-4,-5, 25: -1,-3,-4,-6, 26: -1,-3,-5,-6, 27: -1,-4,-5,-6, 28: -2,-3,-4,-5, 29: -2,-3,-4,-6, 30: -2,-3,-5,-6, 31: -2,-4,-5,-6, 32: -3,-4,-5,-6]",cnf.toString());

    }

    @Test
    public void analyseExactly() throws Unsatisfiable {
        System.out.println("analyse Exactly");
        ProblemSupervisor problemSupervisor = prepare();
        Model model = problemSupervisor.model;
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null, "true", errors, warnings) : null;
        ClauseTransformer ct = new ClauseTransformer(problemSupervisor, monitor);

        Clause c1 = new Clause(new int[]{1, ctEx, 0, 1, 2});
        assertNull(ct.analyseExactly(c1,"test 1",null));
        assertEquals("Model:\n-1,-2",model.toNumbers());
        model.clear();

        c1 = new Clause(new int[]{2, ctEx, 3, 1, 2});
        assertNull(ct.analyseExactly(c1,"test 2",null));
        assertEquals("Model:\n1,2",model.toNumbers());
        model.clear();

        model.add(2,new InferenceTest("Test 1"),null);
        model.add(3,new InferenceTest("Test 2"),null);
        c1 = new Clause(new int[]{3, ctEx, 3, 1, 2,3,4});
        Clause c2 = ct.analyseExactly(c1,"test 3",null);
        assertEquals("X-11: EXACTLY 1: 1,4",c2.toNumbers());
        model.clear();

        eqc.addBasicEquivalenceClause(new int[]{4,ctE,1,2,3});
        c1 = new Clause(new int[]{4, ctEx, 2, 1, 2,3,4,5,6});
        c2 = ct.analyseExactly(c1,"test 4",null);
        assertEquals("X-13: EXACTLY 2: 4,5,6",c2.toNumbers());

    }

    @Test
    public void exactlyToCNF()  {
        System.out.println("exactly to CNF");
        ProblemSupervisor problemSupervisor = prepare();
        Model model = problemSupervisor.model;
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null, "true", errors, warnings) : null;
        ClauseTransformer ct = new ClauseTransformer(problemSupervisor, monitor);
        Clause c1 = new Clause( new int[]{1,ctEx,2,1,2,3});
        ArrayList<Clause> cnf = ct.exactlyToCNF(c1,"test 1");
        assertEquals("[11: 1,2, 12: 1,3, 13: 2,3, 14: -1,-2,-3]",cnf.toString());

        c1 = new Clause( new int[]{1,ctEx,2,1,2,2});
        cnf = ct.exactlyToCNF(c1,"test 2");
        assertEquals("[15: 2, 16: -1,-2]",cnf.toString());

        c1 = new Clause( new int[]{1,ctEx,2,1,2,3,4});
        cnf = ct.exactlyToCNF(c1,"test 3");
        assertEquals("[17: 1,2,3, 18: 1,2,4, 19: 1,3,4, 20: 2,3,4, 21: -1,-2,-3, 22: -1,-2,-4, 23: -1,-3,-4, 24: -2,-3,-4]",cnf.toString());

        c1 = new Clause( new int[]{1,ctEx,3,1,2,3,1,2,3});
        long t1 = System.nanoTime();
        cnf = ct.exactlyToCNF(c1,"test 3");
        System.out.println(System.nanoTime()-t1);
        System.out.println(cnf.size());
        for(Clause c : cnf) System.out.println(c.toNumbers());

        //System.out.println(Utilities.over(20,11));
        //System.out.println(Utilities.over(5,4));
    }

    }