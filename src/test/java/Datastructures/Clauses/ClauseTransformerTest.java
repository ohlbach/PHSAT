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

import java.util.HashMap;

import static org.junit.Assert.*;

public class ClauseTransformerTest {

    boolean monitoring = true;

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
        Model model = new Model(10,symboltable);
        problemSupervisor.model = model;
        EquivalenceClasses equivalenceClasses = new EquivalenceClasses(problemSupervisor);
        problemSupervisor.equivalenceClasses = equivalenceClasses;
        return problemSupervisor;}

    StringBuilder errors = new StringBuilder();
    StringBuilder warnings = new StringBuilder();
    int ctA = ClauseType.AND.ordinal();
    int ctO = ClauseType.OR.ordinal();
    int ctE = ClauseType.EQUIV.ordinal();

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
        try{c2 = ct.analyseEquiv(c1, "test 3", null);}
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
    public void analyseAtleast() {
    }
}