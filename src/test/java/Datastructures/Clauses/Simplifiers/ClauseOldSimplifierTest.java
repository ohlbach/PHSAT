package Datastructures.Clauses.Simplifiers;

import Datastructures.Clauses.ClauseOld;
import Datastructures.Clauses.Connective;
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

public class ClauseOldSimplifierTest {
    boolean monitoring = false;

    private ProblemSupervisor prepare() {
        HashMap<String, Object> problemParameters = new HashMap<>();
        Controller controller = new Controller(null, null, null);
        GlobalParameters globalParameters = new GlobalParameters();
        globalParameters.trackReasoning = true;
        problemParameters.put("name", "test");
        ProblemSupervisor problemSupervisor = new ProblemSupervisor(controller, globalParameters, problemParameters, null);
        problemSupervisor.clauseCounter = 10;
        Symboltable symboltable = new Symboltable(10);
        symboltable.setName(1, "p");
        symboltable.setName(2, "q");
        symboltable.setName(3, "r");
        symboltable.setName(4, "s");
        symboltable.setName(5, "t");
        symboltable.setName(6, "u");
        symboltable.setName(7, "v");
        symboltable.setName(8, "w");
        symboltable.setName(9, "x");
        problemSupervisor.model = new Model(10, symboltable);
        problemSupervisor.equivalenceClasses = new EquivalenceClasses(problemSupervisor);
        return problemSupervisor;
    }

    StringBuilder errors = new StringBuilder();
    StringBuilder warnings = new StringBuilder();
    Connective cA = Connective.AND;
    Connective cO = Connective.OR;
    Connective cE = Connective.EQUIV;
    Connective atL = Connective.ATLEAST;
    Connective atM = Connective.ATMOST;
    Connective ex = Connective.EXACTLY;


    @Test
    public void replaceEquivalences() throws Unsatisfiable {
        System.out.println("replaceEquivalences");
        ProblemSupervisor problemSupervisor = prepare();
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null, "true", errors, warnings) : null;
        ClauseSimplifierOld cs = new ClauseSimplifierOld(problemSupervisor, monitor, "test", null);
        ClauseOld c1 = new ClauseOld(1, cO, 1, 2, 3, 4, 5, 6);
        assertSame(c1, cs.replaceEquivalences(c1));
        cs.trackReasoning = true;
        problemSupervisor.equivalenceClasses.addBasicEquivalenceClause(new int[]{1, cE.ordinal(), 2, 3, 4});
        problemSupervisor.equivalenceClasses.addBasicEquivalenceClause(new int[]{1, cE.ordinal(), 5, 6});
        ClauseOld c2 = cs.replaceEquivalences(c1);
        assertEquals("11: 1,2,2,2,5,5", c2.toNumbers());
        assertEquals("1: 1,2,3,4,5,6", c1.toNumbers());
        cs.trackReasoning = false;
        ClauseOld c3 = cs.replaceEquivalences(c1);
        assertSame(c3, c1);
        assertEquals("1: 1,2,2,2,5,5", c1.toNumbers());
    }

    @Test
    public void simplifyOr() throws Unsatisfiable {
        System.out.println("simplifyOr");
        ProblemSupervisor problemSupervisor = prepare();
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null, "true", errors, warnings) : null;
        ClauseSimplifierOld cs = new ClauseSimplifierOld(problemSupervisor, monitor, "test", null);
        ClauseOld c1 = new ClauseOld(1, cO, 1, 2, 3, 4, 5, -3, 6);
        assertNull(cs.simplifyOr(c1));
        ClauseOld c2 = new ClauseOld(2, cO, 1, 2, 3, 2, 1, 3, 4, 2);
        ClauseOld c3 = cs.simplifyOr(c2);
        assertSame(c2, c3);
        assertEquals("2: 1,2,3,4", c3.toNumbers());
        try {cs.simplifyOr(new ClauseOld(3, cO));
            assertTrue(false);}
        catch (Unsatisfiable uns) {
            System.out.println(uns.toString());}
    }

    @Test
    public void simplifyOrModel() throws Unsatisfiable {
        System.out.println("simplifyOr with Model");
        ProblemSupervisor problemSupervisor = prepare();
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null, "true", errors, warnings) : null;
        Model model = problemSupervisor.model;
        ClauseSimplifierOld cs = new ClauseSimplifierOld(problemSupervisor, monitor, "test", null);
        model.add(3,new InferenceTest("test"),null);
        ClauseOld c1 = new ClauseOld(1, cO, 1, 2, 3, 4, 5, -3, 6);
        assertNull(cs.simplifyOr(c1));

        ClauseOld c2 = new ClauseOld(2, cO, 1, 2, -3, 4, 5);
        ClauseOld c3 = cs.simplifyOr(c2);
        assertEquals("11: 1,2,4,5",c3.toNumbers());

        model.add(5,new InferenceTest("test 2"),null);
        ClauseOld c4 = new ClauseOld(4, cO, 1, 2, 2, -3, 4, 4, -5, -5);
        ClauseOld c5 = cs.simplifyOr(c4);
        assertEquals("12: 1,2,4",c5.toNumbers());
        cs.trackReasoning = false;
        assertSame(c4,cs.simplifyOr(c4));
        assertEquals("4: 1,2,4",c4.toNumbers());

        cs.trackReasoning = true;
        ClauseOld c6 = new ClauseOld(6, cO, -3,-5,-3,-5,-5);
        try{cs.simplifyOr(c6);
            assertTrue(false);}
        catch(Unsatisfiable uns) {
            System.out.println(uns.toString());}   }


    @Test
    public void simplifyAnd() throws Unsatisfiable {
        System.out.println("simplifyAnd");
        ProblemSupervisor problemSupervisor = prepare();
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null, "true", errors, warnings) : null;
        ClauseSimplifierOld cs = new ClauseSimplifierOld(problemSupervisor, monitor, "test", null);
        ClauseOld c1 = new ClauseOld(1, cA, 1, -2, 3);
        cs.simplifyAnd(c1);
        assertEquals("Model:\n" +
                "1,-2,3",problemSupervisor.model.toNumbers());
        try {cs.simplifyAnd(new ClauseOld(3, cA,4,2));
            assertTrue(false);}
        catch (Unsatisfiable uns) {
            System.out.println(uns.toString());}}

    @Test
    public void simplifyAtleastDestructively() throws Unsatisfiable {
        System.out.println("simplifyAtleastDestructively");
        ProblemSupervisor problemSupervisor = prepare();
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null, "true", errors, warnings) : null;
        Model model = problemSupervisor.model;
        ClauseSimplifierOld cs = new ClauseSimplifierOld(problemSupervisor, monitor, "test", null);
        ClauseOld c1 = new ClauseOld(1, atL, 2, 1, 2, 3);
        ClauseOld c2 = cs.simplifyAtleastDestructively(c1);
        assertSame(c1,c2);
        assertEquals("L-1: ATLEAST 2: 1,2,3",c2.toNumbers());

        model.add(2,new InferenceTest("test 1"),null);
        model.add(4,new InferenceTest("test 2"),null);

        c1 = new ClauseOld(2, atL, 3, 1, 2, 3,4,5);
        c2 = cs.simplifyAtleastDestructively(c1);
        assertEquals("2: 1,3,5",c2.toNumbers());

        c1 = new ClauseOld(3, atL, 3, 1, -2, 3,-4,5,6);
        c2 = cs.simplifyAtleastDestructively(c1);
        assertEquals("L-3: ATLEAST 3: 1,3,5,6",c2.toNumbers());

        c1 = new ClauseOld(4, atL, 3, 1, -2, 3,-4,5);
        assertNull(cs.simplifyAtleastDestructively(c1));
        assertEquals("Model:\n" +
                "1,2,3,4,5",model.toNumbers());

        c1 = new ClauseOld(5, atL, 4, 6,7,6,6,7,8,9);
        c2 = cs.simplifyAtleastDestructively(c1);
        assertEquals("5: 6,7,8,9",c2.toNumbers());
        c1 = new ClauseOld(6, atL, 3, 6,7,6,6,7);
        c2 = cs.simplifyAtleastDestructively(c1);
        assertNull(c2);

        c1 = new ClauseOld(7, atL, 3, 6,7,-6,6,7);
        c2 = cs.simplifyAtleastDestructively(c1);
        assertEquals("7: 7,6",c2.toNumbers());

        c1 = new ClauseOld(8, atL, 3, 6,7,-6,6,-7,8);
        c2 = cs.simplifyAtleastDestructively(c1);
        assertEquals("8: 6,8",c2.toNumbers());

        c1 = new ClauseOld(9, atL, 4, 6,7,-6,6,-7);
        try{c2 = cs.simplifyAtleastDestructively(c1);
            assertTrue(false);}
        catch(Unsatisfiable uns) {System.out.println(uns.toString(model.symboltable));}

        c1 = new ClauseOld(10, atL, 2, 6,7,-6,-7);
        assertNull(cs.simplifyAtleastDestructively(c1));}

    @Test
    public void simplifyAtleastCloning() throws Unsatisfiable {
        System.out.println("simplifyAtleastCloning");
        ProblemSupervisor problemSupervisor = prepare();
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null, "true", errors, warnings) : null;
        Model model = problemSupervisor.model;
        ClauseSimplifierOld cs = new ClauseSimplifierOld(problemSupervisor, monitor, "test", null);
        ClauseOld c1 = new ClauseOld(1, atL, 2, 1, 2, 3);
        ClauseOld c2 = cs.simplifyAtleastCloning(c1);
        assertSame(c1, c2);
        assertEquals("L-1: ATLEAST 2: 1,2,3", c2.toNumbers());


        model.add(2,new InferenceTest("test 1"),null);
        model.add(4,new InferenceTest("test 2"),null);

        c1 = new ClauseOld(2, atL, 3, 1, 2, 3,4,5);
        c2 = cs.simplifyAtleastCloning(c1);
        assertEquals("11: 1,3,5",c2.toNumbers());

        c1 = new ClauseOld(3, atL, 3, 1, -2, 3,-4,5,6);
        c2 = cs.simplifyAtleastCloning(c1);
        assertEquals("L-12: ATLEAST 3: 1,3,5,6",c2.toNumbers());

        c1 = new ClauseOld(4, atL, 3, 1, -2, 3,-4,5);
        assertNull(cs.simplifyAtleastCloning(c1));
        assertEquals("Model:\n" +
                "1,2,3,4,5",model.toNumbers());

        c1 = new ClauseOld(5, atL, 4, 6,7,6,6,7,8,9);
        c2 = cs.simplifyAtleastCloning(c1);
        assertEquals("14: 6,7,8,9",c2.toNumbers());
        c1 = new ClauseOld(6, atL, 3, 6,7,6,6,7);
        c2 = cs.simplifyAtleastCloning(c1);
        assertNull(c2);

        c1 = new ClauseOld(7, atL, 3, 6,7,-6,6,7);
        c2 = cs.simplifyAtleastCloning(c1);
        assertEquals("16: 7,6",c2.toNumbers());

        c1 = new ClauseOld(8, atL, 3, 6,7,-6,6,-7,8);
        c2 = cs.simplifyAtleastCloning(c1);
        assertEquals("17: 6,8",c2.toNumbers());

        c1 = new ClauseOld(9, atL, 4, 6,7,-6,6,-7);
        try{c2 = cs.simplifyAtleastCloning(c1);
            assertTrue(false);}
        catch(Unsatisfiable uns) {System.out.println(uns.toString(model.symboltable));}

        c1 = new ClauseOld(10, atL, 2, 6,7,-6,-7);
        assertNull(cs.simplifyAtleastCloning(c1));
    }


    @Test
    public void simplifyAtmostDestructively() throws Unsatisfiable {
        System.out.println("simplifyAtmostDestructively");
        ProblemSupervisor problemSupervisor = prepare();
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null, "true", errors, warnings) : null;
        Model model = problemSupervisor.model;
        ClauseSimplifierOld cs = new ClauseSimplifierOld(problemSupervisor, monitor, "test", null);
        ClauseOld c1 = new ClauseOld(1, atM, 2, 1, 2, 3);
        ClauseOld c2 = cs.simplifyAtmostDestructively(c1);
        assertSame(c1,c2);
        assertEquals("M-1: ATMOST 2: 1,2,3",c2.toNumbers());


        model.add(2,new InferenceTest("test 1"),null);
        model.add(4,new InferenceTest("test 2"),null);

        c1 = new ClauseOld(2, atM, 3, 1, 2, 3,4,5);
        c2 = cs.simplifyAtmostDestructively(c1);
        assertEquals("M-2: ATMOST 1: 1,3,5",c2.toNumbers());

        c1 = new ClauseOld(3, atM, 3, 1, -2, 3,-4,5,6);
        c2 = cs.simplifyAtmostDestructively(c1);
        assertEquals("M-3: ATMOST 3: 1,3,5,6",c2.toNumbers());

        c1 = new ClauseOld(4, atM, 2, 1, 2, 3,4,5);
        assertNull(cs.simplifyAtmostDestructively(c1));
        assertEquals("Model:\n" +
                "-1,2,-3,4,-5",model.toNumbers());

        c1 = new ClauseOld(5, atM, 4, 6,7,6,6,7,8,9);
        c2 = cs.simplifyAtmostDestructively(c1);
        assertEquals("M-5: ATMOST 1: 6,7,8,9",c2.toNumbers());
        c1 = new ClauseOld(6, atM, 3, 6,7,6,6,7);
        c2 = cs.simplifyAtmostDestructively(c1);
        assertNull(c2);
        assertEquals("Model:\n" +
                "-1,2,-3,4,-5,-6,-7",model.toNumbers());

        c1 = new ClauseOld(7, atM, 3, 6,7,-6,6,7);
        c2 = cs.simplifyAtmostDestructively(c1);
        assertNull(c2);


        c1 = new ClauseOld(8, atM, 3, 6,7,-6,6,-7,8);
        c2 = cs.simplifyAtmostDestructively(c1);
        assertNull(c2);

        c1 = new ClauseOld(9, atM, 4, 6,7,-6,6,-7);
        assertNull(cs.simplifyAtmostDestructively(c1));

        c1 = new ClauseOld(10, atM, 2, 6,7,-6,-7);
        assertNull(cs.simplifyAtmostDestructively(c1));}



    @Test
    public void simplifyAtmostCloning() throws Unsatisfiable {
        System.out.println("simplifyAtmostCloning");
        ProblemSupervisor problemSupervisor = prepare();
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null, "true", errors, warnings) : null;
        Model model = problemSupervisor.model;
        ClauseSimplifierOld cs = new ClauseSimplifierOld(problemSupervisor, monitor, "test", null);
        ClauseOld c1 = new ClauseOld(1, atM, 2, 1, 2, 3);
        ClauseOld c2 = cs.simplifyAtmostCloning(c1);
        assertSame(c1,c2);
        assertEquals("M-1: ATMOST 2: 1,2,3",c2.toNumbers());


        model.add(2,new InferenceTest("test 1"),null);
        model.add(4,new InferenceTest("test 2"),null);

        c1 = new ClauseOld(2, atM, 3, 1, 2, 3,4,5);
        c2 = cs.simplifyAtmostCloning(c1);
        assertEquals("M-11: ATMOST 1: 1,3,5",c2.toNumbers());

        c1 = new ClauseOld(3, atM, 3, 1, -2, 3,-4,5,6);
        c2 = cs.simplifyAtmostCloning(c1);
        assertEquals("M-12: ATMOST 3: 1,3,5,6",c2.toNumbers());

        c1 = new ClauseOld(4, atM, 2, 1, 2, 3,4,5);
        assertNull(cs.simplifyAtmostCloning(c1));
        assertEquals("Model:\n" +
                "-1,2,-3,4,-5",model.toNumbers());

        c1 = new ClauseOld(5, atM, 4, 6,7,6,6,7,8,9);
        c2 = cs.simplifyAtmostCloning(c1);
        assertEquals("M-14: ATMOST 1: 6,7,8,9",c2.toNumbers());
        c1 = new ClauseOld(6, atM, 3, 6,7,6,6,7);
        c2 = cs.simplifyAtmostCloning(c1);
        assertNull(c2);
        assertEquals("Model:\n" +
                "-1,2,-3,4,-5,-6,-7",model.toNumbers());

        c1 = new ClauseOld(7, atM, 3, 6,7,-6,6,7);
        c2 = cs.simplifyAtmostCloning(c1);
        assertNull(c2);


        c1 = new ClauseOld(8, atM, 3, 6,7,-6,6,-7,8);
        c2 = cs.simplifyAtmostCloning(c1);
        assertNull(c2);

        c1 = new ClauseOld(9, atM, 4, 6,7,-6,6,-7);
        assertNull(cs.simplifyAtmostCloning(c1));

        c1 = new ClauseOld(10, atM, 2, 6,7,-6,-7);
        assertNull(cs.simplifyAtmostCloning(c1));}


    @Test
    public void simplifyExactlyDestructively() throws Unsatisfiable {
        System.out.println("simplifyExactlyDestructively");
        ProblemSupervisor problemSupervisor = prepare();
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null, "true", errors, warnings) : null;
        Model model = problemSupervisor.model;
        ClauseSimplifierOld cs = new ClauseSimplifierOld(problemSupervisor, monitor, "test", null);
        ClauseOld c1 = new ClauseOld(1, ex, 2, 1, 2, 3);
        ClauseOld c2 = cs.simplifyExactlyDestructively(c1);
        assertSame(c1, c2);
        assertEquals("X-1: EXACTLY 2: 1,2,3", c2.toNumbers());


        model.add(2, new InferenceTest("test 1"), null);
        model.add(4, new InferenceTest("test 2"), null);

        c1 = new ClauseOld(2, ex, 3, 1, 2, 3, 4, 5);
        c2 = cs.simplifyExactlyDestructively(c1);
        assertEquals("X-2: EXACTLY 1: 1,3,5", c2.toNumbers());

        c1 = new ClauseOld(3, ex, 3, 1, -2, 3, -4, 5, 6);
        c2 = cs.simplifyExactlyDestructively(c1);
        assertEquals("X-3: EXACTLY 3: 1,3,5,6", c2.toNumbers());

        c1 = new ClauseOld(4, ex, 2, 1, 2, 3, 4, 5);
        assertNull(cs.simplifyExactlyDestructively(c1));
        assertEquals("Model:\n" +
                "-1,2,-3,4,-5", model.toNumbers());

        c1 = new ClauseOld(5, ex, 4, 6, 7, 6, 6, 7, 8, 9);
        c2 = cs.simplifyExactlyDestructively(c1);
        assertEquals("X-5: EXACTLY 1: 6,7,8,9", c2.toNumbers());
        c1 = new ClauseOld(6, ex, 3, 6, 7, 6, 6, 7);
        c2 = cs.simplifyExactlyDestructively(c1);
        assertNull(c2);
        assertEquals("Model:\n" +
                "-1,2,-3,4,-5,-6,-7", model.toNumbers());

        c1 = new ClauseOld(7, ex, 3, 6, 7, -6, 6, 7);
        try {c2 = cs.simplifyExactlyDestructively(c1);
            assertTrue(false);}
        catch (Unsatisfiable uns) {
            System.out.println(uns.toString(model.symboltable));}

        c1 = new ClauseOld(8, ex, 3, 6, 7, -6, 6, -7, 8);
        c2 = cs.simplifyExactlyDestructively(c1);
        assertNull(c2);
    }


    @Test
    public void simplifyExactlyCloning() throws Unsatisfiable {
        System.out.println("simplifyExactlyCloning");
        ProblemSupervisor problemSupervisor = prepare();
        EquivalenceClasses eqc = problemSupervisor.equivalenceClasses;
        Monitor monitor = monitoring ? new Monitor(null, "true", errors, warnings) : null;
        Model model = problemSupervisor.model;
        ClauseSimplifierOld cs = new ClauseSimplifierOld(problemSupervisor, monitor, "test", null);
        ClauseOld c1 = new ClauseOld(1, ex, 2, 1, 2, 3);
        ClauseOld c2 = cs.simplifyExactlyCloning(c1);
        assertSame(c1, c2);
        assertEquals("X-1: EXACTLY 2: 1,2,3", c2.toNumbers());


        model.add(2, new InferenceTest("test 1"), null);
        model.add(4, new InferenceTest("test 2"), null);

        c1 = new ClauseOld(2, ex, 3, 1, 2, 3, 4, 5);
        c2 = cs.simplifyExactlyCloning(c1);
        assertEquals("X-11: EXACTLY 1: 1,3,5", c2.toNumbers());

        c1 = new ClauseOld(3, ex, 3, 1, -2, 3, -4, 5, 6);
        c2 = cs.simplifyExactlyCloning(c1);
        assertEquals("X-12: EXACTLY 3: 1,3,5,6", c2.toNumbers());

        c1 = new ClauseOld(4, ex, 2, 1, 2, 3, 4, 5);
        assertNull(cs.simplifyExactlyCloning(c1));
        assertEquals("Model:\n" +
                "-1,2,-3,4,-5", model.toNumbers());

        c1 = new ClauseOld(5, ex, 4, 6, 7, 6, 6, 7, 8, 9);
        c2 = cs.simplifyExactlyCloning(c1);
        assertEquals("X-14: EXACTLY 1: 6,7,8,9", c2.toNumbers());
        c1 = new ClauseOld(6, ex, 3, 6, 7, 6, 6, 7);
        c2 = cs.simplifyExactlyCloning(c1);
        assertNull(c2);
        assertEquals("Model:\n" +
                "-1,2,-3,4,-5,-6,-7", model.toNumbers());

        c1 = new ClauseOld(7, ex, 3, 6, 7, -6, 6, 7);
        try {c2 = cs.simplifyExactlyCloning(c1);
            assertTrue(false);}
        catch (Unsatisfiable uns) {
            System.out.println(uns.toString(model.symboltable));}

        c1 = new ClauseOld(8, ex, 3, 6, 7, -6, 6, -7, 8);
        c2 = cs.simplifyExactlyCloning(c1);
        assertNull(c2);
    }

}
