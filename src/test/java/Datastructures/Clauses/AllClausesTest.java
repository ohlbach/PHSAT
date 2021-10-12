package Datastructures.Clauses;

import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Datastructures.TwoLiteral.TwoLitClause;
import Datastructures.TwoLiteral.TwoLitClauses;
import InferenceSteps.InferenceTest;
import Management.Controller;
import Management.GlobalParameters;
import Management.Monitor;
import Management.ProblemSupervisor;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import java.util.HashMap;

import static org.junit.Assert.*;

public class AllClausesTest {
    StringBuffer errors = new StringBuffer();
    StringBuffer warnings = new StringBuffer();
    boolean monitoring = false;

    int typeOR = ClauseType.OR.ordinal();
    int typeXOR = ClauseType.XOR.ordinal();
    int typeDISJ = ClauseType.DISJOINT.ordinal();
    int typeEQ = ClauseType.EQUIV.ordinal();


    private AllClauses prepare(boolean monitoring, boolean withSymboltable) {
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
        problemSupervisor.clauseCounter = 9;
        problemSupervisor.basicClauseList = new BasicClauseList();
        problemSupervisor.model = new Model(20,symboltable);
        problemSupervisor.equivalenceClasses = new EquivalenceClasses(problemSupervisor);
        problemSupervisor.disjointnessClasses =  new DisjointnessClasses(problemSupervisor);
        problemSupervisor.twoLitClauses = new TwoLitClauses(problemSupervisor);
        return new AllClauses(problemSupervisor);
    }

    private Clause make(int id,int... literals) {
        return new Clause(id,ClauseType.OR, IntArrayList.wrap(literals));}

    @Test
    public void replaceDoublesAndTautologies() {
        System.out.println("replaceDoublesAndTautologies");
        AllClauses allClauses = prepare(monitoring,true);
        Clause clause1 = make(1,1,2,3);
        assertEquals("1: 1,2,3",allClauses.replaceDoublesAndTautologies(clause1).toString());
        Clause clause2 = make(2,1,2,1,3,2);
        assertEquals("2: 1,2,3",allClauses.replaceDoublesAndTautologies(clause2).toString());
        Clause clause3 = make(2,1,2,1,3,-2);
        assertNull(allClauses.replaceDoublesAndTautologies(clause3));
    }
    @Test
    public void replaceTruthValues() throws Unsatisfiable {
        System.out.println("replaceTruthValues");
        AllClauses allClauses = prepare(monitoring, true);
        Clause clause1 = make(1, 1, 2, 3);
        assertEquals("1: 1,2,3", allClauses.replaceTruthValues(clause1).toString());
        allClauses.model.add(2,new InferenceTest("my test 1"),null);
        assertNull(allClauses.replaceTruthValues(clause1));
        Clause clause2 = make(2, 1, -2, 3);
        assertEquals("10: 1,3", allClauses.replaceTruthValues(clause2).toString());
        allClauses.model.add(4,new InferenceTest("my test 2"),null);
        Clause clause3 = make(3, 1, -2, 3,-4,5);
        assertEquals("12: 1,3,5", allClauses.replaceTruthValues(clause3).toString());
    }
    @Test
    public void isSubsumed() {
        System.out.println("isSubsumed");
        AllClauses allClauses = prepare(monitoring, true);
        Clause clause1 = make(1, 1, 2, 3);
        allClauses.insertClause(clause1);
        Clause clause2 = make(2, 1, -2, 3);
        assertFalse(allClauses.isSubsumed(clause2));
        Clause clause3 = make(3, 3,2,1);
        assertTrue(allClauses.isSubsumed(clause3));
        Clause clause4 = make(4, 3,-4,2,1);
        assertTrue(allClauses.isSubsumed(clause4));
    }
    @Test
    public void removeSubsumedClauses() {
        System.out.println("isSubsumed");
        AllClauses allClauses = prepare(monitoring, true);
        Clause clause1 = make(1, 1, 2, 3, 4 );
        allClauses.insertClause(clause1);
        Clause clause2 = make(2, 2, 3, 4, 5);
        allClauses.insertClause(clause2);
        Clause clause3 = make(3, 2, 3, -4, 5);
        allClauses.insertClause(clause3);

        Clause clause4 = make(4, 2, 3, 4);
        allClauses.removeSubsumedClauses(clause4);
        assertEquals("All Clauses of Problem test:\n" +
                "3: 2,3,-4,5",allClauses.toNumbers());
    }
    @Test
    public void replacementResolutionBackwards1() throws Result {
        System.out.println("replacementResolutionBackwards 1");
        AllClauses allClauses = prepare(monitoring, true);
        Clause clause1 = make(1, 1, 2, 3 );
        allClauses.insertClause(clause1);
        Clause clause2 = make(2, 1, -2, 4 );
        assertSame(clause2,allClauses.replacementResolutionBackwards(clause2));
        Clause clause3 = make(3, 1, -2 );
        assertSame(clause3,allClauses.replacementResolutionBackwards(clause3));
        Clause clause4 = make(4, 1, -2 ,3);
        Clause clause5 = allClauses.replacementResolutionBackwards(clause4);
        assertEquals("10: 1,3",clause5.toNumbers());
        Clause clause6 = make(5, 1, -2 ,3,4);
        Clause clause7 = allClauses.replacementResolutionBackwards(clause6);
        assertEquals("11: 1,3,4",clause7.toNumbers());
    }

    @Test
    public void replacementResolutionBackwards2() throws Result {
        System.out.println("replacementResolutionBackwards 2");
        AllClauses allClauses = prepare(monitoring, true);
        Clause clause1 = make(1, 1, 2, 3);
        allClauses.insertClause(clause1);
        Clause clause2 = make(2, -3,2,5);
        allClauses.insertClause(clause2);
        Clause clause3 = make(3, -1,2,-4,3,5);
        Clause clause4 = allClauses.replacementResolutionBackwards(clause3);
        assertEquals("11: 2,-4,5",clause4.toNumbers());
    }

    @Test
    public void replacementResolutionBackwards3() throws Result {
        System.out.println("replacementResolutionBackwards 3");
        AllClauses allClauses = prepare(monitoring, true);
        Clause clause1 = make(1, 1, 2, 3);
        allClauses.insertClause(clause1);
        Clause clause2 = make(2, -2, 3);
        allClauses.insertClause(clause2);
        Clause clause3 = make(3, -1, 2, 3);
        assertNull(allClauses.replacementResolutionBackwards(clause3));
        assertEquals("Model:\n3",allClauses.model.toNumbers());

    }

    @Test
    public void replacementResolutionBackwards4() throws Result {
        System.out.println("replacementResolutionBackwards 2LitClauses");
        AllClauses allClauses = prepare(monitoring, true);
        TwoLitClause clause1 = new TwoLitClause(1,-2,-3);
        allClauses.twoLitClauses.integrateClause(clause1,false);
        Clause clause2 = make(2, 1, 2, 4);
        allClauses.insertClause(clause2);
        Clause clause3 = make(3, 1, 3,4);
        Clause clause4 = allClauses.replacementResolutionBackwards(clause3);
        assertEquals("10: 1,4",clause4.toNumbers());
    }
    @Test
    public void replacementResolutionBackwards5() throws Exception {
        System.out.println("replacementResolutionBackwards 2LitClauses 2");
        AllClauses allClauses = prepare(monitoring, false);
        TwoLitClause clause1 = new TwoLitClause(1,-2,-5);
        allClauses.twoLitClauses.integrateClause(clause1,false);
        TwoLitClause clause2 = new TwoLitClause(2,5,-3);
        allClauses.twoLitClauses.integrateClause(clause2,false);
        Thread thread = new Thread(allClauses.twoLitClauses::run);
        thread.start(); Thread.sleep(20);
        thread.interrupt();
        Clause clause3 = make(2, 1, 2, 4);
        allClauses.insertClause(clause3);
        Clause clause4 = make(3, 1, 3,4,6);
        Clause clause5 = allClauses.replacementResolutionBackwards(clause4);
        assertEquals("11: 1,4,6",clause5.toNumbers());
    }
    @Test
    public void replacementResolutionBackwards6() throws Result {
        System.out.println("replacementResolutionBackwards double");
        AllClauses allClauses = prepare(monitoring, true);
        TwoLitClause clause1 = new TwoLitClause(1,-2,-3);
        allClauses.twoLitClauses.integrateClause(clause1,false);
        Clause clause2 = make(2, 1, 2, 4);
        allClauses.insertClause(clause2);
        Clause clause3 = make(3, 1, -5, 6);
        allClauses.insertClause(clause3);
        Clause clause4 = make(4, 1, 3,4,5,6);
        Clause clause5 = allClauses.replacementResolutionBackwards(clause4);
        assertEquals("11: 1,4,6",clause5.toNumbers());
    }

    @Test
    public void replacementResolutionForward1() throws Exception{
        System.out.println("replacementResolutionForward 1");
        AllClauses allClauses = prepare(monitoring, true);
        allClauses.problemSupervisor.clauseCounter = 9;
        Clause clause1 = make(1, 1, 2, 3);
        allClauses.insertClause(clause1);
        Clause clause2 = make(2, -1, 2, 4);
        allClauses.replacementResolutionForward(clause2);
        assertEquals("All Clauses of Problem test:\n" +
                "1: 1,2,3",allClauses.toNumbers());
        Clause clause3 = make(3, -1, 2, 3);
        allClauses.replacementResolutionForward(clause3);
        Thread thread = new Thread(allClauses::run);
        thread.start(); Thread.sleep(20);
        thread.interrupt();
        assertEquals("All Clauses of Problem test:\n" +
                "10: 2,3",allClauses.toNumbers());
    }
    @Test
    public void replacementResolutionForward2() throws Exception{
        System.out.println("replacementResolutionForward 2");
        AllClauses allClauses = prepare(monitoring, true);
        allClauses.problemSupervisor.clauseCounter = 9;

        TwoLitClause clause1 = new TwoLitClause(1,-2,-3);
        allClauses.twoLitClauses.integrateClause(clause1,false);
        TwoLitClause clause2 = new TwoLitClause(2,-2,-4);
        allClauses.twoLitClauses.integrateClause(clause2,false);

        Clause clause3 = make(3, 1, 3, 4, 5);
        allClauses.insertClause(clause3);
        Clause clause4 = make(4, 4, 6, 1, 5);
        allClauses.insertClause(clause4);

        Clause clause5 = make(5, 1, 2, 5);
        allClauses.replacementResolutionForward(clause5);
        Thread thread = new Thread(allClauses::run);
        thread.start(); Thread.sleep(20);
        thread.interrupt();
        assertEquals("All Clauses of Problem test:\n" +
                "10: 1,4,5\n" +
                "11: 6,1,5",allClauses.toNumbers());
    }
    @Test
    public void replacementResolutionForward3() throws Exception{
        System.out.println("replacementResolutionForward unit");
        AllClauses allClauses = prepare(monitoring, true);
        allClauses.problemSupervisor.clauseCounter = 9;
        Clause clause1 = make(1, 1, 2);
        allClauses.insertClause(clause1);
        Clause clause2 = make(2, -1, 2);
        allClauses.replacementResolutionForward(clause2);
        assertTrue(allClauses.isEmpty());
        assertEquals("Model:\n2",allClauses.model.toNumbers());
    }

    @Test
    public void replacementResolutionForward4() throws Exception{
        System.out.println("replacementResolutionForward 4");
        AllClauses allClauses = prepare(monitoring, true);
        allClauses.problemSupervisor.clauseCounter = 9;
        Clause clause1 = make(1, 1, 2, 4, 5);
        allClauses.insertClause(clause1);
        Clause clause2 = make(2, 4, 2, 1, 6);
        allClauses.insertClause(clause2);

        Clause clause3 = make(3, 1, -2, 4);
        allClauses.replacementResolutionForward(clause3);
        Thread thread = new Thread(allClauses::run);
        thread.start(); Thread.sleep(20);
        thread.interrupt();
        assertEquals("All Clauses of Problem test:\n" +
                "10: 1,4,5\n" +
                "11: 4,1,6",allClauses.toNumbers());
    }

    @Test
    public void replacementResolutionTwo1() throws Exception {
        System.out.println("replacementResolutionForward Two 1");
        AllClauses allClauses = prepare(monitoring, false);
        allClauses.problemSupervisor.clauseCounter = 9;
        TwoLitClause clause1 = new TwoLitClause(1,-2,-3);
        Clause clause2 = make(2, 1, 2, 4, 5);
        allClauses.insertClause(clause2);
        Clause clause3 = make(3, 1, 3, 4, 5);
        allClauses.insertClause(clause3);
        Clause clause4 = make(4, 1, 2, 6);
        allClauses.insertClause(clause4);
        Clause clause5 = make(5, 1, 3, 6, 7);
        allClauses.insertClause(clause5);
        allClauses.replacementResolutionTwo(clause1);
        Thread thread = new Thread(allClauses::run);
        thread.start(); Thread.sleep(20);
        thread.interrupt();
        assertEquals("All Clauses of Problem test:\n" +
                "4:  1,2,6\n" +
                "10: 1,6,7\n" +
                "11: 1,4,5",allClauses.toNumbers());}


    }