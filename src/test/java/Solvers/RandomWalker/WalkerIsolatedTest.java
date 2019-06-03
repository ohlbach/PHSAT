package Solvers.RandomWalker;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Results.Result;
import Datastructures.Theory.ImplicationDAG;
import Datastructures.Theory.Model;
import Generators.SingleModelGenerator;
import Utilities.Utilities;
import org.junit.*;
import org.junit.Test;

import java.util.*;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 16.05.2019.
 */
public class WalkerIsolatedTest {

    @Test
    public void parseParameters() throws Exception {
        System.out.println("parseParameters");
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("seed","3,4");
        parameters.put("flips","10000");
        parameters.put("jumps", "10,20");
        ArrayList<HashMap<String,Object>> pars = WalkerIsolated.parseParameters(parameters,errors,warnings);
        assertEquals("[{seed=3, flips=10000, name=W1, jumps=10}, {seed=4, flips=10000, name=W2, jumps=10}, {seed=3, flips=10000, name=W3, jumps=20}, {seed=4, flips=10000, name=W4, jumps=20}]",pars.toString());
    }

    @Test
    public void help() throws Exception {
        System.out.println(WalkerIsolated.help());
    }

    private HashMap<String,Object> makeAP() {
        HashMap<String,Object> applicationParameters = new HashMap<>();
        applicationParameters.put("seed",0);
        applicationParameters.put("flips",100);
        applicationParameters.put("jumps",10);
        return applicationParameters;}

    @Test
    public void addImplications() throws Exception {
        System.out.println("addImplications");
        int predicates = 10;
        ClauseList clauses = new ClauseList(10,predicates);
        ImplicationDAG dag = new ImplicationDAG();
        dag.addClause(-1,2);
        dag.addClause(-1,3);
        dag.addClause(-2,4);
        dag.addClause(-3,4);
        dag.addClause(-4,5);
        Model model = new Model(predicates);
        WalkerIsolated walker = new WalkerIsolated(predicates,makeAP(),model,clauses, dag);
        assertTrue(clauses.toString().startsWith("P_0: (-4,5)\n" +
                "P_1: (-3,5)\n" +
                "P_2: (-2,5)\n" +
                "P_3: (-1,2)\n" +
                "P_4: (-1,3)\n" +
                "P_5: (-1,4)\n" +
                "P_6: (-1,5)"));
       // System.out.println(clauses);
       // System.out.println(dag);
    }

    @Test
    public void initializeModel() throws Exception {
        System.out.println("initializeModel");
        int predicates = 10;
        ClauseList clauses = new ClauseList(10, 10);
        clauses.addClause(Utilities.makeClause("1","1,-3"));
        clauses.addClause(Utilities.makeClause("2","1,-3,4"));
        clauses.addClause(Utilities.makeClause("3","-1,3,4"));
        Model model = new Model(predicates);
        WalkerIsolated walker = new WalkerIsolated(predicates,makeAP(),model,clauses, null);
        walker.initializeModel();
        assertEquals("1,-3,4,",walker.rwModel.toString());
    }

    @Test
    public void initializeScores1() throws Exception {
        System.out.println("initializeScores1");
        int predicates = 10;
        ClauseList clauses = new ClauseList(10, predicates);
        clauses.addClause(Utilities.makeClause("1","1,-3"));
        clauses.addClause(Utilities.makeClause("2","1,-3,4"));
        clauses.addClause(Utilities.makeClause("3","-1,3,4"));
        Model model = new Model(predicates);
        model.add(1); model.add(-3); model.add(-4);
        WalkerIsolated walker = new WalkerIsolated(predicates,makeAP(),model,clauses, null);
        //System.out.println(walker.rwModel.toString());
        walker.initializeModel();
        walker.initializeScores();
        assertEquals("[0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0]", Arrays.toString(walker.flipScores));
        assertEquals("[3: (-1,3,4)]",walker.falseClauses.toString());
        //System.out.println(walker.predicateQueue.toString());
    }

    @Test
    public void initializeScores2() throws Exception {
        System.out.println("initializeScores2");
        int predicates = 5;
        ClauseList clauses = new ClauseList(10, predicates);
        clauses.addClause(Utilities.makeClause("1","1,2,3,4"));
        clauses.addClause(Utilities.makeClause("2","-1,-2,3,4"));
        clauses.addClause(Utilities.makeClause("3","1,-2,-3,4"));
        clauses.addClause(Utilities.makeClause("4","1,2,-3,-4"));
        clauses.addClause(Utilities.makeClause("5","-1,-2,3,-4"));
        clauses.addClause(Utilities.makeClause("6","1,-2,-3,-4"));
        clauses.addClause(Utilities.makeClause("7","-1,-2,-3,-4"));
        Model model = new Model(predicates);
        model.add(1); model.add(2); model.add(3); model.add(4);
        WalkerIsolated walker = new WalkerIsolated(predicates,makeAP(),model,clauses, null);
        //System.out.println(walker.rwModel.toString());
        walker.initializeModel();
        walker.initializeScores();
        assertEquals("[0, 0, 1, 0, 1, 0]",Arrays.toString(walker.flipScores));
        assertEquals("[7: (-1,-2,-3,-4)]",walker.falseClauses.toString());
        //System.out.println(walker.predicateQueue.toString());
    }

    @Test
    public void initializeScores3() throws Exception {
        System.out.println("initializeScores3");
        int predicates = 5;
        ClauseList clauses = new ClauseList(10, predicates);
        clauses.addClause(Utilities.makeClause("1","1,-2,-3,-4"));
        clauses.addClause(Utilities.makeClause("2","-1,2,-3,-4"));
        clauses.addClause(Utilities.makeClause("3","-1,-2,3,-4"));
        clauses.addClause(Utilities.makeClause("4","-1,-2,-3,4"));
        Model model = new Model(predicates);
        model.add(1); model.add(2); model.add(3); model.add(4);
        WalkerIsolated walker = new WalkerIsolated(predicates,makeAP(),model,clauses, null);
        //System.out.println(walker.rwModel.toString());
        walker.initializeModel();
        walker.initializeScores();
        System.out.println(walker.toString());
        assertEquals("[0, -1, -1, -1, -1, 0]",Arrays.toString(walker.flipScores));
        assertEquals("[]",walker.falseClauses.toString());
        System.out.println(walker.predicateQueue.toString());
    }

    @Test
    public void updateClauseScore() throws Exception {
        System.out.println("update clause score");
        int predicates = 5;
        ClauseList clauses = new ClauseList(10, predicates);
        Clause c = Utilities.makeClause("1","1,2,3,4");
        Model model = new Model(predicates);
        WalkerIsolated walker = new WalkerIsolated(predicates,makeAP(),model,clauses, null);
        walker.updateClauseScore(c,1);
        //System.out.println(walker.falseClauses.toString());
        assertEquals("[1: (1,2,3,4)]",walker.falseClauses.toString());
        //System.out.println(Arrays.toString(walker.flipScores));
        assertEquals("[0, 1, 1, 1, 1, 0]",Arrays.toString(walker.flipScores));
        walker.updateClauseScore(c,-1);
        assertEquals("[]",walker.falseClauses.toString());
        assertEquals("[0, 0, 0, 0, 0, 0]",Arrays.toString(walker.flipScores));
        // one literal is true.
        walker.rwModel.makeTrue(2);
        walker.updateClauseScore(c,1);
        assertEquals("[]",walker.falseClauses.toString());
        assertEquals("[0, 0, -1, 0, 0, 0]",Arrays.toString(walker.flipScores));
        walker.updateClauseScore(c,-1);
        assertEquals("[]",walker.falseClauses.toString());
        assertEquals("[0, 0, 0, 0, 0, 0]",Arrays.toString(walker.flipScores));
        // two literals are true
        walker.rwModel.makeTrue(3);
        walker.updateClauseScore(c,1);
        assertEquals("[]",walker.falseClauses.toString());
        assertEquals("[0, 0, 0, 0, 0, 0]",Arrays.toString(walker.flipScores));
        walker.updateClauseScore(c,-1);
        assertEquals("[]",walker.falseClauses.toString());
        assertEquals("[0, 0, 0, 0, 0, 0]",Arrays.toString(walker.flipScores));
    }

    @Test
    public void predicateQueue() throws Exception {
        System.out.println("predicate queue");
        int predicates = 5;
        ClauseList clauses = new ClauseList(10, predicates);
        Model model = new Model(predicates);
        WalkerIsolated walker = new WalkerIsolated(predicates, makeAP(), model, clauses, null);
        walker.changeScore(1, 5);
        assertEquals(1, (int) walker.predicateQueue.peek());
        walker.changeScore(2, 10);
        assertEquals(2, (int) walker.predicateQueue.peek());
        walker.changeScore(1, 20);
        assertEquals(1, (int) walker.predicateQueue.peek());
    }

    @Test
    public void flipPredicate() throws Exception {
        System.out.println("flip predicate");
        int predicates = 5;
        ClauseList clauses = new ClauseList(10, predicates);
        Model model = new Model(predicates);
        WalkerIsolated walker = new WalkerIsolated(predicates, makeAP(), model, clauses, null);
        Clause c = Utilities.makeClause("1", "1,2,3");
        clauses.addClause(c);
        walker.rwModel.makeFalse(1);
        walker.rwModel.makeFalse(2);
        walker.rwModel.makeFalse(3);
        walker.updateClauseScore(c, 1);
        assertEquals("[0, 1, 1, 1, 0, 0]", Arrays.toString(walker.flipScores));
        assertEquals(1, (int) walker.predicateQueue.peek());
        walker.flipPredicate(1);
        assertEquals("[0, -1, 0, 0, 0, 0]", Arrays.toString(walker.flipScores));
        walker.flipPredicate(2);
        assertEquals("[0, 0, 0, 0, 0, 0]", Arrays.toString(walker.flipScores));
        walker.flipPredicate(1);
        assertEquals("[0, 0, -1, 0, 0, 0]", Arrays.toString(walker.flipScores));
        assertEquals(1, (int) walker.predicateQueue.peek());
    }

    @Test
    public void selectFlipPredicate() throws Exception {
        System.out.println("select flip");
        int predicates = 10;
        ClauseList clauses = new ClauseList(10, predicates);
        Model model = new Model(predicates);
        WalkerIsolated walker = new WalkerIsolated(predicates, makeAP(), model, clauses, null);
        walker.flipCounter = 1;
        walker.changeScore(1, 5);
        walker.changeScore(2, -5);
        walker.changeScore(3, 2);
        assertEquals(1, (int) walker.predicateQueue.peek());
        assertEquals(1,walker.selectFlipPredicate());
        assertEquals("[1, 2, 3]",walker.predicateQueue.toString());
        assertEquals(3,walker.selectFlipPredicate());
        assertEquals(3,walker.selectFlipPredicate());
        assertEquals(1,walker.selectFlipPredicate());

        Clause c = Utilities.makeClause("1", "-10,-12,-13");
        walker.falseClauses.add(c);
        walker.jumpFrequency = 1;
        int fp = walker.selectFlipPredicate();
        assertTrue(fp >= 10 && fp <= 13);
    }

    @Test
    public void solve1() throws Exception {
        System.out.println("solve1");
        int predicates = 10;
        ClauseList clauses = new ClauseList(10, predicates);
        Model model = new Model(predicates);
        clauses.addClause(Utilities.makeClause("1", "1,2"));
        clauses.addClause(Utilities.makeClause("2", "1,-2"));
        clauses.addClause(Utilities.makeClause("1", "-1,2"));
        WalkerIsolated walker = new WalkerIsolated(predicates, makeAP(), model, clauses, null);
        walker.solve();
        assertEquals("1,2,",walker.rwModel.toString());
    }

    @Test
    public void solve2() throws Exception {
        System.out.println("solve2 unsat");
        int predicates = 10;
        ClauseList clauses = new ClauseList(10, predicates);
        Model model = new Model(predicates);
        clauses.addClause(Utilities.makeClause("1", "1,2"));
        clauses.addClause(Utilities.makeClause("2", "1,-2"));
        clauses.addClause(Utilities.makeClause("1", "-1,2"));
        clauses.addClause(Utilities.makeClause("1", "-1,-2"));
        WalkerIsolated walker = new WalkerIsolated(predicates, makeAP(), model, clauses, null);
        walker.maxFlips = 10;
        Result result = walker.solve();
        assertEquals("Maximum number of flips: 10 reached.",result.toString());
    }

    @Test
    public void testgen() {
        HashMap<String,String> parameters = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        parameters.put("predicates","30");
        parameters.put("clauses","40");
        parameters.put("length","2");
        parameters.put("precise", "true");
        parameters.put("seed","1");
        HashMap<String,Object> map = SingleModelGenerator.parseParameters(parameters,errors,warnings).get(0);

        BasicClauseList bcl = SingleModelGenerator.generate(map,errors,warnings);
        System.out.println(bcl.testString("clauses"));
    }

    // 1,-2,-3,-4,-5,-6,-7,8,9,10,-11,-12,13,-14,15,16,17,18,19,-20,-21,22,-23,-24,-25,-26,27,28,-29,30

    @Test
    public void solve3() throws Exception {
        System.out.println("solve3 unsat");
        int predicates = 30;
        ClauseList clauses = new ClauseList(80, predicates);
        Model model = new Model(predicates);
        clauses.addClause(Utilities.makeClause("1","-1,-26"));
        clauses.addClause(Utilities.makeClause("2","17,-16"));
        clauses.addClause(Utilities.makeClause("3","-16,10"));
        clauses.addClause(Utilities.makeClause("4","21,15"));
        clauses.addClause(Utilities.makeClause("5","8,-28"));
        clauses.addClause(Utilities.makeClause("6","-19,13"));
        clauses.addClause(Utilities.makeClause("7","23,-11"));
        clauses.addClause(Utilities.makeClause("8","-4,-16"));
        clauses.addClause(Utilities.makeClause("9","-9,-7"));
        clauses.addClause(Utilities.makeClause("10","-20,-15"));
        clauses.addClause(Utilities.makeClause("11","1,-17"));
        clauses.addClause(Utilities.makeClause("12","-4,7"));
        clauses.addClause(Utilities.makeClause("13","9,6"));
        clauses.addClause(Utilities.makeClause("14","-17,9"));
        clauses.addClause(Utilities.makeClause("15","-26,-13"));
        clauses.addClause(Utilities.makeClause("16","28,2"));
        clauses.addClause(Utilities.makeClause("17","22,-9"));
        clauses.addClause(Utilities.makeClause("18","13,-19"));
        clauses.addClause(Utilities.makeClause("19","16,-30"));
        clauses.addClause(Utilities.makeClause("20","-1,-6"));
        clauses.addClause(Utilities.makeClause("21","28,-15"));
        clauses.addClause(Utilities.makeClause("22","6,-11"));
        clauses.addClause(Utilities.makeClause("23","-27,-2"));
        clauses.addClause(Utilities.makeClause("24","1,23"));
        clauses.addClause(Utilities.makeClause("25","-19,-25"));
        clauses.addClause(Utilities.makeClause("26","-8,-4"));
        clauses.addClause(Utilities.makeClause("27","12,8"));
        clauses.addClause(Utilities.makeClause("28","-17,-24"));
        clauses.addClause(Utilities.makeClause("29","18,11"));
        clauses.addClause(Utilities.makeClause("30","-20,24"));
        clauses.addClause(Utilities.makeClause("31","7,-21"));
        clauses.addClause(Utilities.makeClause("32","20,17"));
        clauses.addClause(Utilities.makeClause("33","15,-9"));
        clauses.addClause(Utilities.makeClause("34","26,22"));
        clauses.addClause(Utilities.makeClause("35","16,-15"));
        clauses.addClause(Utilities.makeClause("36","-17,22"));
        clauses.addClause(Utilities.makeClause("37","-29,-22"));
        clauses.addClause(Utilities.makeClause("38","-23,11"));
        clauses.addClause(Utilities.makeClause("39","-20,23"));
        clauses.addClause(Utilities.makeClause("40","23,-14"));

        WalkerIsolated walker = new WalkerIsolated(predicates, makeAP(), model, clauses, null);
        walker.maxFlips = 10;
        Result result = walker.solve();
        System.out.println(result);
        System.out.println(walker);

    }


    }