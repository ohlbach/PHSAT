package Solvers.RandomWalker;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Utilities.Utilities;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 07.05.2019.
 */
public class InitializerIsolatedTest {
    @Test
    public void initializeModel() throws Exception {
        System.out.println("initializeModel");
        ClauseList clauses = new ClauseList(10, 10);
        clauses.addClause(Utilities.makeClause("1","1,-3"));
        clauses.addClause(Utilities.makeClause("2","1,-3,4"));
        clauses.addClause(Utilities.makeClause("3","-1,3,4"));
        InitializerIsolated smi = new InitializerIsolated(clauses);
        RWModel model = new RWModel(10);
        smi.initializeModel(model);
        assertEquals("1,-3,4,",model.toString());
    }

    @Test
    public void initializeScores1() throws Exception {
        System.out.println("initializeScores1");
        ClauseList clauses = new ClauseList(10, 10);
        clauses.addClause(Utilities.makeClause("1","1,-3"));
        clauses.addClause(Utilities.makeClause("2","1,-3,4"));
        clauses.addClause(Utilities.makeClause("3","-1,3,4"));
        InitializerIsolated smi = new InitializerIsolated(clauses);
        RWModel model = new RWModel(10);
        model.makeTrue(1);
        model.makeTrue(-3);
        model.makeTrue(-4);
        int[] flipScores = new int[11];
        ArrayList<Clause > falseClauses = new ArrayList();
        HashSet<Integer> affectedPredicates = new HashSet<>();
        smi.initializeScores(model,flipScores,falseClauses,affectedPredicates);
        assertEquals("[0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0]",Arrays.toString(flipScores));
        assertEquals("[3: (-1,3,4)]",falseClauses.toString());
        assertEquals("[1, 3, 4]",affectedPredicates.toString());
        //System.out.println(Arrays.toString(flipScores));
    }

    @Test
    public void initializeScores2() throws Exception {
        System.out.println("initializeScores2");
        ClauseList clauses = new ClauseList(10, 5);
        clauses.addClause(Utilities.makeClause("1","1,2,3,4"));
        clauses.addClause(Utilities.makeClause("2","-1,-2,3,4"));
        clauses.addClause(Utilities.makeClause("3","1,-2,-3,4"));
        clauses.addClause(Utilities.makeClause("4","1,2,-3,-4"));
        clauses.addClause(Utilities.makeClause("5","-1,-2,3,-4"));
        clauses.addClause(Utilities.makeClause("6","1,-2,-3,-4"));
        clauses.addClause(Utilities.makeClause("7","-1,-2,-3,-4"));
        InitializerIsolated smi = new InitializerIsolated(clauses);
        RWModel model = new RWModel(5);
        model.makeTrue(1);
        model.makeTrue(2);
        model.makeTrue(3);
        model.makeTrue(4);
        int[] flipScores = new int[5];
        ArrayList<Clause > falseClauses = new ArrayList();
        HashSet<Integer> affectedPredicates = new HashSet<>();
        smi.initializeScores(model,flipScores,falseClauses,affectedPredicates);
        assertEquals("[0, 0, 1, 0, 1]",Arrays.toString(flipScores));
        assertEquals("[7: (-1,-2,-3,-4)]",falseClauses.toString());
        assertEquals("[1, 2, 3, 4]",affectedPredicates.toString());
    }

    @Test
    public void initializeScores3() throws Exception {
        System.out.println("initializeScores3");
        ClauseList clauses = new ClauseList(10, 5);
        clauses.addClause(Utilities.makeClause("1","1,-2,-3,-4"));
        clauses.addClause(Utilities.makeClause("2","-1,2,-3,-4"));
        clauses.addClause(Utilities.makeClause("3","-1,-2,3,-4"));
        clauses.addClause(Utilities.makeClause("4","-1,-2,-3,4"));
        InitializerIsolated smi = new InitializerIsolated(clauses);
        RWModel model = new RWModel(5);
        model.makeTrue(1);
        model.makeTrue(2);
        model.makeTrue(3);
        model.makeTrue(4);
        int[] flipScores = new int[5];
        ArrayList<Clause > falseClauses = new ArrayList();
        HashSet<Integer> affectedPredicates = new HashSet<>();
        smi.initializeScores(model,flipScores,falseClauses,affectedPredicates);
        //System.out.println(Arrays.toString(flipScores));
        //System.out.println(falseClauses);
        //System.out.println(affectedPredicates);
        assertEquals("[0, -1, -1, -1, -1]",Arrays.toString(flipScores));
        assertEquals("[]",falseClauses.toString());
        assertEquals("[]",affectedPredicates.toString());
    }



}