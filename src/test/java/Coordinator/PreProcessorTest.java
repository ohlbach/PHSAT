package Coordinator;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Erraneous;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Generators.RandomClauseSetGenerator;
import Management.GlobalParameters;
import Management.ProblemSupervisor;
import Utilities.Utilities;
import javafx.scene.control.RadioMenuItem;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Random;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 02.11.2018.
 */
public class PreProcessorTest {

    static boolean monitoring = false;

    @Test
    public void prepareClauses() throws Exception {
        System.out.println("disjoints 3SAT");
        int unsolved = 0;
        int satisfied = 0;
        int unsatisfied = 0;
        int erraneous = 0;
        int from = 0;
        int to = 100;
        for(int seed = from; seed <= to; ++seed) {
            if(monitoring)System.out.println("SEED " + seed);
            HashMap<String, String> pars = new HashMap<>();
            if(monitoring){pars.put("monitor","true");}
            StringBuffer errors = new StringBuffer();
            StringBuffer warnings = new StringBuffer();
            GlobalParameters glb = new GlobalParameters(pars,errors,warnings);
            pars.put("seed",""+seed);
            pars.put("predicates","4");
            pars.put("disjunctions","8");
            pars.put("length","3");
            pars.put("precise","true");
            ArrayList<HashMap<String,Object>> rpars =  RandomClauseSetGenerator.parseParameters(pars,errors,warnings);
            if(monitoring)System.out.println(errors.toString());
            BasicClauseList bClauses = RandomClauseSetGenerator.generate(rpars.get(0),errors,warnings);
            HashMap<String,Object> probPars = new HashMap<>();
            probPars.put("name","test");
            ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
            PreProcessor prep = new PreProcessor(psu,probPars,bClauses);
            if(monitoring)System.out.println(bClauses.toString());
            Result result = prep.prepareClauses();
            if(result != null && result instanceof Erraneous) {
                System.out.println("Result SEED " + seed);
                System.out.println(result);
                System.out.println(prep.toString());
                break;}
            if(result == null) {++unsolved;}
            else{
                if(result instanceof Satisfiable) ++satisfied;
                if(result instanceof Unsatisfiable) ++unsatisfied;
                if(result instanceof Erraneous) ++erraneous;
        }}
        System.out.println(satisfied + " " + unsatisfied + " " + erraneous + " " + unsolved);
        assertEquals(76,satisfied);
        assertEquals(0,unsatisfied);
        assertEquals(0,erraneous);
        assertEquals(21,unsolved);
    }

    @Test
    public void fourSAT() throws Exception {
        System.out.println("disjoints 4SAT");
        int unsolved = 0;
        int satisfied = 0;
        int unsatisfied = 0;
        int erraneous = 0;
        int from = 0;
        int to = 100;
        for(int seed = from; seed <= to; ++seed) {
            if(monitoring)System.out.println("SEED " + seed);
            HashMap<String, String> pars = new HashMap<>();
            if(monitoring){pars.put("monitor","true");}
            StringBuffer errors = new StringBuffer();
            StringBuffer warnings = new StringBuffer();
            GlobalParameters glb = new GlobalParameters(pars,errors,warnings);
            pars.put("seed",""+seed);
            pars.put("predicates","4");
            pars.put("disjunctions","8");
            pars.put("length","4");
            pars.put("precise","false");
            ArrayList<HashMap<String,Object>> rpars =  RandomClauseSetGenerator.parseParameters(pars,errors,warnings);
            if(monitoring)System.out.println(errors.toString());
            BasicClauseList bClauses = RandomClauseSetGenerator.generate(rpars.get(0),errors,warnings);
            HashMap<String,Object> probPars = new HashMap<>();
            probPars.put("name","test");
            ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
            PreProcessor prep = new PreProcessor(psu,probPars,bClauses);
            if(monitoring)System.out.println(bClauses.toString());
            Result result = prep.prepareClauses();
            if(result != null && result instanceof Erraneous) {
                System.out.println("Result SEED " + seed);
                System.out.println(result);
                System.out.println(prep.toString());
                break;}
            if(result == null) {++unsolved;}
            else{
                if(result instanceof Satisfiable) ++satisfied;
                if(result instanceof Unsatisfiable) ++unsatisfied;
                if(result instanceof Erraneous) ++erraneous;
            }}
        System.out.println(satisfied + " " + unsatisfied + " " + erraneous + " " + unsolved);
        assertEquals(77,satisfied);
        assertEquals(22,unsatisfied);
        assertEquals(0,erraneous);
        assertEquals(2,unsolved);
    }

    @Test
    public void ThreeSatFlexible() throws Exception {
        System.out.println("3SAT with Units");
        int from = 0;
        int to = 100;
        int unsolved = 0;
        int satisfied = 0;
        int unsatisfied = 0;
        int erraneous = 0;
        for(int seed = from; seed <= to; ++seed) {
            if(monitoring) System.out.println("SEED " + seed);
            HashMap<String, String> pars = new HashMap<>();
            if(monitoring){pars.put("monitor","true");}
            StringBuffer errors = new StringBuffer();
            StringBuffer warnings = new StringBuffer();
            GlobalParameters glb = new GlobalParameters(pars,errors,warnings);
            pars.put("seed",""+seed);
            pars.put("predicates","4");
            pars.put("disjunctions","8");
            pars.put("length","3");
            pars.put("precise","false");
            ArrayList<HashMap<String,Object>> rpars =  RandomClauseSetGenerator.parseParameters(pars,errors,warnings);
            //System.out.println(errors.toString());
            BasicClauseList bClauses = RandomClauseSetGenerator.generate(rpars.get(0),errors,warnings);
            HashMap<String,Object> probPars = new HashMap<>();
            probPars.put("name","test");
            ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
            PreProcessor prep = new PreProcessor(psu,probPars,bClauses);
            if(monitoring) System.out.println(bClauses.toString());
            Result result = prep.prepareClauses();
            if(result == null) {++unsolved;}
            else{
                if(result instanceof Satisfiable) ++satisfied;
                if(result instanceof Unsatisfiable) ++unsatisfied;
                if(result instanceof Erraneous) ++erraneous;
            }
            if(result != null && result instanceof Erraneous) {
                System.out.println("Result SEED " + seed);
                System.out.println(result);
                System.out.println(prep.toString());
                break;}}
        //System.out.println(satisfied + " " + unsatisfied + " " + erraneous + " " + unsolved);
        assertEquals(56,satisfied);
        assertEquals(45,unsatisfied);
        assertEquals(0,erraneous);
        assertEquals(0,unsolved);
    }

    @Test
    public void binaryClauses() throws Exception {
        System.out.println("disjoints 2SAT");
        int from = 0;
        int to = 100;
        int unsolved = 0;
        int satisfied = 0;
        int unsatisfied = 0;
        int erraneous = 0;
        for(int seed = from; seed <= to; ++seed) {
            if(monitoring) System.out.println("SEED " + seed);
            HashMap<String, String> pars = new HashMap<>();
            if(monitoring){pars.put("monitor","true");}
            StringBuffer errors = new StringBuffer();
            StringBuffer warnings = new StringBuffer();
            GlobalParameters glb = new GlobalParameters(pars,errors,warnings);
            pars.put("seed",""+seed);
            pars.put("predicates","4");
            pars.put("disjunctions","8");
            pars.put("length","2");
            pars.put("precise","true");
            ArrayList<HashMap<String,Object>> rpars =  RandomClauseSetGenerator.parseParameters(pars,errors,warnings);
            //System.out.println(errors.toString());
            BasicClauseList bClauses = RandomClauseSetGenerator.generate(rpars.get(0),errors,warnings);
            HashMap<String,Object> probPars = new HashMap<>();
            probPars.put("name","test");
            ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
            PreProcessor prep = new PreProcessor(psu,probPars,bClauses);
            if(monitoring) System.out.println(bClauses.toString());
            Result result = prep.prepareClauses();
            if(result == null) {++unsolved;}
            else{
                if(result instanceof Satisfiable) ++satisfied;
                if(result instanceof Unsatisfiable) ++unsatisfied;
                if(result instanceof Erraneous) ++erraneous;
            }
            if(result != null && result instanceof Erraneous) {
                System.out.println("Result SEED " + seed);
                System.out.println(result);
                System.out.println(prep.toString());
                break;}}
        assertEquals(82,satisfied);
        assertEquals(19,unsatisfied);
        assertEquals(0,erraneous);
        assertEquals(0,unsolved);
    }
    @Test
    public void binaryClausesUnits() throws Exception {
        System.out.println("disjoints 2SAT with Units");
        int from = 0;
        int to = 100;
        int unsolved = 0;
        int satisfied = 0;
        int unsatisfied = 0;
        int erraneous = 0;
        for(int seed = from; seed <= to; ++seed) {
            if(monitoring) System.out.println("SEED " + seed);
            HashMap<String, String> pars = new HashMap<>();
            if(monitoring){pars.put("monitor","true");}
            StringBuffer errors = new StringBuffer();
            StringBuffer warnings = new StringBuffer();
            GlobalParameters glb = new GlobalParameters(pars,errors,warnings);
            pars.put("seed",""+seed);
            pars.put("predicates","4");
            pars.put("disjunctions","8");
            pars.put("length","2");
            pars.put("precise","false");
            ArrayList<HashMap<String,Object>> rpars =  RandomClauseSetGenerator.parseParameters(pars,errors,warnings);
            //System.out.println(errors.toString());
            BasicClauseList bClauses = RandomClauseSetGenerator.generate(rpars.get(0),errors,warnings);
            HashMap<String,Object> probPars = new HashMap<>();
            probPars.put("name","test");
            ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
            PreProcessor prep = new PreProcessor(psu,probPars,bClauses);
            if(monitoring) System.out.println(bClauses.toString());
            Result result = prep.prepareClauses();
            if(result == null) {++unsolved;}
            else{
                if(result instanceof Satisfiable) ++satisfied;
                if(result instanceof Unsatisfiable) ++unsatisfied;
                if(result instanceof Erraneous) ++erraneous;
            }
            if(result != null && result instanceof Erraneous) {
                System.out.println("Result SEED " + seed);
                System.out.println(result);
                System.out.println(prep.toString());
                break;}}
        //System.out.println(satisfied + " " + unsatisfied + " " + erraneous + " " + unsolved);
        assertEquals(40,satisfied);
        assertEquals(61,unsatisfied);
        assertEquals(0,erraneous);
        assertEquals(0,unsolved);
    }
    @Test
    public void largeClauseSets() throws Exception {
        System.out.println("large clause sets");
        int unsolved = 0;
        int satisfied = 0;
        int unsatisfied = 0;
        int erraneous = 0;
        int from = 0;
        int to = 100;
        for(int seed = from; seed <= to; ++seed) {
            if(monitoring)System.out.println("SEED " + seed);
            HashMap<String, String> pars = new HashMap<>();
            if(monitoring){pars.put("monitor","true");}
            StringBuffer errors = new StringBuffer();
            StringBuffer warnings = new StringBuffer();
            GlobalParameters glb = new GlobalParameters(pars,errors,warnings);
            pars.put("seed",""+seed);
            pars.put("predicates","20");
            pars.put("disjunctions","80");
            pars.put("length","3");
            pars.put("precise","true");
            ArrayList<HashMap<String,Object>> rpars =  RandomClauseSetGenerator.parseParameters(pars,errors,warnings);
            if(monitoring)System.out.println(errors.toString());
            BasicClauseList bClauses = RandomClauseSetGenerator.generate(rpars.get(0),errors,warnings);
            HashMap<String,Object> probPars = new HashMap<>();
            probPars.put("name","test");
            ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
            PreProcessor prep = new PreProcessor(psu,probPars,bClauses);
            if(monitoring)System.out.println(bClauses.toString());
            Result result = prep.prepareClauses();
            if(result != null && result instanceof Erraneous) {
                System.out.println("Result SEED " + seed);
                System.out.println(result);
                System.out.println(prep.toString());
                break;}
            if(result == null) {++unsolved;}
            else{
                if(result instanceof Satisfiable) ++satisfied;
                if(result instanceof Unsatisfiable) ++unsatisfied;
                if(result instanceof Erraneous) ++erraneous;
            }}
        System.out.println(satisfied + " " + unsatisfied + " " + erraneous + " " + unsolved);
        assertEquals(0,satisfied);
        assertEquals(0,unsatisfied);
        assertEquals(0,erraneous);
        assertEquals(101,unsolved);
    }

    @Test
    public void largeClauseSetsFlexible() throws Exception {
        System.out.println("large clause sets flexible");
        int unsolved = 0;
        int satisfied = 0;
        int unsatisfied = 0;
        int erraneous = 0;
        int from = 0;
        int to = 100;
        for(int seed = from; seed <= to; ++seed) {
            if(monitoring)System.out.println("SEED " + seed);
            HashMap<String, String> pars = new HashMap<>();
            if(monitoring){pars.put("monitor","true");}
            StringBuffer errors = new StringBuffer();
            StringBuffer warnings = new StringBuffer();
            GlobalParameters glb = new GlobalParameters(pars,errors,warnings);
            pars.put("seed",""+seed);
            pars.put("predicates","20");
            pars.put("disjunctions","50");
            pars.put("length","3");
            pars.put("precise","false");
            ArrayList<HashMap<String,Object>> rpars =  RandomClauseSetGenerator.parseParameters(pars,errors,warnings);
            if(monitoring)System.out.println(errors.toString());
            BasicClauseList bClauses = RandomClauseSetGenerator.generate(rpars.get(0),errors,warnings);
            HashMap<String,Object> probPars = new HashMap<>();
            probPars.put("name","test");
            ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
            PreProcessor prep = new PreProcessor(psu,probPars,bClauses);
            if(monitoring)System.out.println(bClauses.toString());
            Result result = prep.prepareClauses();
            if(result != null && result instanceof Erraneous) {
                System.out.println("Result SEED " + seed);
                System.out.println(result);
                System.out.println(prep.toString());
                break;}
            if(result == null) {++unsolved;}
            else{
                if(result instanceof Satisfiable) ++satisfied;
                if(result instanceof Unsatisfiable) ++unsatisfied;
                if(result instanceof Erraneous) ++erraneous;
            }}
        System.out.println(satisfied + " " + unsatisfied + " " + erraneous + " " + unsolved);
        assertEquals(2,satisfied);
        assertEquals(99,unsatisfied);
        assertEquals(0,erraneous);
        assertEquals(0,unsolved);
    }

    @Test
    public void longClauses() throws Exception {
        System.out.println("long clauses");
        int unsolved = 0;
        int satisfied = 0;
        int unsatisfied = 0;
        int erraneous = 0;
        int from = 0;
        int to = 100;
        for(int seed = from; seed <= to; ++seed) {
            if(monitoring)System.out.println("SEED " + seed);
            HashMap<String, String> pars = new HashMap<>();
            if(monitoring){pars.put("monitor","true");}
            StringBuffer errors = new StringBuffer();
            StringBuffer warnings = new StringBuffer();
            GlobalParameters glb = new GlobalParameters(pars,errors,warnings);
            pars.put("seed",""+seed);
            pars.put("predicates","50");
            pars.put("disjunctions","40");
            pars.put("length","10");
            pars.put("precise","true");
            ArrayList<HashMap<String,Object>> rpars =  RandomClauseSetGenerator.parseParameters(pars,errors,warnings);
            if(monitoring)System.out.println(errors.toString());
            BasicClauseList bClauses = RandomClauseSetGenerator.generate(rpars.get(0),errors,warnings);
            HashMap<String,Object> probPars = new HashMap<>();
            probPars.put("name","test");
            ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
            PreProcessor prep = new PreProcessor(psu,probPars,bClauses);
            if(monitoring)System.out.println(bClauses.toString());
            Result result = prep.prepareClauses();
            if(result != null && result instanceof Unsatisfiable) {
                System.out.println("Result SEED " + seed);
                System.out.println(result);
                System.out.println(prep.toString());
                break;}
            if(result == null) {++unsolved;}
            else{
                if(result instanceof Satisfiable) ++satisfied;
                if(result instanceof Unsatisfiable) ++unsatisfied;
                if(result instanceof Erraneous) ++erraneous;
            }}
        System.out.println(satisfied + " " + unsatisfied + " " + erraneous + " " + unsolved);
        assertEquals(101,satisfied);
        assertEquals(0,unsatisfied);
        assertEquals(0,erraneous);
        assertEquals(0,unsolved);
    }

    @Test
    public void longClausesFlexible() throws Exception {
        System.out.println("long clauses flexible");
        int unsolved = 0;
        int satisfied = 0;
        int unsatisfied = 0;
        int erraneous = 0;
        int from = 0;
        int to = 100;
        for(int seed = from; seed <= to; ++seed) {
            if(monitoring)System.out.println("SEED " + seed);
            HashMap<String, String> pars = new HashMap<>();
            if(monitoring){pars.put("monitor","true");}
            StringBuffer errors = new StringBuffer();
            StringBuffer warnings = new StringBuffer();
            GlobalParameters glb = new GlobalParameters(pars,errors,warnings);
            pars.put("seed",""+seed);
            pars.put("predicates","50");
            pars.put("disjunctions","40");
            pars.put("length","10");
            pars.put("precise","false");
            ArrayList<HashMap<String,Object>> rpars =  RandomClauseSetGenerator.parseParameters(pars,errors,warnings);
            if(monitoring)System.out.println(errors.toString());
            BasicClauseList bClauses = RandomClauseSetGenerator.generate(rpars.get(0),errors,warnings);
            HashMap<String,Object> probPars = new HashMap<>();
            probPars.put("name","test");
            ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
            PreProcessor prep = new PreProcessor(psu,probPars,bClauses);
            if(monitoring)System.out.println(bClauses.toString());
            Result result = prep.prepareClauses();
            if(result != null && result instanceof Erraneous) {
                System.out.println("Result SEED " + seed);
                System.out.println(result);
                System.out.println(prep.toString());
                break;}
            if(result == null) {++unsolved;}
            else{
                if(result instanceof Satisfiable) ++satisfied;
                if(result instanceof Unsatisfiable) ++unsatisfied;
                if(result instanceof Erraneous) ++erraneous;
            }}
        System.out.println(satisfied + " " + unsatisfied + " " + erraneous + " " + unsolved);
        assertEquals(97,satisfied);
        assertEquals(4,unsatisfied);
        assertEquals(0,erraneous);
        assertEquals(0,unsolved);
    }

    @Test
    public void addConjunction() throws Exception {
        System.out.println("addConjunction");
        HashMap<String, String> pars = new HashMap<>();
        if(monitoring){pars.put("monitor","true");}
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        GlobalParameters glb = new GlobalParameters(pars,errors,warnings);
        HashMap<String,Object> probPars = new HashMap<>();
        probPars.put("name","test");
        BasicClauseList bcl = new BasicClauseList();
        bcl.predicates = 10;
        ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
        PreProcessor prep = new PreProcessor(psu,probPars,bcl);

        Result result = prep.addConjunction(new int[]{1,ClauseType.AND.ordinal(),1,2,-3});
        assertNull(result);
        assertEquals("[1, 2, -3]",prep.model.toString());

        prep.addConjunction(new int[]{2,ClauseType.AND.ordinal(),1,2,4});
        assertEquals("[1, 2, -3, 4]",prep.model.toString());

        result = prep.addConjunction(new int[]{1,ClauseType.AND.ordinal(),5,6,-5});
        assertTrue(result instanceof Unsatisfiable);

        result = prep.addConjunction(new int[]{1,ClauseType.AND.ordinal(),-1,2});
        assertTrue(result instanceof Unsatisfiable);
        //System.out.println(result);
        //System.out.println(prep.model.toString());
    }


    @Test
    public void addDisjunctionNoIDLong() throws Exception {
        System.out.println("addDisjunction no Implication DAG, long clauses");

        HashMap<String, String> pars = new HashMap<>();
        if(monitoring){pars.put("monitor","true");}
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        GlobalParameters glb = new GlobalParameters(pars,errors,warnings);
        HashMap<String,Object> probPars = new HashMap<>();
        probPars.put("name","test");
        BasicClauseList bcl = new BasicClauseList();
        bcl.predicates = 20;
        ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
        PreProcessor prep = new PreProcessor(psu,probPars,bcl);

        Result result = prep.addDisjunction(new int[]{1,ClauseType.OR.ordinal(),1,2,-1}); // tautology
        assertNull(result);
        assertEquals("",prep.clauses.toString());


        result = prep.addDisjunction(new int[]{1,ClauseType.OR.ordinal(),1,2,-3,1,4,5,6,5}); // double literal
        assertNull(result);
        assertEquals("1: (1,2,-3,4,5,6)\n",prep.clauses.toString());
        result = prep.addDisjunction(new int[]{2,ClauseType.OR.ordinal(),1,2,-3,4,7,8});
        assertNull(result);
        assertEquals("1: (1,2,-3,4,5,6)\n" +
                "2: (1,2,-3,4,7,8)\n",prep.clauses.toString());
        result = prep.addDisjunction(new int[]{3,ClauseType.OR.ordinal(),2,1,3,4});
        assertNull(result);
        assertEquals("3: (2,1,3,4)\n" +
                        "2: (1,2,4,7,8)\n" +
                        "1: (1,2,4,5,6)\n",prep.clauses.toString());
        prep.addDisjunction(new int[]{4,ClauseType.OR.ordinal(),10,11,12,-13});
        prep.addDisjunction(new int[]{5,ClauseType.OR.ordinal(),10,11,12,-14});
        prep.addDisjunction(new int[]{6,ClauseType.OR.ordinal(),14,13,12,11,10});
        assertEquals("6: (12,11,10)\n" +
                "3: (2,1,3,4)\n" +
                "1: (1,2,4,5,6)\n" +
                "2: (1,2,4,7,8)\n",prep.clauses.toString());

        //System.out.println("CL\n"+prep.clauses.toString());
        //System.out.println(prep.statistics.toString());


    }
    @Test
    public void addDisjunctionModel() throws Exception {
        System.out.println("addDisjunction with model");

        HashMap<String, String> pars = new HashMap<>();
        if (monitoring) {
            pars.put("monitor", "true");
        }
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        GlobalParameters glb = new GlobalParameters(pars, errors, warnings);
        HashMap<String, Object> probPars = new HashMap<>();
        probPars.put("name", "test");
        BasicClauseList bcl = new BasicClauseList();
        bcl.predicates = 20;
        ProblemSupervisor psu = new ProblemSupervisor(1, glb, probPars, null);
        PreProcessor prep = new PreProcessor(psu, probPars, bcl);

        prep.addConjunction(new int[]{1, ClauseType.AND.ordinal(),1,2,3});
        Result result = prep.addDisjunction(new int[]{1, ClauseType.OR.ordinal(), 4,3,5});
        assertNull(result);
        assertEquals("", prep.clauses.toString());
        result = prep.addDisjunction(new int[]{2, ClauseType.OR.ordinal(), 4,-3,5,6});
        assertNull(result);
        assertEquals("2: (4,5,6)\n", prep.clauses.toString());
        result = prep.addDisjunction(new int[]{3, ClauseType.OR.ordinal(), -1,-2,4});
        assertNull(result);
        result = prep.addDisjunction(new int[]{4, ClauseType.OR.ordinal(), -3,-2,-1});
        assertTrue(result instanceof Unsatisfiable);
    }

    @Test
    public void addDisjunctionNoID() throws Exception {
        System.out.println("addDisjunction no Implication DAG");

        HashMap<String, String> pars = new HashMap<>();
        if(monitoring){pars.put("monitor","true");}
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        GlobalParameters glb = new GlobalParameters(pars,errors,warnings);
        HashMap<String,Object> probPars = new HashMap<>();
        probPars.put("name","test");
        BasicClauseList bcl = new BasicClauseList();
        bcl.predicates = 10;
        ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
        PreProcessor prep = new PreProcessor(psu,probPars,bcl);

        Result result = prep.addDisjunction(new int[]{1,ClauseType.OR.ordinal(),1,2,-1}); // tautology
        assertNull(result);
        assertEquals("",prep.clauses.toString());


        result = prep.addDisjunction(new int[]{1,ClauseType.OR.ordinal(),1,2,-3,1}); // double literal
        assertNull(result);
        assertEquals("1: (1,2,-3)\n",prep.clauses.toString());
        result = prep.addDisjunction(new int[]{2,ClauseType.OR.ordinal(),1,2,3,4});
        assertNull(result);
        assertEquals("1: (1,2,-3)\n" +
                "2: (1,2,4)\n",prep.clauses.toString());
        result = prep.addDisjunction(new int[]{3,ClauseType.OR.ordinal(),1,2,4,5});
        assertNull(result);
        prep.addDisjunction(new int[]{4,ClauseType.OR.ordinal(),3,4,5});

        result = prep.addDisjunction(new int[]{4,ClauseType.OR.ordinal(),2,1});
        assertNull(result);
        assertEquals("4: (3,4,5)\n",prep.clauses.toString());
        //System.out.println("CL\n"+prep.clauses.toString());
        //System.out.println(prep.statistics.toString());


    }

    public void addDisjunction() throws Exception {
        System.out.println("addDisjunction");

        HashMap<String, String> pars = new HashMap<>();
        if(monitoring){pars.put("monitor","true");}
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        GlobalParameters glb = new GlobalParameters(pars,errors,warnings);
        HashMap<String,Object> probPars = new HashMap<>();
        probPars.put("name","test");
        BasicClauseList bcl = new BasicClauseList();
        bcl.predicates = 10;
        ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
        PreProcessor prep = new PreProcessor(psu,probPars,bcl);

        Result result = prep.addDisjunction(new int[]{1,ClauseType.OR.ordinal(),1,2,-3});
        assertNull(result);
        assertEquals("1: (1,2,-3)\n",prep.clauses.toString());
        result = prep.addDisjunction(new int[]{2,ClauseType.OR.ordinal(),1,2,3,4});
        assertNull(result);
        assertEquals("1: (1,2,-3)\n" +
                "2: (1,2,4)\n",prep.clauses.toString());
        result = prep.addDisjunction(new int[]{3,ClauseType.OR.ordinal(),1,2,4,5});
        assertNull(result);

        prep.implicationDAG.addClause(-5,6);
        result = prep.addDisjunction(new int[]{3,ClauseType.OR.ordinal(),4,5,6});
        assertNull(result);
        assertEquals("1: (1,2,-3)\n" +
                "2: (1,2,4)\n",prep.clauses.toString());

        result = prep.addDisjunction(new int[]{3,ClauseType.OR.ordinal(),7,6,-5,8});
        assertNull(result);



        //System.out.println("CL\n"+prep.clauses.toString());
        //System.out.printf("IG\n" + prep.implicationDAG.toString());
        //System.out.println(prep.statistics.toString());


    }

    @Test
    public void addXor() throws Exception {
        System.out.println("addXor");
        HashMap<String, String> pars = new HashMap<>();
        if(monitoring){pars.put("monitor","true");}
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        GlobalParameters glb = new GlobalParameters(pars,errors,warnings);
        HashMap<String,Object> probPars = new HashMap<>();
        probPars.put("name","test");
        BasicClauseList bcl = new BasicClauseList();
        bcl.predicates = 10;
        ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
        PreProcessor prep = new PreProcessor(psu,probPars,bcl);

        Result result = prep.addXor(new int[]{1,ClauseType.XOR.ordinal(),1,2,-3});
        assertNull(result);
        assertEquals("1: (1,2,-3)\n",prep.clauses.toString());
        assertEquals("-3 -> -1,-2\n" +
                "1 -> -2,3\n" +
                "2 -> -1,3\n",prep.implicationDAG.toString());
        assertEquals("Disjointenss Classes:\nD-312: (-3,1,2)\n",prep.disjointnesses.toString());
        //System.out.println(prep.toString());
        //System.out.println("NEXT");
        result = prep.addXor(new int[]{2,ClauseType.XOR.ordinal(),-3,2});
        //System.out.println(prep.toString());
        assertNull(result);
        assertTrue(prep.clauses.isEmpty());
        assertEquals("[-1]",prep.model.toString());
        assertTrue(prep.implicationDAG.isEmpty());
        assertTrue(prep.disjointnesses.isEmpty());
        assertEquals("Equivalence Classes:\n" +
                "E2: (2,3)\n" +
                "\n" +
                "Replacements:\n" +
                "-3 -> -2\n" +
                "3 -> 2\n", prep.equivalences.toString());
        //System.out.println(prep.toString());

        //System.out.println("NEXTT");
        result = prep.addXor(new int[]{3,ClauseType.XOR.ordinal(),2,-3,4});
        assertNull(result);
        assertTrue(prep.disjointnesses.isEmpty());
        assertTrue(prep.implicationDAG.isEmpty());
        //System.out.println(prep.toString());
    }

    @Test
    public void addXorLonger() throws Exception {
        System.out.println("addXor longer");
        HashMap<String, String> pars = new HashMap<>();
        if(monitoring){pars.put("monitor","true");}
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        GlobalParameters glb = new GlobalParameters(pars,errors,warnings);
        glb.debug = true;
        HashMap<String,Object> probPars = new HashMap<>();
        probPars.put("name","test");
        BasicClauseList bcl = new BasicClauseList();
        bcl.predicates = 20;
        ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
        PreProcessor prep = new PreProcessor(psu,probPars,bcl);

        Result result = prep.addXor(new int[]{1,ClauseType.XOR.ordinal(),1,2,3,4,5});
        assertNull(result);
        assertEquals("1 -> -2,-3,-4,-5\n" +
                "2 -> -1,-3,-4,-5\n" +
                "3 -> -1,-2,-4,-5\n" +
                "4 -> -1,-2,-3,-5\n" +
                "5 -> -1,-2,-3,-4\n",prep.implicationDAG.toString());
        assertEquals("Disjointenss Classes:\n" +
                "D51234: (5,1,2,3,4)\n",prep.disjointnesses.toString());
        System.out.println(prep.toString());

        result = prep.addXor(new int[]{2,ClauseType.XOR.ordinal(),4,5,6,7,8});
        assertNull(result);
        assertEquals("1 -> -2,-3,-4,-5\n" +
                "2 -> -1,-3,-4,-5\n" +
                "3 -> -1,-2,-4,-5\n" +
                "4 -> -1,-2,-3,-5,-6,-7,-8\n" +
                "5 -> -1,-2,-3,-4,-6,-7,-8\n" +
                "6 -> -4,-5,-7,-8\n" +
                "7 -> -4,-5,-6,-8\n" +
                "8 -> -4,-5,-6,-7\n",prep.implicationDAG.toString());
        System.out.println(prep.toString());

        result = prep.addXor(new int[]{3,ClauseType.XOR.ordinal(),1,2,-4,7,8});
        assertNull(result);
        //assertEquals("",prep.implicationDAG.toString());
        System.out.println(prep.toString());
    }



    @Test
    public void addDisjoint() throws Exception {

    }

}