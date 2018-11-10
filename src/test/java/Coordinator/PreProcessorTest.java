package Coordinator;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Erraneous;
import Datastructures.Results.Result;
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

    static boolean monitoring = true;
    @Test
    public void prepareClauses() throws Exception {
        System.out.println("disjoints 3SAT");
        int from = 0;
        int to = 100;
        for(int seed = from; seed <= to; ++seed) {
            System.out.println("SEED " + seed);
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
            System.out.println(errors.toString());
            BasicClauseList bClauses = RandomClauseSetGenerator.generate(rpars.get(0),errors,warnings);
            HashMap<String,Object> probPars = new HashMap<>();
            probPars.put("name","test");
            ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
            PreProcessor prep = new PreProcessor(psu,probPars,bClauses);
            System.out.println(bClauses.toString());
            Result result = prep.prepareClauses();
            if(result != null && result instanceof Erraneous) {
                System.out.println("Result SEED " + seed);
                System.out.println(result);
                System.out.println(prep.toString());
            break;}}
    }

    @Test
    public void binaryClauses() throws Exception {
        System.out.println("disjoints 2SAT");
        int from = 0;
        int to = 100;
        for(int seed = from; seed <= to; ++seed) {
            System.out.println("SEED " + seed);
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
            System.out.println(errors.toString());
            BasicClauseList bClauses = RandomClauseSetGenerator.generate(rpars.get(0),errors,warnings);
            HashMap<String,Object> probPars = new HashMap<>();
            probPars.put("name","test");
            ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
            PreProcessor prep = new PreProcessor(psu,probPars,bClauses);
            System.out.println(bClauses.toString());
            Result result = prep.prepareClauses();
            if(result != null && result instanceof Erraneous) {
                System.out.println("Result SEED " + seed);
                System.out.println(result);
                System.out.println(prep.toString());
                break;}}
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

    }

    @Test
    public void addDisjoint() throws Exception {

    }

}