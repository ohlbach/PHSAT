package Solvers.Walker;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.Connective;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import Generators.RandomClauseSetGenerator;
import Management.Controller;
import Management.GlobalParameters;
import Management.ProblemSupervisor;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import static org.junit.Assert.*;

public class WalkerTest {
    StringBuilder errors = new StringBuilder();
    StringBuilder warnings = new StringBuilder();


    private static final Connective or = Connective.OR;
    private static final Connective al = Connective.ATLEAST;
    private static final Connective am = Connective.ATMOST;
    private static final Connective ex = Connective.EXACTLY;
    private static final Connective iv = Connective.INTERVAL;

    private static Clause make(int id, Connective connective, int... literals)  throws Unsatisfiable{
        int[] basicClause = new int[literals.length+2];
        basicClause[0] = id;
        basicClause[1] = connective.ordinal();
        for(int i = 0; i < literals.length; ++i) {
             basicClause[i+2] = literals[i];}
        Clause clause = new Clause(basicClause);
        return clause;}


    private Walker prepare(int predicates, boolean withSymboltable, HashMap<String,Object> solverParameters) {
        if(solverParameters == null) {
            solverParameters = Walker.parseParameters(new HashMap<>(),errors,warnings).get(0);}
        Controller controller = new Controller(null,null,null);
        GlobalParameters globalParameters=new GlobalParameters();
        globalParameters.monitor =  null;
        HashMap<String,Object> problemParameters = new HashMap<>();
        problemParameters.put("name","test");
        ProblemSupervisor problemSupervisor = new ProblemSupervisor(controller,globalParameters,problemParameters,null);

        Symboltable symboltable = null;
        if(withSymboltable) {
            symboltable =  new Symboltable(predicates);
            symboltable.setName(1,"p");
            symboltable.setName(2,"q");
            symboltable.setName(3,"r");
            symboltable.setName(4,"s");
            symboltable.setName(5,"t");}
        //problemSupervisor.clauseCounter = 9;
        BasicClauseList bcl = new BasicClauseList();
        bcl.predicates = predicates;
        bcl.symboltable = symboltable;
        problemSupervisor.basicClauseList = bcl;
        problemSupervisor.model = new Model(predicates,symboltable);

        return new Walker(1,solverParameters, problemSupervisor);
    }

    private void add2Walker(Walker walker, BasicClauseList bcl)  throws Unsatisfiable {
        for(int[] clause : bcl.disjunctions) walker.addClause(new Clause(clause));
        for(int[] clause : bcl.quantifieds) walker.addClause(new Clause(clause));
        for(int[] clause : bcl.intervals) walker.addClause(new Clause(clause));
    }

    @Test
    public void help() {
        System.out.println(Walker.help());
    }

    @Test
    public void parseParameters() {
        System.out.println("parseParameters");
        HashMap<String,String> parameters = new HashMap<>();
        parameters.put("seed","0,1");
        parameters.put("flips", "1 to 2");
        parameters.put("jumps","10 to 20 step 5");
        ArrayList<HashMap<String,Object>> par = Walker.parseParameters(parameters,errors,warnings);
        String s = "";
        for(HashMap<String,Object> map : par) s += map.toString()+"\n";
        assertEquals("{seed=0, flips=1, name=W1, jumps=10}\n" +
                "{seed=0, flips=2, name=W2, jumps=10}\n" +
                "{seed=1, flips=1, name=W3, jumps=10}\n" +
                "{seed=1, flips=2, name=W4, jumps=10}\n" +
                "{seed=0, flips=1, name=W5, jumps=15}\n" +
                "{seed=0, flips=2, name=W6, jumps=15}\n" +
                "{seed=1, flips=1, name=W7, jumps=15}\n" +
                "{seed=1, flips=2, name=W8, jumps=15}\n" +
                "{seed=0, flips=1, name=W9, jumps=20}\n" +
                "{seed=0, flips=2, name=W10, jumps=20}\n" +
                "{seed=1, flips=1, name=W11, jumps=20}\n" +
                "{seed=1, flips=2, name=W12, jumps=20}\n",s);

        parameters = new HashMap<>();
        par = Walker.parseParameters(parameters,errors,warnings);
        assertEquals("[{seed=0, flips=2147483647, name=W1, jumps=10}]",par.toString());
    }


    @Test
    public void addClause()  throws Unsatisfiable{
        System.out.println("addClause");
        Walker walker = prepare(10,true, null);
        walker.addClause(make(1, or, 1, 1, 2, 3));
        System.out.println(walker.toString(walker.symboltable));
    }
    @Test
    public void setInitialScores()  throws Unsatisfiable{
        System.out.println("setInitialScores");
        Walker walker = prepare(5, true, null);
        walker.addClause(make(1, or, 1, -2, 3));
        int[] posScores = new int[6];
        int[] negScores = new int[6];
        walker.initialScores(posScores, negScores);
        assertEquals("[0.0, 1.0, 0.0, 1.0, 0.0, 0.0]",Arrays.toString(posScores));
        assertEquals("[0.0, 0.0, 1.0, 0.0, 0.0, 0.0]",Arrays.toString(negScores));

        walker = prepare(5, true, null);
        posScores = new int[6];
        negScores = new int[6];
        walker.addClause(make(2, iv, 1, 3, 1, -2, 3));
        walker.initialScores(posScores, negScores);
        assertEquals("[0.0, 1.0, 0.0, 1.0, 0.0, 0.0]",Arrays.toString(posScores));
        assertEquals("[0.0, 0.0, 1.0, 0.0, 0.0, 0.0]",Arrays.toString(negScores));

        walker = prepare(5, true, null);
        posScores = new int[6];
        negScores = new int[6];
        walker.addClause(make(3, iv, 1, 3, 1, -2, 3, 4));
        walker.initialScores(posScores, negScores);
        assertEquals("[0.0, 1.0, 0.0, 1.0, 1.0, 0.0]",Arrays.toString(posScores));
        assertEquals("[0.0, 0.0, 1.0, 0.0, 0.0, 0.0]",Arrays.toString(negScores));

        walker = prepare(5, true, null);
        posScores = new int[6];
        negScores = new int[6];
        walker.addClause(make(3, iv, 2, 3, 1, -2, 3, 4));
        walker.initialScores(posScores, negScores);
        assertEquals("[0.0, 0.5, 0.0, 0.5, 0.5, 0.0]",Arrays.toString(posScores));
        assertEquals("[0.0, 0.0, 0.5, 0.0, 0.0, 0.0]",Arrays.toString(negScores));

        walker = prepare(5, true, null);
        posScores = new int[6];
        negScores = new int[6];
        walker.addClause(make(3, iv, 0, 2, 1, -2, 3, 4));
        walker.initialScores(posScores, negScores);
        assertEquals("[0.0, 0.0, 0.5, 0.0, 0.0, 0.0]",Arrays.toString(posScores));
        assertEquals("[0.0, 0.5, 0.0, 0.5, 0.5, 0.0]",Arrays.toString(negScores));

        walker = prepare(5, true, null);
        posScores = new int[6];
        negScores = new int[6];
        walker.addClause(make(3, iv, 2, 3, 1, -2, 3, 4));
        walker.addClause(make(4, iv, 0, 2, 1, -2, 3, 4));
        walker.initialScores(posScores, negScores);
        assertEquals("[0.0, 0.5, 0.5, 0.5, 0.5, 0.0]",Arrays.toString(posScores));
        assertEquals("[0.0, 0.5, 0.5, 0.5, 0.5, 0.0]",Arrays.toString(negScores));
    }


    @Test
    public void getLocalTruthValue()  throws Unsatisfiable{
        System.out.println("getLocalTruthValue");
        Walker walker = prepare(5, true, null);
        WClause c1 = new WClause(make(1, or, 1, 2, 3));
        assertFalse(walker.getLocalTruthValue(c1));
        WClause c2 = new WClause(make(2, iv, 0,1, 1, 2, 3));
        assertTrue(walker.getLocalTruthValue(c2));
        walker.localModel[2] = true;
        assertTrue(walker.getLocalTruthValue(c1));
        assertTrue(walker.getLocalTruthValue(c2));
        walker.localModel[1] = true;
        walker.localModel[3] = true;
        assertFalse(walker.getLocalTruthValue(c2));
    }

    @Test
    public void initializeModel()  throws Unsatisfiable{
        System.out.println("initializeModel");
        Walker walker = prepare(5, true, null);
        walker.addClause(make(1, or, 1, -2, 3,-4,-5));
        walker.initializeModel();
        assertEquals("1,3,",walker.localModelToString(null));

        walker = prepare(5, true, null);
        Clause c1 = make(2, iv, 2, 3 , 1,2, 3,-4,-5);
        WClause w1 = walker.addClause(c1);
        walker.initializeModel();
        assertFalse(w1.isLocallyTrue);
        assertEquals("1,2,3,",walker.localModelToString(null));
        Clause c2 = make(3, iv, 3, 5 , 1,2, 3,-4,-5);
        WClause w2 = walker.addClause(c2);
        walker.initializeModel();
        assertEquals("1,2,3,",walker.localModelToString(null));
        assertFalse(w1.isLocallyTrue);
        assertTrue(w2.isLocallyTrue);
    }

    @Test
    public void initializeModel1()  throws Unsatisfiable{
        System.out.println("initializeModel 1");
        Walker walker = prepare(5, true, null);
        walker.addClause(make(1, or, 1,2));
        walker.addClause(make(2, or, 1,-2));
        walker.addClause(make(3, or, -1,2));
        walker.initializeModel();
        assertEquals("1,2,3,4,5",walker.localModelToString(null));
        assertEquals(0,walker.falseClauses);
    }

    @Test
    public void initializeModel2()  throws Unsatisfiable{
        System.out.println("initializeModel 2");
        Walker walker = prepare(5, true, null);
        walker.addClause(make(1, or, 1,2,3));
        //walker.addClause(make(2, or, 1,2,-3));
        walker.addClause(make(3, or, 1,-2,3));
        walker.addClause(make(4, or, 1,-2,-3));
        walker.addClause(make(5, or, -1,2,3));
        walker.addClause(make(6, or, -1,2,-3));
        walker.addClause(make(7, or, -1,-2,3));
        walker.addClause(make(8, or, -1,-2,-3));
        walker.initializeModel();
        //System.out.println(walker.toString());
        assertEquals("3,4,5,",walker.localModelToString(null));
        assertEquals(0,walker.falseClauses);
    }

    @Test
    public void updateFlipScores()  throws Unsatisfiable{
        System.out.println("updateFlipScores");
        Walker walker = prepare(5, true, null);
        WClause w1 = walker.addClause(make(1, or, 1,2,3));
        walker.updateFlipScores(w1,(short)1);
        assertEquals("Integer Queue:  item: score\n" +
                "1:1.0, 2:1.0, 3:1.0, 4:0.0, 5:0.0, 0:-2.14748365E9, ",walker.flipScoresToString());

        walker = prepare(5, true, null);
        w1 = walker.addClause(make(2, iv, 2,2,1,2,3));
        walker.updateFlipScores(w1,(short)1);
        assertEquals("Integer Queue:  item: score\n" +
                "1:0.5, 2:0.5, 3:0.5, 4:0.0, 5:0.0, 0:-2.14748365E9, ",walker.flipScoresToString());

        walker = prepare(5, true, null);
        w1 = walker.addClause(make(3, iv, 2,2,1,2,3,4,5));
        walker.localModel[2] = true;
        w1.isLocallyTrue=true;
        walker.updateFlipScores(w1,(short)1);
        assertEquals("Integer Queue:  item: score\n" +
                "1:-1.0, 2:-1.0, 3:-1.0, 4:-1.0, 5:-1.0, 0:-2.14748365E9, ",walker.flipScoresToString());

        walker = prepare(5, true, null);
        w1 = walker.addClause(make(3, iv, 2,3, 1,2,3,4,5));
        walker.localModel[2] = true;
        walker.localModel[3] = true;
        walker.localModel[4] = true;
        w1.isLocallyTrue=true;
        walker.updateFlipScores(w1,(short)1);
        assertEquals("Integer Queue:  item: score\n" +
                "2:0.0, 3:0.0, 4:0.0, 1:-1.0, 5:-1.0, 0:-2.14748365E9, ",walker.flipScoresToString());

        walker = prepare(5, true, null);
        w1 = walker.addClause(make(3, iv, 3,4, 1,2,3,4,5));
        walker.localModel[2] = true;
        walker.localModel[3] = true;
        walker.updateFlipScores(w1,(short)1);
        assertEquals("Integer Queue:  item: score\n" +
                "1:1.0, 4:1.0, 5:1.0, 3:-1.0, 2:-1.0, 0:-2.14748365E9, ",walker.flipScoresToString());
    }

    @Test
    public void random()  throws Unsatisfiable{
        System.out.println("random");
        int predicates = 50;
        float cpRatio = (float)4;
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        Walker walker = prepare(predicates, true, null);
        HashMap<String, String> pars = new HashMap<>();
        pars.put("predicates", ""+predicates);
        pars.put("lengths", "3");
        pars.put("cpRatios", ""+cpRatio);
        pars.put("seeds", "5");
        ArrayList<HashMap<String, Object>> parameters = RandomClauseSetGenerator.parseParameters(pars,errors,warnings);
        //System.out.println(parameters.get(0));
        BasicClauseList bcl = RandomClauseSetGenerator.generate(parameters.get(0), walker.problemSupervisor, errors, warnings);
        //System.out.println(bcl.toString());
        add2Walker(walker,bcl);
        walker.print = true;
        walker.threshold = 4;
        walker.blocked = false;
        walker.jumpDistance = 40;
        walker.jumpFrequency = 10;
        walker.exponent = 3;
        walker.maxFlips = 70;
        //System.out.println(walker.toString());
        Satisfiable result = (Satisfiable)walker.solve();
        System.out.println(result.model);
        System.out.println(bcl.falseClausesInModel(result.model));

    }


    }