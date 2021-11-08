package Solvers.Walker;

import Datastructures.Clauses.AllClauses;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Datastructures.TwoLiteral.TwoLitClauses;
import Management.Controller;
import Management.GlobalParameters;
import Management.Monitor;
import Management.ProblemSupervisor;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

import static org.junit.Assert.*;

public class WalkerTest {
    StringBuilder errors = new StringBuilder();
    StringBuilder warnings = new StringBuilder();


    private static final ClauseType or = ClauseType.OR;
    private static final ClauseType al = ClauseType.ATLEAST;
    private static final ClauseType am = ClauseType.ATMOST;
    private static final ClauseType ex = ClauseType.EXACTLY;

    private static Clause make(int id, ClauseType type, int quantifier, int... literals) {
        Clause clause = new Clause(id,type,quantifier, IntArrayList.wrap(literals));
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
        problemSupervisor.clauseCounter = 9;
        BasicClauseList bcl = new BasicClauseList();
        bcl.predicates = predicates;
        bcl.symboltable = symboltable;
        problemSupervisor.basicClauseList = bcl;
        problemSupervisor.model = new Model(predicates,symboltable);

        return new Walker(1,solverParameters, problemSupervisor);
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
    public void addClause() {
        System.out.println("addClause");
        Walker walker = prepare(10,true, null);
        walker.addClause(make(1, or, 1, 1, 2, 3));
        System.out.println(walker.toString(walker.symboltable));
    }
    @Test
    public void setInitialScores() {
        System.out.println("setInitialScores");
        Walker walker = prepare(5, true, null);
        walker.addClause(make(1, or, 1, 1, -2, 3));
        int[] posScores = new int[6];
        int[] negScores = new int[6];
        walker.setInitialScores(posScores, negScores);
        assertEquals("[0, 1, 0, 1, 0, 0]",Arrays.toString(posScores));
        assertEquals("[0, 0, 1, 0, 0, 0]",Arrays.toString(negScores));

        walker.addClause(make(2, al, 2, 1, -2, 3,4));

        posScores = new int[6];
        negScores = new int[6];
        walker.setInitialScores(posScores, negScores);

        assertEquals("[0, 3, 0, 3, 1, 0]",Arrays.toString(posScores));
        assertEquals("[0, 0, 3, 0, 0, 0]",Arrays.toString(negScores));

        walker = prepare(5, true, null);
        walker.addClause(make(2, am, 3, -2, -3,-4,5));

        posScores = new int[6];
        negScores = new int[6];
        walker.setInitialScores(posScores, negScores);
        assertEquals("[0, 0, 0, 0, 0, -1]",Arrays.toString(posScores));
        assertEquals("[0, 0, -1, -1, -1, 0]",Arrays.toString(negScores));
    }


    @Test
    public void setInitialTruthValueOR() {
        System.out.println("setInitialTruthValue OR");
        Walker walker = prepare(5, true, null);
        walker.model.addImmediately(1);

        Clause c1 = make(1, or, 1, 1, -2, 3);
        WClause wc1 = walker.addClause(c1);
        walker.setInitialTruthValue(wc1);
        assertTrue(wc1.isGloballyTrue);
        assertTrue(wc1.isLocallyTrue);
        Clause c2 = make(2, or, 1, -4,-5);
        WClause wc2 = walker.addClause(c2);

        walker.setInitialTruthValue(wc2);
        assertFalse(wc2.isGloballyTrue);
        assertTrue(wc2.isLocallyTrue);

        walker.model.addImmediately(-5);
        walker.setInitialTruthValue(wc2);
        assertTrue(wc2.isGloballyTrue);
        assertTrue(wc2.isLocallyTrue);
    }
    @Test
    public void setInitialTruthValueATLEAST() {
        System.out.println("setInitialTruthValue ATLEAST");
        Walker walker = prepare(10, true, null);

        Clause c1 = make(1, al, 1, 1, -2, 3);
        WClause wc1 = walker.addClause(c1);
        walker.setInitialTruthValue(wc1);
        assertFalse(wc1.isGloballyTrue);
        assertTrue(wc1.isLocallyTrue);

        walker.model.addImmediately(1);
        walker.setInitialTruthValue(wc1);
        assertTrue(wc1.isGloballyTrue);
        assertTrue(wc1.isLocallyTrue);

        Clause c2 = make(2, al, 2, -4,-5,-6);
        WClause wc2 = walker.addClause(c2);

        walker.setInitialTruthValue(wc2);
        assertFalse(wc2.isGloballyTrue);
        assertTrue(wc2.isLocallyTrue);

        walker.model.addImmediately(-5);
        walker.setInitialTruthValue(wc2);
        assertFalse(wc2.isGloballyTrue);
        assertTrue(wc2.isLocallyTrue);

        walker.model.addImmediately(-6);
        walker.setInitialTruthValue(wc2);
        assertTrue(wc2.isGloballyTrue);
        assertTrue(wc2.isLocallyTrue);

        Clause c3 = make(2, al, 2, 7,8,9);
        WClause wc3 = walker.addClause(c3);
        walker.setInitialTruthValue(wc3);
        assertFalse(wc3.isGloballyTrue);
        assertFalse(wc3.isLocallyTrue);
        walker.localModel[8] = true;
        walker.setInitialTruthValue(wc3);
        assertFalse(wc3.isGloballyTrue);
        assertFalse(wc3.isLocallyTrue);
        walker.localModel[9] = true;
        walker.setInitialTruthValue(wc3);
        assertFalse(wc3.isGloballyTrue);
        assertTrue(wc3.isLocallyTrue);
  }
    @Test
    public void setInitialTruthValueATMOST() {
        System.out.println("setInitialTruthValue ATMOST");
        Walker walker = prepare(10, true, null);

        Clause c1 = make(1, am, 1, 1, -2, -3);
        WClause wc1 = walker.addClause(c1);
        walker.setInitialTruthValue(wc1);
        assertFalse(wc1.isGloballyTrue);
        assertFalse(wc1.isLocallyTrue);

        wc1 = walker.addClause(c1);
        walker.model.addImmediately(1);
        walker.setInitialTruthValue(wc1);
        assertTrue(wc1.isGloballyTrue);
        assertTrue(wc1.isLocallyTrue);

        wc1 = walker.addClause(c1);
        walker.model.addImmediately(-2);
        walker.setInitialTruthValue(wc1);
        assertFalse(wc1.isGloballyTrue);
        assertFalse(wc1.isLocallyTrue);

        Clause c2 = make(2, am, 2, 4,5,6);
        WClause wc2 = walker.addClause(c2);
        walker.setInitialTruthValue(wc2);
        assertFalse(wc2.isGloballyTrue);
        assertTrue(wc2.isLocallyTrue);

        walker.localModel[5] = true;
        walker.setInitialTruthValue(wc2);
        assertFalse(wc2.isGloballyTrue);
        assertTrue(wc2.isLocallyTrue);
        walker.localModel[6] = true;

        walker.setInitialTruthValue(wc2);
        assertFalse(wc2.isGloballyTrue);
        assertTrue(wc2.isLocallyTrue);
        walker.localModel[4] = true;

        wc2 = walker.addClause(c2);
        walker.setInitialTruthValue(wc2);
        assertFalse(wc2.isGloballyTrue);
        assertFalse(wc2.isLocallyTrue);


    }
    @Test
    public void setInitialTruthValueEXACTLY() {
        System.out.println("setInitialTruthValue EXACTLY");
        Walker walker = prepare(10, true, null);

        Clause c1 = make(1, ex, 2, 1, -2, 3);
        WClause wc1 = walker.addClause(c1);
        walker.setInitialTruthValue(wc1);
        assertFalse(wc1.isGloballyTrue);
        assertFalse(wc1.isLocallyTrue);

        walker.model.addImmediately(-2);
        walker.setInitialTruthValue(wc1);
        assertFalse(wc1.isGloballyTrue);
        assertFalse(wc1.isLocallyTrue);

        walker.model.addImmediately(1);
        walker.setInitialTruthValue(wc1);
        assertTrue(wc1.isGloballyTrue);
        assertTrue(wc1.isLocallyTrue);


        walker.model.addImmediately(3);
        wc1 = walker.addClause(c1);
        walker.setInitialTruthValue(wc1);
        assertFalse(wc1.isGloballyTrue);
        assertFalse(wc1.isLocallyTrue);

        Clause c2 = make(2, ex, 2, 4,5,6);
        WClause wc2 = walker.addClause(c2);
        walker.setInitialTruthValue(wc2);
        assertFalse(wc2.isGloballyTrue);
        assertFalse(wc2.isLocallyTrue);

        walker.localModel[6] = true;
        walker.setInitialTruthValue(wc2);
        assertFalse(wc2.isGloballyTrue);
        assertFalse(wc2.isLocallyTrue);
        walker.localModel[5] = true;
        walker.setInitialTruthValue(wc2);
        assertFalse(wc2.isGloballyTrue);
        assertTrue(wc2.isLocallyTrue);


        wc2 = walker.addClause(c2);
        walker.localModel[4] = true;
        walker.setInitialTruthValue(wc2);
        assertFalse(wc2.isGloballyTrue);
        assertFalse(wc2.isLocallyTrue);
    }

    @Test
    public void getLocalTruthValue1() {
        System.out.println("getLocalTruthValue 1");
        Walker walker = prepare(5, true, null);
        Clause c1 = make(1, or, 1, 1, 2, 3);
        WClause wc1 = walker.addClause(c1);
        Clause c2 = make(2, al, 2, 1, 2, 3);
        WClause wc2 = walker.addClause(c2);
        Clause c3 = make(3, am, 2, 1, 2, 3);
        WClause wc3 = walker.addClause(c3);
        Clause c4 = make(4, ex, 2, 1, 2, 3);
        WClause wc4 = walker.addClause(c4);

        assertFalse(walker.getLocalTruthValue(wc1));
        assertFalse(walker.getLocalTruthValue(wc2));
        assertTrue(walker.getLocalTruthValue(wc3));
        assertFalse(walker.getLocalTruthValue(wc4));

        walker.localModel[3] = true;
        assertTrue(walker.getLocalTruthValue(wc1));
        assertFalse(walker.getLocalTruthValue(wc2));
        assertTrue(walker.getLocalTruthValue(wc3));
        assertFalse(walker.getLocalTruthValue(wc4));

        walker.localModel[2] = true;
        assertTrue(walker.getLocalTruthValue(wc1));  // or
        assertTrue(walker.getLocalTruthValue(wc2));  // atleast 2
        assertTrue(walker.getLocalTruthValue(wc3));  // atmost 2
        assertTrue(walker.getLocalTruthValue(wc4));  // exactly 2

        walker.localModel[1] = true;
        assertTrue(walker.getLocalTruthValue(wc1));  // or
        assertTrue(walker.getLocalTruthValue(wc2));  // atleast 2
        assertFalse(walker.getLocalTruthValue(wc3));  // atmost 2
        assertFalse(walker.getLocalTruthValue(wc4));  // exactly 2
    }

    @Test
    public void getLocalTruthValue2() {
        System.out.println("getLocalTruthValue 2");
        Walker walker = prepare(5, true, null);
        Clause c1 = make(1, or, 1, 1, -2, 3);
        WClause wc1 = walker.addClause(c1);
        Clause c2 = make(2, al, 2, 1, -2, 3);
        WClause wc2 = walker.addClause(c2);
        Clause c3 = make(3, am, 2, 1, -2, 3);
        WClause wc3 = walker.addClause(c3);
        Clause c4 = make(4, ex, 2, 1, -2, 3);
        WClause wc4 = walker.addClause(c4);

        assertTrue(walker.getLocalTruthValue(wc1));
        assertFalse(walker.getLocalTruthValue(wc2));
        assertTrue(walker.getLocalTruthValue(wc3));
        assertFalse(walker.getLocalTruthValue(wc4));

        walker.localModel[3] = true;
        assertTrue(walker.getLocalTruthValue(wc1));  // or
        assertTrue(walker.getLocalTruthValue(wc2));  // atleast 2
        assertTrue(walker.getLocalTruthValue(wc3));  // atmost 2
        assertTrue(walker.getLocalTruthValue(wc4));  // exactly 2

        walker.localModel[2] = true;
        assertTrue(walker.getLocalTruthValue(wc1));  // or
        assertFalse(walker.getLocalTruthValue(wc2));  // atleast 2
        assertTrue(walker.getLocalTruthValue(wc3));  // atmost 2
        assertFalse(walker.getLocalTruthValue(wc4));  // exactly 2

        walker.localModel[1] = true; // 1, -2, 3
        assertTrue(walker.getLocalTruthValue(wc1));  // or
        assertTrue(walker.getLocalTruthValue(wc2));  // atleast 2
        assertTrue(walker.getLocalTruthValue(wc3));  // atmost 2
        assertTrue(walker.getLocalTruthValue(wc4));  // exactly 2
    }

    @Test
    public void getGlobalTruthValue1() throws Unsatisfiable {
        System.out.println("getGlobalTruthValue 1");
        Walker walker = prepare(5, true, null);
        Model model = walker.model;
        Clause c1 = make(1, or, 1, 1, 2, 3);
        WClause wc1 = walker.addClause(c1);
        Clause c2 = make(2, al, 2, 1, 2, 3);
        WClause wc2 = walker.addClause(c2);
        Clause c3 = make(3, am, 2, 1, 2, 3);
        WClause wc3 = walker.addClause(c3);
        Clause c4 = make(4, ex, 2, 1, 2, 3);
        WClause wc4 = walker.addClause(c4);

        assertFalse(walker.getGlobalTruthValue(wc1));  // or
        assertFalse(walker.getGlobalTruthValue(wc2));  // atleast 2
        assertFalse(walker.getGlobalTruthValue(wc3));  // atmost 2
        assertFalse(walker.getGlobalTruthValue(wc4));  // exactly 2

        model.addImmediately(3);
        assertTrue(walker.getGlobalTruthValue(wc1));  // or
        assertFalse(walker.getGlobalTruthValue(wc2));  // atleast 2
        assertTrue(walker.getGlobalTruthValue(wc3));  // atmost 2
        assertFalse(walker.getGlobalTruthValue(wc4));  // exactly 2

        model.addImmediately(2);
        assertTrue(walker.getGlobalTruthValue(wc1));  // or
        assertTrue(walker.getGlobalTruthValue(wc2));  // atleast 2
        assertTrue(walker.getGlobalTruthValue(wc3));  // atmost 2
        assertTrue(walker.getGlobalTruthValue(wc4));  // exactly 2


        model.addImmediately(1);
        assertTrue(walker.getGlobalTruthValue(wc1));  // or
        assertTrue(walker.getGlobalTruthValue(wc2));  // atleast 2
        try {walker.getGlobalTruthValue(wc3);
            assertTrue(false);}  // atmost 2
        catch(Unsatisfiable uns) {
            System.out.println(uns);}

        try {walker.getGlobalTruthValue(wc4);
            assertTrue(false);}  // exactly 2
        catch(Unsatisfiable uns) {
            System.out.println(uns);} // exactly 2

        Clause c5 = make(5, ex, 2, 1, -2, -3);
        WClause wc5 = walker.addClause(c5);
        try {walker.getGlobalTruthValue(wc5);
            assertTrue(false);}  // exactly 2
        catch(Unsatisfiable uns) {
            System.out.println(uns);} // exactly 2

    }

    @Test
    public void intializeModel() {
        System.out.println("intializeModel");
        Walker walker = prepare(10, true, null);
        Clause c1 = make(1, or, 1, 1, -2, 3);
        WClause wc1 = walker.addClause(c1);
        walker.initializeModel();
        assertEquals("1,3,4,5,6,7,8,9,10,",walker.localModelToString(null));
        assertTrue(wc1.isLocallyTrue);
        assertFalse(wc1.isGloballyTrue);
        assertEquals(0,walker.falseClauses);

        Clause c2 = make(2, am, 1, 4,5);
        WClause wc2 = walker.addClause(c2);
        walker.initializeModel();
        assertEquals("1,3,6,7,8,9,10,",walker.localModelToString(null));
        assertTrue(wc2.isLocallyTrue);
        assertFalse(wc2.isGloballyTrue);
        assertEquals(0,walker.falseClauses);

        Clause c3 = make(3, al, 2, -4,-5,6);
        WClause wc3 = walker.addClause(c3);
        walker.initializeModel();
        assertEquals("1,3,6,7,8,9,10,",walker.localModelToString(null));
        }

    @Test
    public void setInitialFlipScores() {
        System.out.println("setInitialFlipScores");
        Walker walker = prepare(10, true, null);
        Clause c1 = make(1, or, 1, 1, 2, 3);
        WClause wc1 = walker.addClause(c1);
        walker.localModel[1] = true;
        walker.setInitialTruthValue(wc1);
        assertTrue(wc1.isLocallyTrue);
        walker.setInitialFlipScores(wc1);
        assertEquals("Integer Queue:  item: score\n" +
                "0:0, 2:0, 3:0, 4:0, 5:0, 6:0, 7:0, 8:0, 9:0, 10:0, 1:-1, ",walker.flipScoresToString());

    }

    }