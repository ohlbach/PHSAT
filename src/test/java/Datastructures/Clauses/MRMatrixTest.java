package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.DisjointnessClasses;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Datastructures.TwoLiteral.TwoLitClause;
import Datastructures.TwoLiteral.TwoLitClauses;
import InferenceSteps.MRResolutionSquare1;
import InferenceSteps.MRResolutionSquare2;
import Management.Controller;
import Management.GlobalParameters;
import Management.Monitor;
import Management.ProblemSupervisor;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Random;

import static Utilities.Utilities.sortIntArray;
import static org.junit.Assert.*;

public class MRMatrixTest {

    
    static StringBuffer errors = new StringBuffer();
    static StringBuffer warnings = new StringBuffer();
    boolean monitoring = true;

    private AllClauses prepare(boolean monitoring, boolean withSymboltable) {
        Controller controller = new Controller(null,null,null);
        GlobalParameters globalParameters=new GlobalParameters();
        globalParameters.monitor = !monitoring ? null : new Monitor(null,"mixed",errors,warnings);
        globalParameters.trackReasoning = true;
        HashMap<String,Object> problemParameters = new HashMap<>();
        problemParameters.put("name","test");
        ProblemSupervisor problemSupervisor = new ProblemSupervisor(controller,globalParameters,problemParameters,null);
        Symboltable symboltable = null;
        if(withSymboltable) {
            symboltable =  new Symboltable(20);
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
    
    
    static Clause[] combination = new Clause[4];
    static Symboltable symboltable = new Symboltable(20);
    static{
        symboltable.setName(1,"a");
        symboltable.setName(2,"b");
        symboltable.setName(3,"c");
        symboltable.setName(4,"d");
        symboltable.setName(5,"e");
        symboltable.setName(6,"f");
        symboltable.setName(7,"g");
        symboltable.setName(8,"h");

        combination[0] = new Clause(1,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{1,2}));
        combination[1] = new Clause(2,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{2,3,4,-6}));
        combination[2] = new Clause(344,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{3,-4,-5}));
        combination[3] = new Clause(4,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{5,6,7}));
    }

    @Test
    public void infoString() {
        System.out.println("infoString");
        AllClauses allClauses = prepare(monitoring,true);
        MRMatrix matrix = new MRMatrix(allClauses, combination);
        System.out.println(matrix.infoString(symboltable));
    }

    @Test
    public void insertClause() {
        System.out.println("inserClause");
        AllClauses allClauses = prepare(monitoring,true);
        Clause[] combination = new Clause[4];
        combination[0] = new Clause(1,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{1,3,6,9,13}));
        combination[1] = new Clause(2,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{2,4,7,10,14}));
        combination[2] = new Clause(3,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{5,8,11,15}));
        combination[3] = new Clause(4,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{12,16}));
        MRMatrix matrix = new MRMatrix(allClauses, combination);
        Clause clause1 = new Clause(10, ClauseType.OR, IntArrayList.wrap(new int[]{1, 2}));
        Clause clause2 = new Clause(11, ClauseType.OR, IntArrayList.wrap(new int[]{3,-4,5}));
        Clause clause3 = new Clause(12, ClauseType.OR, IntArrayList.wrap(new int[]{6,7,8}));
        Clause clause4 = new Clause(13, ClauseType.OR, IntArrayList.wrap(new int[]{9,10,11,12}));
        Clause clause5 = new Clause(14, ClauseType.OR, IntArrayList.wrap(new int[]{13,14,15,16}));
        Clause clause6 = new Clause(15, ClauseType.OR, IntArrayList.wrap(new int[]{13,14,15,12}));
        matrix.insertClause(clause5);
        matrix.insertClause(clause3);
        matrix.insertClause(clause2);
        matrix.insertClause(clause1);
        matrix.insertClause(clause4);
        assertFalse(matrix.insertClause(clause6));
        System.out.println(matrix.infoString(null));
        System.out.println(matrix.toString(null));
    }

    @Test
    public void insertClause1() {
        System.out.println("inserClause distribute");
        AllClauses allClauses = prepare(monitoring,true);
        Clause[] combination = new Clause[4];
        combination[0] = new Clause(1,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{1,4,7}));
        combination[1] = new Clause(2,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{2,5,9}));
        combination[2] = new Clause(3,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{3,5,9}));
        combination[3] = new Clause(4,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{6,8,9}));
        MRMatrix matrix = new MRMatrix(allClauses, combination);
        Clause clause1 = new Clause(10, ClauseType.OR, IntArrayList.wrap(new int[]{1, 2,3}));
        Clause clause2 = new Clause(11, ClauseType.OR, IntArrayList.wrap(new int[]{4,5,6,10}));
        Clause clause3 = new Clause(12, ClauseType.OR, IntArrayList.wrap(new int[]{7,5,8}));
        matrix.insertClause(clause1);
        matrix.insertClause(clause2);
        matrix.insertClause(clause3);
        System.out.println(matrix.infoString(null));
        System.out.println(matrix.toString(null));
    }


    @Test
    public void findFirstIndices() {
        System.out.println("findFirstColIndices");
        AllClauses allClauses = prepare(monitoring,true);
        Clause[] combination = new Clause[4];
        combination[0] = new Clause(1,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{1,3,6,9,13}));
        combination[1] = new Clause(2,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{2,4,7,10,14}));
        combination[2] = new Clause(3,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{5,8,11,15}));
        combination[3] = new Clause(4,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{12,16}));
        MRMatrix matrix = new MRMatrix(allClauses, combination);
        Clause clause1 = new Clause(10, ClauseType.OR, IntArrayList.wrap(new int[]{1, 2}));
        Clause clause2 = new Clause(11, ClauseType.OR, IntArrayList.wrap(new int[]{3,-4,5}));
        Clause clause3 = new Clause(12, ClauseType.OR, IntArrayList.wrap(new int[]{6,7,8}));
        Clause clause4 = new Clause(13, ClauseType.OR, IntArrayList.wrap(new int[]{9,10,11,12}));
        Clause clause5 = new Clause(14, ClauseType.OR, IntArrayList.wrap(new int[]{13,14,15,16}));
        matrix.insertClause(clause5);
        matrix.insertClause(clause3);
        matrix.insertClause(clause2);
        matrix.insertClause(clause1);
        matrix.insertClause(clause4);
        assertEquals("[0, 1]", Arrays.toString(matrix.findFirstColIndices(2)));
        assertEquals("[0, 1, 2]", Arrays.toString(matrix.findFirstColIndices(3)));
        assertNull(matrix.findFirstColIndices(3));
        assertEquals("[0, 1, 2, 3]", Arrays.toString(matrix.findFirstColIndices(4)));
        assertNull(matrix.findFirstColIndices(4));
        //System.out.println(matrix.infoString(null));
        //System.out.println(matrix.toString(null));
    }

    @Test
    public void findBlock() {
        System.out.println("findBlock");
        AllClauses allClauses = prepare(monitoring,true);
        Clause[] combination = new Clause[4];
        combination[0] = new Clause(100,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{1,3,6,9,13}));
        combination[1] = new Clause(2,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{2,4,7,10,14}));
        combination[2] = new Clause(3,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{5,8,11,15}));
        combination[3] = new Clause(4,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{12,16}));
        MRMatrix matrix = new MRMatrix(allClauses, combination);
        Clause clause1 = new Clause(10, ClauseType.OR, IntArrayList.wrap(new int[]{1, 2}));
        Clause clause2 = new Clause(11, ClauseType.OR, IntArrayList.wrap(new int[]{3,4,5}));
        Clause clause3 = new Clause(12, ClauseType.OR, IntArrayList.wrap(new int[]{6,7,8}));
        Clause clause4 = new Clause(13, ClauseType.OR, IntArrayList.wrap(new int[]{9,10,11,12}));
        Clause clause5 = new Clause(14, ClauseType.OR, IntArrayList.wrap(new int[]{13,14,15,16}));
        matrix.insertClause(clause5);
        matrix.insertClause(clause3);
        matrix.insertClause(clause2);
        matrix.insertClause(clause1);
        matrix.insertClause(clause4);
        int[] colIndices = matrix.findFirstColIndices(2);
        ArrayList<CLiteral[]> block = matrix.findBlock(colIndices);
        System.out.println(matrix.block2String(colIndices,block,null));
        colIndices = matrix.findFirstColIndices(3);
        block = matrix.findBlock(colIndices);
        System.out.println(matrix.block2String(colIndices,block,null));
        colIndices = matrix.findFirstColIndices(4);
        block = matrix.findBlock(colIndices);
        System.out.println(matrix.block2String(colIndices,block,symboltable));
        //System.out.println(matrix.infoString(null));
        //System.out.println(matrix.toString(null));
    }

    @Test
    public void mrResolveSquare() throws Unsatisfiable {
        System.out.println("mrResolveSquare");
        AllClauses allClauses = prepare(monitoring,false);
        Clause[] combination = new Clause[4];
        combination[0] = new Clause(100, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{1, 3, 6, 9, 13}));
        combination[1] = new Clause(2, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{2, 4, 7, 10, 13}));
        combination[2] = new Clause(3, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{5, 8, 11, 15}));
        combination[3] = new Clause(4, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{12, 16}));
        MRMatrix matrix = new MRMatrix(allClauses, combination);
        Clause clause1 = new Clause(10, ClauseType.OR, IntArrayList.wrap(new int[]{1, 2}));
        Clause clause2 = new Clause(11, ClauseType.OR, IntArrayList.wrap(new int[]{3, 4, 5}));
        Clause clause3 = new Clause(12, ClauseType.OR, IntArrayList.wrap(new int[]{6, 7, 8}));
        Clause clause4 = new Clause(14, ClauseType.OR, IntArrayList.wrap(new int[]{13, 14, 15, 17}));
        matrix.insertClause(clause1);
        matrix.insertClause(clause2);
        matrix.insertClause(clause3);
        matrix.insertClause(clause4);
        int[] colIndices = matrix.findFirstColIndices(3);
        ArrayList<CLiteral[]> block = matrix.findBlock(colIndices);
        System.out.println(matrix.block2String(colIndices, block, null));
        ArrayList<TwoLitClause > twoLitClauses = new ArrayList<>();
        matrix.mrResolveSquare(colIndices, block, twoLitClauses);
        assertEquals("Model:\n-9,-10,-11,-13,-15",allClauses.model.toNumbers());
        assertTrue(twoLitClauses.isEmpty());
        assertEquals("[2, 3, 10, 11, 12, 100]",sortIntArray(allClauses.model.getInferenceStep(-13).origins()).toString());
        assertEquals("[2, 3, 10, 11, 12, 100]",sortIntArray(allClauses.model.getInferenceStep(-15).origins()).toString());
    }

    @Test
    public void mrResolveSquareUnsat() {
        System.out.println("mrResolveSquare unsatisfiable");
        AllClauses allClauses = prepare(monitoring,true);
        Clause[] combination = new Clause[4];
        combination[0] = new Clause(100, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{1, 3, 6, 9, 13}));
        combination[1] = new Clause(2, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{2, 4, 7, 10, -15}));
        combination[2] = new Clause(3, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{5, 8, 11, 15}));
        combination[3] = new Clause(4, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{12, 16}));
        MRMatrix matrix = new MRMatrix(allClauses, combination);
        Clause clause1 = new Clause(10, ClauseType.OR, IntArrayList.wrap(new int[]{1, 2}));
        Clause clause2 = new Clause(11, ClauseType.OR, IntArrayList.wrap(new int[]{3, 4, 5}));
        Clause clause3 = new Clause(12, ClauseType.OR, IntArrayList.wrap(new int[]{6, 7, 8}));
        matrix.insertClause(clause1);
        matrix.insertClause(clause2);
        matrix.insertClause(clause3);
        int[] colIndices = matrix.findFirstColIndices(3);
        ArrayList<CLiteral[]> block = matrix.findBlock(colIndices);
        System.out.println(matrix.block2String(colIndices, block, null));
        ArrayList<TwoLitClause > twoLitClauses = new ArrayList<>();
        try {
            matrix.mrResolveSquare(colIndices, block, twoLitClauses);
        }
        catch(Unsatisfiable uns) {
            System.out.println(uns);
        }}

    @Test
    public void mrResolveSquareTwoLit() throws Unsatisfiable {
        System.out.println("mrResolveSquare yielding 2-lit clauses");
        AllClauses allClauses = prepare(monitoring,false);
        Clause[] combination = new Clause[3];
        combination[0] = new Clause(100, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{1, 4, 7, 20}));
        combination[1] = new Clause(2, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{2, 5, 8, 21}));
        combination[2] = new Clause(3, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{3, 6, 9, 22}));
        MRMatrix matrix = new MRMatrix(allClauses, combination);
        Clause clause1 = new Clause(10, ClauseType.OR, IntArrayList.wrap(new int[]{1, 2,3}));
        Clause clause2 = new Clause(11, ClauseType.OR, IntArrayList.wrap(new int[]{4, 5,6,10}));
        Clause clause3 = new Clause(12, ClauseType.OR, IntArrayList.wrap(new int[]{7, 8, 9}));
         matrix.insertClause(clause1);
        matrix.insertClause(clause2);
        matrix.insertClause(clause3);
        //System.out.println(matrix.infoString(null));
        int[] colIndices = matrix.findFirstColIndices(3);
        ArrayList<CLiteral[]> block = matrix.findBlock(colIndices);
        System.out.println(matrix.block2String(colIndices, block, null));
        ArrayList<TwoLitClause > twoLitClauses = new ArrayList<>();
        matrix.mrResolveSquare(colIndices, block, twoLitClauses);
        //System.out.println(twoLitClauses.toString());
        assertEquals("[2-10: 10,-20, 2-11: 10,-21, 2-12: 10,-22]", twoLitClauses.toString());
        assertEquals("[2, 3, 10, 11, 12, 100]",sortIntArray(twoLitClauses.get(0).inferenceStep.origins()).toString());
        System.out.println(MRResolutionSquare2.rule);
    }

    @Test
    public void mrResolveRectangle() throws Unsatisfiable {
        System.out.println("mrResolveRectangle");
        AllClauses allClauses = prepare(monitoring,true);
        Clause[] combination = new Clause[3];
        combination[0] = new Clause(100, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{1, 4, 7, 10}));
        combination[1] = new Clause(2, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{2, 5, 8, 11}));
        combination[2] = new Clause(3, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{3, 6, 9, 12}));
        MRMatrix matrix = new MRMatrix(allClauses, combination);
        Clause clause1 = new Clause(10, ClauseType.OR, IntArrayList.wrap(new int[]{1, 2,3}));
        Clause clause2 = new Clause(11, ClauseType.OR, IntArrayList.wrap(new int[]{4, 5,6,20}));
        Clause clause3 = new Clause(12, ClauseType.OR, IntArrayList.wrap(new int[]{7, 8, 9}));
        Clause clause4 = new Clause(13, ClauseType.OR, IntArrayList.wrap(new int[]{10,11,12}));
        matrix.insertClause(clause1);
        matrix.insertClause(clause2);
        matrix.insertClause(clause3);
        matrix.insertClause(clause4);
        System.out.println(matrix.infoString(null));
        int[] colIndices = matrix.findFirstColIndices(3);
        ArrayList<CLiteral[]> block = matrix.findBlock(colIndices);
        System.out.println(matrix.block2String(colIndices, block, null));
        ArrayList<TwoLitClause > twoLitClauses = new ArrayList<>();
        matrix.mrResolveRectangle(colIndices, block, twoLitClauses);
    }

    @Test
    public void mrResolveRectangle2Lit() throws Unsatisfiable {
        System.out.println("mrResolveRectangle Two Lit Clauses");
        AllClauses allClauses = prepare(monitoring,true);
        Clause[] combination = new Clause[3];
        combination[0] = new Clause(100, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{1, 4, 7, 10}));
        combination[1] = new Clause(2, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{2, 5, 8, 11}));
        combination[2] = new Clause(3, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{3, 6, 9, 12}));
        MRMatrix matrix = new MRMatrix(allClauses, combination);
        Clause clause1 = new Clause(10, ClauseType.OR, IntArrayList.wrap(new int[]{1, 2,3}));
        Clause clause2 = new Clause(11, ClauseType.OR, IntArrayList.wrap(new int[]{4, 5,6,20}));
        Clause clause3 = new Clause(12, ClauseType.OR, IntArrayList.wrap(new int[]{7, 8, 9,30}));
        Clause clause4 = new Clause(13, ClauseType.OR, IntArrayList.wrap(new int[]{10,11,12}));
        matrix.insertClause(clause1);
        matrix.insertClause(clause2);
        matrix.insertClause(clause3);
        matrix.insertClause(clause4);
        //System.out.println(matrix.infoString(null));
        int[] colIndices = matrix.findFirstColIndices(3);
        ArrayList<CLiteral[]> block = matrix.findBlock(colIndices);
        //System.out.println(matrix.block2String(colIndices, block, null));
        ArrayList<TwoLitClause > twoLitClauses = new ArrayList<>();
        matrix.mrResolveRectangle(colIndices, block, twoLitClauses);
        assertEquals("[2-0: 20,30]", twoLitClauses.toString());
    }

    @Test
    public void mrResolveRectangleUnsat(){
        System.out.println("mrResolveRectangle Unsatisfiable");
        AllClauses allClauses = prepare(monitoring,true);
        Clause[] combination = new Clause[3];
        combination[0] = new Clause(100, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{1, 4, 7, 10}));
        combination[1] = new Clause(2, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{2, 5, 8, 11}));
        combination[2] = new Clause(3, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{3, 6, 9, 12}));
        MRMatrix matrix = new MRMatrix(allClauses, combination);
        Clause clause1 = new Clause(10, ClauseType.OR, IntArrayList.wrap(new int[]{1, 2,3}));
        Clause clause2 = new Clause(11, ClauseType.OR, IntArrayList.wrap(new int[]{4, 5,6}));
        Clause clause3 = new Clause(12, ClauseType.OR, IntArrayList.wrap(new int[]{7, 8, 9}));
        Clause clause4 = new Clause(13, ClauseType.OR, IntArrayList.wrap(new int[]{10,11,12}));
        matrix.insertClause(clause1);
        matrix.insertClause(clause2);
        matrix.insertClause(clause3);
        matrix.insertClause(clause4);
        //System.out.println(matrix.infoString(null));
        int[] colIndices = matrix.findFirstColIndices(3);
        ArrayList<CLiteral[]> block = matrix.findBlock(colIndices);
        //System.out.println(matrix.block2String(colIndices, block, null));
        ArrayList<TwoLitClause > twoLitClauses = new ArrayList<>();
        try{
        matrix.mrResolveRectangle(colIndices, block, twoLitClauses);}
        catch(Unsatisfiable uns) {System.out.println(uns);}
    }

    @Test
    public void mrResolve1() {
        System.out.println("mrResolve 1");
        AllClauses allClauses = prepare(monitoring,true);
        Clause[] combination = new Clause[3];
        combination[0] = new Clause(100, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{1, 4, 7, 10}));
        combination[1] = new Clause(2, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{2, 5, 8, 11}));
        combination[2] = new Clause(3, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{3, 6, 9, 12}));
        MRMatrix matrix = new MRMatrix(allClauses, combination);
        Clause clause1 = new Clause(10, ClauseType.OR, IntArrayList.wrap(new int[]{1, 2,3}));
        Clause clause2 = new Clause(11, ClauseType.OR, IntArrayList.wrap(new int[]{4, 5,6}));
        Clause clause3 = new Clause(12, ClauseType.OR, IntArrayList.wrap(new int[]{7, 8, 9}));
        Clause clause4 = new Clause(13, ClauseType.OR, IntArrayList.wrap(new int[]{10,11,12}));
        matrix.insertClause(clause1);
        matrix.insertClause(clause2);
        matrix.insertClause(clause3);
        matrix.insertClause(clause4);
        //System.out.println(matrix.infoString(null));
        ArrayList<TwoLitClause > twoLitClauses = new ArrayList<>();
        try{
            matrix.mrResolve(twoLitClauses);}
        catch(Unsatisfiable uns) {System.out.println(uns);}
    }

    @Test
    public void mrResolve2() throws Unsatisfiable {
        System.out.println("mrResolve 2");
        AllClauses allClauses = prepare(monitoring,true);
        Clause[] combination = new Clause[3];
        combination[0] = new Clause(100, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{1, 4, 7, 10}));
        combination[1] = new Clause(2, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{2, 5, 8, 11}));
        combination[2] = new Clause(3, ClauseType.DISJOINT, IntArrayList.wrap(new int[]{3, 6, 9,12}));
        MRMatrix matrix = new MRMatrix(allClauses, combination);
        Clause clause1 = new Clause(10, ClauseType.OR, IntArrayList.wrap(new int[]{1, 2,3}));
        Clause clause2 = new Clause(11, ClauseType.OR, IntArrayList.wrap(new int[]{4, 5,6,20}));
        Clause clause3 = new Clause(12, ClauseType.OR, IntArrayList.wrap(new int[]{7, 8, 9}));
        Clause clause4 = new Clause(13, ClauseType.OR, IntArrayList.wrap(new int[]{10,11}));
        matrix.insertClause(clause1);
        matrix.insertClause(clause2);
        matrix.insertClause(clause3);
        matrix.insertClause(clause4);
        //System.out.println(matrix.infoString(null));
        ArrayList<Clause> oneLitClauses = new ArrayList<>();
        ArrayList<TwoLitClause > twoLitClauses = new ArrayList<>();
        matrix.mrResolve(twoLitClauses);
        System.out.println(oneLitClauses);
        System.out.println(twoLitClauses);

        assertEquals("[A-0: 20]", oneLitClauses.toString());
        assertEquals("[2-0: 3,9]", twoLitClauses.toString());
    }

    private boolean contains (int i, int[] ints) {
        for(int k : ints) if(k == i) return true;
        return false;}

    @Test
    public void mrResolveRandom() throws Unsatisfiable {
        System.out.println("mrResolve random");
        AllClauses allClauses = prepare(monitoring,true);
        Random ramdom = new Random(0);
        int combSize = 10;
        int clauseSize = 50;
        Clause[] combination = new Clause[combSize];
        for (int i = 0; i < combSize; ++i) {
            int size = ramdom.nextInt(8) + 3;
            int[] lits = new int[size];
            for (int l = 0; l < size; ++l) {
                int lit =  ramdom.nextInt(50);
                if(contains(lit,lits)) {--l; continue;}
                lits[l] = lit;}
            combination[i] = new Clause(i, ClauseType.DISJOINT, IntArrayList.wrap(lits));
        }
        MRMatrix matrix = new MRMatrix(allClauses, combination);
        for (int i = 0; i < clauseSize; ++i) {
            int size = ramdom.nextInt(5) + 2;
            int[] lits = new int[size];
            for (int l = 0; l < size; ++l) {
                int lit =  ramdom.nextInt(50);
                if(contains(lit,lits)) {--l; continue;}
                lits[l] = lit;}
            matrix.insertClause(new Clause(100 + i, ClauseType.OR, IntArrayList.wrap(lits)));
        }
        System.out.println(matrix.infoString(null));
        System.out.println(matrix.toString(null));

        ArrayList<Clause> oneLitClauses = new ArrayList<>();
        ArrayList<TwoLitClause > twoLitClauses = new ArrayList<>();
        matrix.mrResolve(twoLitClauses);
        System.out.println(oneLitClauses);
        System.out.println(twoLitClauses);
        assertEquals("[2-0: 21,-27, 2-0: 21,-12, 2-0: 21,-45, 2-0: 21,-44, 2-0: 21,-34, 2-0: 21,-25, 2-0: 21,-41, 2-0: 21,-20, 2-0: 21,-48, 2-0: 21,-24, 2-0: 21,-19, 2-0: 21,-46, 2-0: 13,-45, 2-0: 13,-37, 2-0: 13,-12, 2-0: 13,-25, 2-0: 13,-3, 2-0: 13,-38, 2-0: 13,-35, 2-0: 13,-10, 2-0: 13,-18, 2-0: 13,-22, 2-0: 13,-7, 2-0: 13,-1]",
                twoLitClauses.toString());
    }

    @Test
    public void pidgeon()  {
        System.out.println("pidgeon hole");
        AllClauses allClauses = prepare(monitoring,true);
        int hole = 30;
        int pidgeon = 31;
        Clause[] combination = new Clause[hole];
        for(int h = 1; h <= hole; ++h) {
            int[] lits = new int[pidgeon];
            for(int p = 1; p <= pidgeon; ++p) {lits[p-1] = p*100+h;}
            combination[h-1] = new Clause(h, ClauseType.DISJOINT, IntArrayList.wrap(lits));
            }
        MRMatrix matrix = new MRMatrix(allClauses, combination);
        for(int p = 1; p <= pidgeon; ++p) {
            int[] lits = new int[hole];
            for(int h = 1; h <= hole; ++h) {
                lits[h-1] = p*100+h;}
            matrix.insertClause(new Clause(1000 + p, ClauseType.OR, IntArrayList.wrap(lits)));
        }
        System.out.println(matrix.infoString(null));
        System.out.println(matrix.toString(null));

        ArrayList<TwoLitClause > twoLitClauses = new ArrayList<>();

        try{matrix.mrResolve(twoLitClauses);}
        catch(Unsatisfiable uns) {System.out.println(uns);}
        }
    @Test
    public void pidgeonDerivation()  {
        System.out.println("pidgeon hole with derivation");
        AllClauses allClauses = prepare(monitoring,true);
        int hole = 10;
        int pidgeon = 11;
        Clause[] combination = new Clause[hole];
        for(int h = 1; h <= hole; ++h) {
            int[] lits = new int[pidgeon];
            for(int p = 1; p <= pidgeon; ++p) {lits[p-1] = p*100+h;}
            combination[h-1] = new Clause(h, ClauseType.DISJOINT, IntArrayList.wrap(lits));
        }
        MRMatrix matrix = new MRMatrix(allClauses, combination);
        for(int p = 1; p <= pidgeon; ++p) {
            int[] lits = new int[hole + (p == 1 ? 1 : 0)];
            if(p == 1) lits[hole] = -1;
            for(int h = 1; h <= hole; ++h) {
                lits[h-1] = p*100+h;}
            matrix.insertClause(new Clause(1000 + p, ClauseType.OR, IntArrayList.wrap(lits)));
        }
        System.out.println(matrix.infoString(null));
        System.out.println(matrix.toString(null));

        ArrayList<Clause> oneLitClauses = new ArrayList<>();
        ArrayList<TwoLitClause > twoLitClauses = new ArrayList<>();

        try{matrix.mrResolve(twoLitClauses);}
        catch(Unsatisfiable uns) {System.out.println(uns);}
        System.out.println(oneLitClauses);
        assertEquals("[A-0: -1]",oneLitClauses.toString());
    }
    

    }