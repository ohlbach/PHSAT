package Solvers;

import Datastructures.Clauses.Connective;
import Datastructures.Clauses.InputClauses;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.Model;
import junit.framework.TestCase;

import java.util.Arrays;

public class ClausePurifierTest extends TestCase {
    private static final int cOr = Connective.OR.ordinal();
    private static final int cEquiv = Connective.EQUIV.ordinal();
    private static final int cAtleast = Connective.ATLEAST.ordinal();
    private static final int cAtmost   = Connective.ATMOST.ordinal();
    private static final int cExactly  = Connective.EXACTLY.ordinal();
    private static final int cInterval = Connective.INTERVAL.ordinal();

    public void testCountZeros() {
        System.out.println("countZeros");
        int[] clause1 = new int[]{10,cAtleast,2,1,2,3};
        assertEquals(0,ClausePurifier.countZeros(clause1));
        int[] clause2 = new int[]{11,cAtmost,2,0,2,0,0};
        assertEquals(3,ClausePurifier.countZeros(clause2));
        int[] clause3 = new int[]{12,cInterval,2,3,0,2,0,0};
        assertEquals(3,ClausePurifier.countZeros(clause3));
    }

    public void testCompactify(){
        System.out.println("compactify");
        Model model = new Model(10);
        ClausePurifier cp = new ClausePurifier(model);
        int[] clause = new int[]{10, cAtleast, 2, 1, 2, 3};
        assertTrue(clause == cp.compactify(clause,0));
        clause = new int[]{11, cAtmost, 2, 1, 0, 2, 0,0, 3,0};
        assertEquals("[11, 4, 2, 1, 2, 3]", Arrays.toString(cp.compactify(clause,4)));

        clause = new int[]{12, cInterval, 2, 3, 1, 0, 2, 0,0, 3,0};
        assertEquals("[12, 6, 2, 3, 1, 2, 3]", Arrays.toString(cp.compactify(clause,4)));
        clause = new int[]{13, cOr, 2, 3, 1, 0, 2, 0,0, 3,0};
        assertEquals("[13, 0, 2, 3, 1, 2, 3]", Arrays.toString(cp.compactify(clause,4)));
        }
    public void testMakeAllTrue() throws Result {
        System.out.println("makeAllTrue");
        Model model = new Model(10);
        ClausePurifier cp = new ClausePurifier(model);
        int[] clause = new int[]{10, cAtleast, 2, 0, 1, 2, 0, 3};
        cp.makeAllTrue(clause,3,1);
        assertEquals("1,2,3",model.toString());
        clause = new int[]{11, cInterval, 2, 3, 0, 4, 1, 0, 5};
        try{
            cp.makeAllTrue(clause,4,-1);
            assertTrue(false);}
        catch(Unsatisfiable unsatisfiable) {
            //System.out.println(unsatisfiable.toString());
        }}



    public void testPurifyDisjunction() throws Result {
        System.out.println("purifyDisjunction");
        Model model = new Model(10);
        ClausePurifier cp = new ClausePurifier(model);
        int[] oClause1 = new int[]{10, cOr,1,2,3};
        int[] pClause1 = cp.purifyDisjunction(oClause1, true);
        assertTrue(oClause1 == pClause1);

        int[] oClause2 = new int[]{11, cOr,1,2,3,2,1,2};
        int[] pClause2 = cp.purifyDisjunction(oClause2, true);
        assertEquals("[11, 0, 1, 2, 3]", Arrays.toString(pClause2));

        int[] oClause3 = new int[]{12, cOr,1,2,3,2,1,-2};
        int[] pClause3 = cp.purifyDisjunction(oClause3, true);
        assertNull(pClause3);

        int[] oClause4 = new int[]{13, cOr,1};
        int[] pClause4 = cp.purifyDisjunction(oClause4, true);
        assertNull(pClause4);
        assertEquals("1", model.toString());

        int[] oClause5 = new int[]{14, cOr,2,2,2,2};
        int[] pClause5 = cp.purifyDisjunction(oClause5, true);
        assertNull(pClause5);
        assertEquals("1,2", model.toString());
    }

    public void testPurifyEquivalence() throws Result {
        System.out.println("purifyEquivalence");
        Model model = new Model(10);
        ClausePurifier cp = new ClausePurifier(model);
        int[] oClause1 = new int[]{10, cEquiv, 1, 2, 3};
        int[] pClause1 = cp.purifyEquivalence(oClause1);
        assertTrue(oClause1 == pClause1);

        int[] oClause2 = new int[]{11, cEquiv, 1, 2, 3, 2, 1, 2};
        int[] pClause2 = cp.purifyEquivalence(oClause2);
        assertEquals("[11, 2, 1, 2, 3]", Arrays.toString(pClause2));

        int[] oClause3 = new int[]{12, cEquiv, 1, 2, 3, 2, -1, 2};
        try{cp.purifyEquivalence(oClause3);
            assertTrue(false);}
        catch(Result result) {
            //System.out.println(result.toString());
        }
    }

        public void testmakeDisjunction() throws Result {
        System.out.println("makeDisjunction");
        Model model = new Model(10);
        ClausePurifier cp = new ClausePurifier(model);
        int[] clause = new int[]{10, cAtleast,1,2,2,2,0,3,3};
        assertEquals("[10, 0, 2, 3]",Arrays.toString(cp.makeDisjunction(clause,3,5,1)));
        clause = new int[]{11, cInterval,1,5, 2,2,2,0,3,3};
        assertEquals("[11, 0, -2, -3]",Arrays.toString(cp.makeDisjunction(clause,4,5,-1)));
        clause = new int[]{12, cAtleast,1,2,2,2,0,3,3,-2};
        assertNull(cp.makeDisjunction(clause,3,6,-1));
        clause = new int[]{13, cAtleast,1,2,2,2,0,2,0};
        assertNull(cp.makeDisjunction(clause,3,4,-1));
        assertEquals("-2",model.toString());
        clause = new int[]{14, cAtleast,1,2,2,2,0,2,0};
        try{
            cp.makeDisjunction(clause,3,4,1);}
        catch(Unsatisfiable unsatisfiable) {
            //System.out.println(unsatisfiable.toString());
        }
    }
    public void testmakeEquivalence() throws Result {
        System.out.println("makeEquivalence");
        Model model = new Model(10);
        ClausePurifier cp = new ClausePurifier(model);
        int[] clause = new int[]{10, cInterval, 1, 1, 2, 0, 3,0};
        assertEquals("[10, 2, 2, -3]",Arrays.toString(cp.makeEquivalence(clause,4)));

        clause = new int[]{11, cExactly, 1, 2, 0, 3,0};
        assertEquals("[11, 2, 2, -3]",Arrays.toString(cp.makeEquivalence(clause,3)));
    }

    public void testTrueLiteralsFromMultiplicities() throws Result {
        System.out.println("TrueLiteralsFromMultiplicities");
        Model model = new Model(20);
        ClausePurifier cp = new ClausePurifier(model);
        int[] clause = new int[]{10, cAtleast, 2, 1, 0,1,2,0};
        assertNull(cp.trueLiteralsFromMultiplicities(clause,true));
        assertEquals("1",model.toString());
        clause = new int[]{11, cAtleast, 5, 3,3,4,4,5,6};
        assertEquals("[11, 0, 5, 6]",Arrays.toString(cp.trueLiteralsFromMultiplicities(clause,true)));
        clause = new int[]{12, cExactly, 4, 7,7,8,9,9};
        assertNull(cp.trueLiteralsFromMultiplicities(clause,true));
        assertEquals("1,3,4,7,-8,9",model.toString());
        clause = new int[]{13, cAtleast, 4, 10,11,10,0,0,10,11,12,13};
        assertEquals(clause,cp.trueLiteralsFromMultiplicities(clause,false));
        clause = new int[]{14, cExactly, 5, 10,11,10,0,0,10,11,12};
        assertNull(cp.trueLiteralsFromMultiplicities(clause,true));
        assertEquals("1,3,4,7,-8,9,10,11,-12",model.toString());
    }


    public void testReduceMultiplicities() throws Result {
        System.out.println("reduceMultiplicities");
        Model model = new Model(20);
        ClausePurifier cp = new ClausePurifier(model);
        int[] clause = new int[]{10, cAtleast, 2, 1, 2, 3};
        assertEquals(clause,cp.reduceMultiplicities(clause,true));
        clause = new int[]{11, cAtleast, 2, 1, 2,2, 3,2};
        assertEquals("[11, 3, 2, 1, 2, 2, 3, 0]",Arrays.toString(cp.reduceMultiplicities(clause,true)));
        clause = new int[]{12, cAtleast, 2, 1, 2,2, 3,2,3,2,3,1};
        assertEquals("[12, 3, 2, 1, 2, 2, 3, 0, 3, 0, 0, 1]",Arrays.toString(cp.reduceMultiplicities(clause,true)));
        clause = new int[]{13, cExactly, 2, 1, 2,2, 3,2,3,2,3,1};
        assertNull(cp.reduceMultiplicities(clause,false));
        assertEquals("1,-2,-3",model.toString());
        clause = new int[]{14, cInterval, 1,2, 11, 12,12, 13,12,13,12,13,11}; assertNull(cp.reduceMultiplicities(clause,false));
        assertEquals("1,-2,-3,11,-12,-13",model.toString());
    }
    public void testEliminateComplementaryPairs() throws Result {
        System.out.println("eliminateComplementaryPairs");
        Model model = new Model(20);
        ClausePurifier cp = new ClausePurifier(model);
        int[] clause = new int[]{10, cAtleast, 2, 1, 2, 3};
        assertEquals(clause, cp.eliminateComplementaryPairs(clause, true));
        clause = new int[]{11, cAtleast, 2, 1, 2, 3, -2};
        assertEquals("[11, 0, 1, 3]", Arrays.toString(cp.eliminateComplementaryPairs(clause, true)));
        clause = new int[]{12, cAtleast, 2, 1, 2, 3, -2,2};
        assertEquals("[12, 0, 1, 3, 2]", Arrays.toString(cp.eliminateComplementaryPairs(clause, true)));
        clause = new int[]{13, cExactly, 2, 1, 2, 3, -2,2};
        assertEquals("[13, 5, 1, 1, 0, 3, 0, 2]", Arrays.toString(cp.eliminateComplementaryPairs(clause, true)));
        clause = new int[]{14, cInterval,2, 3, 1, 2, 3, -2,-1,2,-2};
        assertNull(cp.eliminateComplementaryPairs(clause, true));
        assertEquals("-3",model.toString());
    }

    public void testTransformOr() throws Result {
        System.out.println("transform or");
        Model model = new Model(20);
        ClausePurifier cp = new ClausePurifier(model);
        int[] clause = new int[]{10, cOr, 1, 2, 3};
        assertEquals(clause,cp.transform(clause,0));
        clause = new int[]{11, cOr, 0, 20, 0};
        assertNull(cp.transform(clause,2));
        assertEquals("20",model.toString());
        try{
            clause = new int[]{12, cOr, 0, 0, 0};
            assertNull(cp.transform(clause,3));
            assertTrue(false);}
        catch(Unsatisfiable unsatisfiable) {
            //System.out.println(unsatisfiable.toString());
        }
    }

    public void testTransformAtleast() throws Result {
        System.out.println("transform atleast");
        Model model = new Model(20);
        ClausePurifier cp = new ClausePurifier(model);
        int[] clause = new int[]{10, cAtleast, 2, 1, 2, 3};
        assertEquals(clause,cp.transform(clause,0));
        clause = new int[]{11, cAtleast, 0, 1, 2, 3};
        assertNull(cp.transform(clause,0));
        clause = new int[]{12, cAtleast, 3, 1, 2,0, 3};
        assertNull(cp.transform(clause,1));
        assertEquals("1,2,3",model.toString());
        clause = new int[]{13, cAtleast, 1, 1, 2,0, 3};
        assertEquals("[13, 0, 1, 2, 3]",Arrays.toString(cp.transform(clause,1)));
        try{
            clause = new int[]{13, cAtleast, 4, 1, 2,0, 3};
            cp.transform(clause,1);
            assertTrue(false);}
        catch(Unsatisfiable unsatisfiable) {
            //System.out.println(unsatisfiable.toString());
        }
    }

    public void testTransformAtmost() throws Result {
        System.out.println("transform atmost");
        Model model = new Model(20);
        ClausePurifier cp = new ClausePurifier(model);
        int[] clause = new int[]{10, cAtmost, 2, 1, 2, 3, 4};
        assertEquals(clause, cp.transform(clause, 0));
        clause = new int[]{11, cAtmost, 2, 1, 2, 0, 3};
        assertEquals("[11, 0, -1, -2, -3]",Arrays.toString(cp.transform(clause,1)));
        clause = new int[]{12, cAtmost, 0, 1, 2, 0, 3};
        assertNull(cp.transform(clause,1));
        assertEquals("-1,-2,-3",model.toString());
        clause = new int[]{13, cAtmost, 3, 1, 2, 0, 3};
        assertNull(cp.transform(clause,1));
        try{
            clause = new int[]{14, cAtmost, -1, 1, 2, 0, 3};
            cp.transform(clause,1);
            assertTrue(false);
        }
        catch (Unsatisfiable unsatisfiable) {
            //System.out.println(unsatisfiable);
        }
    }

    public void testTransformExactly() throws Result {
        System.out.println("transform exactly");
        Model model = new Model(20);
        ClausePurifier cp = new ClausePurifier(model);
        int[] clause = new int[]{10, cExactly, 2, 1, 2, 3, 0,4};
        assertEquals(clause, cp.transform(clause, 1));
        clause = new int[]{12, cExactly, 0, 1, 2, 0, 3};
        assertNull(cp.transform(clause, 1));
        assertEquals("-1,-2,-3",model.toString());
        clause = new int[]{13, cExactly, 3, 11, 12, 0, 13};
        assertNull(cp.transform(clause, 1));
        assertEquals("-1,-2,-3,11,12,13",model.toString());
        clause = new int[]{14, cExactly, 1, 4, 0, 5};
        assertEquals("[14, 2, 4, -5]", Arrays.toString(cp.transform(clause, 1)));
        try {
            clause = new int[]{11, cExactly, 4, 1, 2, 0, 3};
            cp.transform(clause, 1);
            assertTrue(false);}
        catch(Unsatisfiable unsatisfiable) {
            //System.out.println(unsatisfiable.toString());
        }
    }

    public void testTransformInterval() throws Result {
        System.out.println("transform interval");
        Model model = new Model(20);
        ClausePurifier cp = new ClausePurifier(model);
        int[] clause = new int[]{10, cInterval, 1, 3, 2, 3, 0, 4};
        assertEquals("[10, 0, 2, 3, 4]", Arrays.toString(cp.transform(clause, 1)));
        clause = new int[]{11, cInterval, 1, 1, 2, 3, 0};
        assertEquals("[11, 2, 2, -3]", Arrays.toString(cp.transform(clause, 1)));
        clause = new int[]{12, cInterval, 1, 1, 2, 3, 0,4};
        assertEquals("[12, 5, 1, 2, 3, 4]", Arrays.toString(cp.transform(clause, 1)));
        clause = new int[]{13, cInterval, 0, 0, 2, 3, 0,4};
        assertNull(cp.transform(clause, 1));
        assertEquals("-2,-3,-4", model.toString());
        clause = new int[]{14, cInterval, 3, 3, 12, 13, 0,14};
        assertNull(cp.transform(clause, 1));
        assertEquals("-2,-3,-4,12,13,14", model.toString());
        try{
            clause = new int[]{15, cInterval, 4, 4, 12, 13, 0,14};
            cp.transform(clause, 1);
            assertTrue(false);}
        catch(Unsatisfiable unsatisfiable) {
            //System.out.println(unsatisfiable);
        }
    }

    public void testPurifyDisjunctions() throws Result {
        System.out.println("purify disjunction");
        Model model = new Model(20);
        InputClauses inputClauses = new InputClauses("TestProblem",20,null,"Purify Disjunction");
        ClausePurifier cp = new ClausePurifier(inputClauses, model);
        int[] clause = new int[]{10, cOr, 1, 2, 3,4};
        inputClauses.addClause(clause);
        assertEquals(1,inputClauses.disjunctions.size());
        cp.purifyClauses();
        assertEquals(1,inputClauses.disjunctions.size());
        assertTrue(inputClauses.disjunctions == inputClauses.purifiedDisjunctions);
        clause = new int[]{11, cOr, 1, 2, 3,4,2};
        inputClauses.addClause(clause);
        cp.purifyClauses();
        assertEquals(2,inputClauses.disjunctions.size());
        assertEquals(2,inputClauses.purifiedDisjunctions.size());
        assertFalse(inputClauses.disjunctions == inputClauses.purifiedDisjunctions);
        assertEquals("[10, 0, 1, 2, 3, 4]",Arrays.toString(inputClauses.purifiedDisjunctions.get(0)));
        assertEquals("[11, 0, 1, 2, 3, 4]",Arrays.toString(inputClauses.purifiedDisjunctions.get(1)));
        clause = new int[]{12, cOr, 1, 2, 3,4,-2};
        inputClauses.addClause(clause);
        cp.purifyClauses();
        assertEquals(2,inputClauses.purifiedDisjunctions.size());

        inputClauses = new InputClauses("TestProblem",20,null,"Purify Disjunction");
        cp = new ClausePurifier(inputClauses, model);
        clause = new int[]{10, cOr, 1, 2, 3,4,-2};
        inputClauses.addClause(clause);
        try{cp.purifyClauses();}catch(Result result){}
        assertFalse(inputClauses.disjunctions == inputClauses.purifiedDisjunctions);
        assertEquals(1,inputClauses.disjunctions.size());
        assertEquals(0,inputClauses.purifiedDisjunctions.size());
    }

    public void testPurifyAtleasts() throws Result {
        System.out.println("purify atleasts");
        Model model = new Model(20);
        InputClauses inputClauses = new InputClauses("TestProblem", 20, null, "Purify Atleasts");
        ClausePurifier cp = new ClausePurifier(inputClauses, model);
        int[] clause = new int[]{10, cAtleast, 2, 1, 2, 3, 4};
        inputClauses.addClause(clause);
        cp.purifyClauses();
        assertEquals(1, inputClauses.atleasts.size());
        assertTrue(inputClauses.atleasts == inputClauses.purifiedAtleasts);

        clause = new int[]{11, cAtleast, 3, 1, 2, 3, 4, -2, 2};
        inputClauses.addClause(clause);
        cp.purifyClauses();
        assertEquals(2, inputClauses.atleasts.size());
        assertEquals(2, inputClauses.purifiedAtleasts.size());
        assertFalse(inputClauses.atleasts == inputClauses.purifiedAtleasts);
        assertEquals("[11, 3, 2, 1, 3, 4, 2]",Arrays.toString(inputClauses.purifiedAtleasts.get(1)));

        inputClauses = new InputClauses("TestProblem", 20, null, "Purify Atleasts");
        cp = new ClausePurifier(inputClauses, model);
        clause = new int[]{10, cAtleast, 2, 1, 2, 3, 4};
        inputClauses.addClause(clause);
        clause = new int[]{11, cAtleast, 2, 1, 2, 3, 4, -2, 2};
        inputClauses.addClause(clause);
        cp.purifyClauses();
        assertEquals(2, inputClauses.atleasts.size());
        assertEquals(1, inputClauses.purifiedAtleasts.size());
        assertEquals(1, inputClauses.purifiedDisjunctions.size());
        assertEquals("[11, 0, 1, 3, 4, 2]",Arrays.toString(inputClauses.purifiedDisjunctions.get(0)));

        inputClauses = new InputClauses("TestProblem", 20, null, "Purify Atleasts");
        cp = new ClausePurifier(inputClauses, model);
        clause = new int[]{10, cAtleast, 2, 1, 2, -1, -2, 3, 4};
        inputClauses.addClause(clause);
        try{cp.purifyClauses();}catch(Result result){}
        assertFalse(inputClauses.atleasts == inputClauses.purifiedAtleasts);
        assertEquals(0, inputClauses.purifiedAtleasts.size());
    }


    public void testPurifyAtmosts() throws Result {
        System.out.println("purify atmosts");
        Model model = new Model(20);
        InputClauses inputClauses = new InputClauses("TestProblem", 20, null, "Purify Atmosts");
        ClausePurifier cp = new ClausePurifier(inputClauses, model);
        int[] clause = new int[]{10, cAtmost, 2, 1, 2, 3, 4};
        inputClauses.addClause(clause);
        cp.purifyClauses();
        assertEquals(1, inputClauses.atmosts.size());
        assertTrue(inputClauses.atmosts == inputClauses.purifiedAtmosts);

        clause = new int[]{11, cAtmost, 3, 1, 2, 3, 4, -2, 2};
        inputClauses.addClause(clause);
        cp.purifyClauses();
        assertEquals(2, inputClauses.atmosts.size());
        assertEquals(2, inputClauses.purifiedAtmosts.size());
        assertFalse(inputClauses.atmosts == inputClauses.purifiedAtmosts);
        assertEquals("[11, 4, 2, 1, 3, 4, 2]",Arrays.toString(inputClauses.purifiedAtmosts.get(1)));

        inputClauses = new InputClauses("TestProblem", 20, null, "Purify Atmosts");
        cp = new ClausePurifier(inputClauses, model);
        clause = new int[]{10, cAtmost, 2, 1, 2, 3, 4};
        inputClauses.addClause(clause);
        clause = new int[]{11, cAtmost, 3, 1, 2, 3, -2, 2};
        inputClauses.addClause(clause);
        cp.purifyClauses();
        assertEquals(2, inputClauses.atmosts.size());
        assertEquals(1, inputClauses.purifiedAtmosts.size());
        assertEquals(1, inputClauses.purifiedDisjunctions.size());
        assertEquals("[11, 0, -1, -3, -2]",Arrays.toString(inputClauses.purifiedDisjunctions.get(0)));

        inputClauses = new InputClauses("TestProblem", 20, null, "Purify Atmosts");
        cp = new ClausePurifier(inputClauses, model);
        clause = new int[]{10, cAtmost, 4, 1, 2, 3, 4};
        inputClauses.addClause(clause);
        try{cp.purifyClauses();}catch(Result result){}
        assertFalse(inputClauses.atmosts == inputClauses.purifiedAtmosts);
        assertTrue(inputClauses.purifiedAtmosts.isEmpty());
    }
    public void testPurifyExactlys() throws Result {
        System.out.println("purify exactlys");
        Model model = new Model(20);
        InputClauses inputClauses = new InputClauses("TestProblem", 20, null, "Purify Exactlys");
        ClausePurifier cp = new ClausePurifier(inputClauses, model);
        int[] clause = new int[]{10, cExactly, 2, 1, 2, 3, 4};
        inputClauses.addClause(clause);
        cp.purifyClauses();
        assertEquals(1, inputClauses.exacltys.size());

        clause = new int[]{11, cExactly, 3, 1, 2, 3, 4, -2, -1, 5};
        inputClauses.addClause(clause);
        cp.purifyClauses();
        assertEquals(2, inputClauses.exacltys.size());
        assertEquals(2, inputClauses.purifiedExactlys.size());
        assertFalse(inputClauses.exacltys == inputClauses.purifiedExactlys);
        assertEquals("[11, 5, 1, 3, 4, 5]",Arrays.toString(inputClauses.purifiedExactlys.get(1)));

        inputClauses = new InputClauses("TestProblem", 20, null, "Purify Exactlys");
        cp = new ClausePurifier(inputClauses, model);
        clause = new int[]{12, cExactly, 3, 1, 2, 3, 4,-3,-4};
        inputClauses.addClause(clause);
        try{cp.purifyClauses();}catch(Result result){}
        assertTrue(inputClauses.purifiedExactlys.isEmpty());
        assertEquals(1, cp.newEquivalences.size());
        assertEquals("[12, 2, 1, -2]",Arrays.toString(cp.newEquivalences.get(0)));
    }

    public void testPurifyIntervals() throws Result {
        System.out.println("purify intervals");
        Model model = new Model(20);
        InputClauses inputClauses = new InputClauses("TestProblem", 20, null, "Purify Intervals");
        ClausePurifier cp = new ClausePurifier(inputClauses, model);
        int[] clause = new int[]{10, cInterval, 2, 3, 1, 2, 3, 4};
        inputClauses.addClause(clause);
        cp.purifyClauses();
        assertTrue(inputClauses.intervals == inputClauses.purifiedIntervals);

        clause = new int[]{11, cInterval, 2, 3, 1, 2, 3, 4, -3};
        inputClauses.addClause(clause);
        cp.purifyClauses();
        assertFalse(inputClauses.intervals == inputClauses.purifiedIntervals);
        assertEquals(2, inputClauses.intervals.size());
        assertEquals(2, inputClauses.purifiedIntervals.size());
        assertEquals("[11, 6, 1, 2, 1, 2, 4]",Arrays.toString(inputClauses.purifiedIntervals.get(1)));

        inputClauses = new InputClauses("TestProblem", 20, null, "Purify Intervals");
        cp = new ClausePurifier(inputClauses, model);
        clause = new int[]{11, cInterval, 2, 2, 1, 2, 3, -3};
        inputClauses.addClause(clause);
        cp.purifyClauses();
        assertTrue(inputClauses.purifiedIntervals.isEmpty());
        assertEquals(1, cp.newEquivalences.size());
        assertEquals("[11, 2, 1, -2]",Arrays.toString(cp.newEquivalences.get(0)));

        inputClauses = new InputClauses("TestProblem", 20, null, "Purify Intervals");
        cp = new ClausePurifier(inputClauses, model);
        clause = new int[]{11, cInterval, 1, 3, 1, 2, 3};
        inputClauses.addClause(clause);
        cp.purifyClauses();
        assertFalse(inputClauses.intervals == inputClauses.purifiedIntervals);
        assertEquals(1, inputClauses.intervals.size());
        assertEquals(0, inputClauses.purifiedIntervals.size());
    }

    }