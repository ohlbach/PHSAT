package Solvers;

import Datastructures.Clauses.Connective;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.Model;
import junit.framework.TestCase;

import java.util.Arrays;

public class ClausePurifierTest extends TestCase {
    private static final int cOr = Connective.OR.ordinal();
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
    public void testMakeAllTrue() throws Unsatisfiable {
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



    public void testPurifyDisjunction() throws Unsatisfiable {
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

    public void testmakeDisjunction() throws Unsatisfiable {
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
    public void testmakeEquivalence() throws Unsatisfiable {
        System.out.println("makeEquivalence");
        Model model = new Model(10);
        ClausePurifier cp = new ClausePurifier(model);
        int[] clause = new int[]{10, cInterval, 1, 1, 2, 0, 3,0};
        assertEquals("[10, 2, 2, -3]",Arrays.toString(cp.makeEquivalence(clause,4)));

        clause = new int[]{11, cExactly, 1, 2, 0, 3,0};
        assertEquals("[11, 2, 2, -3]",Arrays.toString(cp.makeEquivalence(clause,3)));
    }

    public void testTrueLiteralsFromMultiplicities() throws Unsatisfiable {
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
        assertEquals("1,2,3,4,7,-8,9",model.toString());
        clause = new int[]{13, cAtleast, 4, 10,11,10,0,0,10,11,12,13};
        clause = new int[]{13, cAtleast, 4, 1,1,1,2,2,0,0,3,4};
        assertEquals("",Arrays.toString(cp.trueLiteralsFromMultiplicities(clause,false)));
    }
        public void testPurifyAtleast() throws Unsatisfiable {
    /*
        System.out.println("purifyAtleast");
        Model model = new Model(10);
        int[] oClause1 = new int[]{10, cAtleast, 2, 1, 2, 3, 2};
        int[] pClause1 = ClausePurifier.purifyAtleast(oClause1, model);
        assertTrue(oClause1 == pClause1);

        int[] oClause2 = new int[]{11, cAtleast, 2, 1, 2, 3, -2};
        int[] pClause2 = ClausePurifier.purifyAtleast(oClause2, model);
        assertEquals("[11, 0, 1, 3]", Arrays.toString(pClause2));

        int[] oClause3 = new int[]{12, cAtleast, 5, 1, 2, 3, -2, 2,3,-3,4,-3};
        int[] pClause3 = ClausePurifier.purifyAtleast(oClause3, model);
        assertEquals("[12, 4, 2, 1, 2, 4]", Arrays.toString(pClause3));

        int[] oClause4 = new int[]{13, cAtleast, 3, 1, 2, 3, -2, 2,3,-3,4,-3};
        int[] pClause4 = ClausePurifier.purifyAtleast(oClause4, model);
        assertNull(pClause4);
        assertEquals("",model.toString());

        int[] oClause5 = new int[]{14, cAtleast, 6, 1, 2, 3, -2, 2,3,-3,4,-3};
        int[] pClause5 = ClausePurifier.purifyAtleast(oClause5, model);
        assertNull(pClause5);
        assertEquals("1,2,4",model.toString());

        int[] oClause6 = new int[]{15, cAtleast, 2, 1, 2, 3, 2, 2,3,3,4,3};
        int[] pClause6 = ClausePurifier.purifyAtleast(oClause6, model);
        assertEquals("[15, 4, 2, 1, 2, 2, 3, 4, 3]", Arrays.toString(pClause6));


        try{
            int[] oClause16 = new int[]{16, cAtleast, 2,-4,-5};
            int[] pClause16 = ClausePurifier.purifyAtleast(oClause16, model);
            assertTrue(false);}
        catch(Unsatisfiable unsatisfiable) {
            System.out.println(unsatisfiable.toString());
        }
*/
    }

    }