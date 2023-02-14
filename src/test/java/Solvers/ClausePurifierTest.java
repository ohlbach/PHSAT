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

    public void testCompactify() throws Unsatisfiable {
        System.out.println("compactify");
        Model model = new Model(10);
        int[] clause1 = new int[]{10, cAtleast, 2, 1, 2, 3};
        assertTrue(clause1 == ClausePurifier.compactify(false,clause1,model));
        int[] clause2 = new int[]{11, cAtleast, 2, 1, 2, 0, 3, 0};
        assertEquals("[11, 4, 2, 1, 2, 3]", Arrays.toString(ClausePurifier.compactify(false,clause2,model)));
        int[] clause3 = new int[]{12, cAtleast, 1, 1, 2, 0, 3, 0};
        assertEquals("[12, 0, 1, 2, 3]", Arrays.toString(ClausePurifier.compactify(true,clause3,model)));
        int[] clause4 = new int[]{13, cInterval, 2, 3, 1, 2, 3};
        assertTrue(clause4 == ClausePurifier.compactify(false,clause4,model));
        int[] clause5 = new int[]{14, cInterval, 2, 3, 0, 1, 0, 2,0, 3,0};
        assertEquals("[14, 3, 2, 3, 1, 2, 3]", Arrays.toString(ClausePurifier.compactify(false,clause5,model)));
        int[] clause6 = new int[]{15, cInterval, 1, 3, 0, 1, 0, 2,0, 3,0};
        assertEquals("[15, 0, 1, 2, 3]", Arrays.toString(ClausePurifier.compactify(true,clause6,model)));

        int[] clause7 = new int[]{16, cAtleast, 1, 0, 1, 0, 0};
        assertNull(ClausePurifier.compactify(true,clause7,model));
        assertEquals("1",model.toString());
        int[] clause8 = new int[]{17, cInterval, 1, 3, 0, 2, 0, 0,0};
        assertNull(ClausePurifier.compactify(true,clause8,model));
        assertEquals("1,2",model.toString());

        try{
            int[] clause9 = new int[]{18, cInterval, 1, 3, 0, -2, 0, 0,0};
            ClausePurifier.compactify(true,clause9,model);
            assertTrue(false);}
        catch(Unsatisfiable unsatisfiable) {
            //System.out.println(unsatisfiable);
        }
    }

        public void testPurifyDisjunction() throws Unsatisfiable {
        System.out.println("purifyDisjunction");
        Model model = new Model(10);
        int[] oClause1 = new int[]{10, cOr,1,2,3};
        int[] pClause1 = ClausePurifier.purifyDisjunction(oClause1, model);
        assertTrue(oClause1 == pClause1);

        int[] oClause2 = new int[]{11, cOr,1,2,3,2,1,2};
        int[] pClause2 = ClausePurifier.purifyDisjunction(oClause2, model);
        assertEquals("[11, 0, 1, 2, 3]", Arrays.toString(pClause2));

        int[] oClause3 = new int[]{12, cOr,1,2,3,2,1,-2};
        int[] pClause3 = ClausePurifier.purifyDisjunction(oClause3, model);
        assertNull(pClause3);

        int[] oClause4 = new int[]{13, cOr,1};
        int[] pClause4 = ClausePurifier.purifyDisjunction(oClause4, model);
        assertNull(pClause4);
        assertEquals("1", model.toString());

        int[] oClause5 = new int[]{14, cOr,2,2,2,2};
        int[] pClause5 = ClausePurifier.purifyDisjunction(oClause5, model);
        assertNull(pClause5);
        assertEquals("1,2", model.toString());

    }

    public void testPurifyAtleast() throws Unsatisfiable {
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

    }

    }