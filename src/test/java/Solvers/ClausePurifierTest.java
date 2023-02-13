package Solvers;

import Datastructures.Clauses.Connective;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.Model;
import junit.framework.TestCase;

import java.util.Arrays;

public class ClausePurifierTest extends TestCase {
    static int or = Connective.OR.ordinal();
    static int atl = Connective.ATLEAST.ordinal();

    public void testPurifyDisjunction() throws Unsatisfiable {
        System.out.println("purifyDisjunction");
        Model model = new Model(10);
        int[] oClause1 = new int[]{10,or,1,2,3};
        int[] pClause1 = ClausePurifier.purifyDisjunction(oClause1, model);
        assertTrue(oClause1 == pClause1);

        int[] oClause2 = new int[]{11,or,1,2,3,2,1,2};
        int[] pClause2 = ClausePurifier.purifyDisjunction(oClause2, model);
        assertEquals("[11, 0, 1, 2, 3]", Arrays.toString(pClause2));

        int[] oClause3 = new int[]{12,or,1,2,3,2,1,-2};
        int[] pClause3 = ClausePurifier.purifyDisjunction(oClause3, model);
        assertNull(pClause3);

        int[] oClause4 = new int[]{13,or,1};
        int[] pClause4 = ClausePurifier.purifyDisjunction(oClause4, model);
        assertNull(pClause4);
        assertEquals("1", model.toString());

        int[] oClause5 = new int[]{14,or,2,2,2,2};
        int[] pClause5 = ClausePurifier.purifyDisjunction(oClause5, model);
        assertNull(pClause5);
        assertEquals("1,2", model.toString());

    }

    public void testPurifyAtleast() throws Unsatisfiable {
        System.out.println("purifyAtleast");
        Model model = new Model(10);
        int[] oClause1 = new int[]{10, atl, 2, 1, 2, 3, 2};
        int[] pClause1 = ClausePurifier.purifyAtleast(oClause1, model);
        assertTrue(oClause1 == pClause1);

        int[] oClause2 = new int[]{11, atl, 2, 1, 2, 3, -2};
        int[] pClause2 = ClausePurifier.purifyAtleast(oClause2, model);
        assertEquals("[11, 0, 1, 3]", Arrays.toString(pClause2));

        int[] oClause3 = new int[]{12, atl, 5, 1, 2, 3, -2, 2,3,-3,4,-3};
        int[] pClause3 = ClausePurifier.purifyAtleast(oClause3, model);
        assertEquals("[12, 4, 2, 1, 2, 4]", Arrays.toString(pClause3));

        int[] oClause4 = new int[]{13, atl, 3, 1, 2, 3, -2, 2,3,-3,4,-3};
        int[] pClause4 = ClausePurifier.purifyAtleast(oClause4, model);
        assertNull(pClause4);
        assertEquals("",model.toString());

        int[] oClause5 = new int[]{14, atl, 6, 1, 2, 3, -2, 2,3,-3,4,-3};
        int[] pClause5 = ClausePurifier.purifyAtleast(oClause5, model);
        assertNull(pClause5);
        assertEquals("1,2,4",model.toString());

        int[] oClause6 = new int[]{15, atl, 2, 1, 2, 3, 2, 2,3,3,4,3};
        int[] pClause6 = ClausePurifier.purifyAtleast(oClause6, model);
        assertEquals("[15, 4, 2, 1, 2, 2, 3, 4, 3]", Arrays.toString(pClause6));


        try{
            int[] oClause16 = new int[]{16, atl, 2,-4,-5};
            int[] pClause16 = ClausePurifier.purifyAtleast(oClause16, model);
            assertTrue(false);}
        catch(Unsatisfiable unsatisfiable) {
            System.out.println(unsatisfiable.toString());
        }

    }

    }