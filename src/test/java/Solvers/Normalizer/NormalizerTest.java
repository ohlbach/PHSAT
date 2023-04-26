package Solvers.Normalizer;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Unsatisfiable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import junit.framework.TestCase;

public class NormalizerTest extends TestCase {
    private static final int cOr = Quantifier.OR.ordinal();
    private static final int cAnd = Quantifier.AND.ordinal();
    private static final int cEquiv = Quantifier.EQUIV.ordinal();
    private static final int cAtleast = Quantifier.ATLEAST.ordinal();
    private static final int cAtmost = Quantifier.ATMOST.ordinal();
    private static final int cExactly = Quantifier.EXACTLY.ordinal();
    private static final int cInterval = Quantifier.INTERVAL.ordinal();

    private static boolean monitoring = true;
    public void testNormalizeConjunction() throws Unsatisfiable {
        System.out.println("noramlizeConjunction");
        Normalizer normalizer = new Normalizer(10, monitoring);
        int[] inputClause = new int[]{1,cAnd};
        normalizer.normalizeConjunction(inputClause);
        inputClause = new int[]{2,cAnd,1,2,-3};
        normalizer.normalizeConjunction(inputClause);
        assertEquals("1,2,-3",normalizer.model.toString());
        inputClause = new int[]{3,cAnd,3,4,5};
        try{normalizer.normalizeConjunction(inputClause);}
        catch(Unsatisfiable uns) {
            //System.out.println(uns.toString());
        }}

    public void testNormalizeDisjunction() throws Unsatisfiable{
        System.out.println("noramlizeDisjunction");
        Normalizer normalizer = new Normalizer(10, monitoring);
        int[] inputClause = {0, cOr};
        IntArrayList clause = normalizer.normalizeDisjunction(inputClause);
        assertNull(clause);
        inputClause = new int[]{1, cOr, 1, 2, 3,2,1,1};
        clause = normalizer.normalizeDisjunction(inputClause);
        assertEquals("1: 1v2v3", normalizer.toString(clause));

        inputClause = new int[]{2, cOr, 1, 2, 3,-2};
        assertNull(normalizer.normalizeDisjunction(inputClause));

        inputClause = new int[]{3, cOr, 1, 1, 1};
        assertNull(normalizer.normalizeDisjunction(inputClause));
        assertEquals("1",normalizer.model.toString());}

    public void testNormalizeEquiv() throws Unsatisfiable{
        Normalizer normalizer = new Normalizer(10, monitoring);
        int[] inputClause = {1,cEquiv,2,3,-1};
        normalizer.normalizeEquivalence(inputClause);
        StringBuilder st = new StringBuilder();
        normalizer.toStringEquiv(st,"");
        assertEquals("2=3=-1",st.toString());
        inputClause = new int[]{2,cEquiv,3,4,5};
        normalizer.normalizeEquivalence(inputClause);
        inputClause = new int[]{3,cEquiv,5,-6,4};
        normalizer.normalizeEquivalence(inputClause);
        normalizer.joinEquivalences();
        st = new StringBuilder();
        normalizer.toStringEquiv(st,"");
        assertEquals("1=-3=-2=-4=-5=6",st.toString());
        inputClause = new int[]{4,cEquiv,3,6};
        try{
            normalizer.normalizeEquivalence(inputClause);
            normalizer.joinEquivalences();
            assertTrue(false);}
        catch(Unsatisfiable uns) {
            //System.out.println(uns.toString());
    }}

public void testTransformInputClauses() {
    System.out.println("TransformInputClauses");
    Normalizer normalizer = new Normalizer(10,monitoring);
    int[] inputClause = {1, cAtleast, 2, 1, 2, 3};
    IntArrayList clause = normalizer.transformInputClause(inputClause);
    assertEquals("1: >=2 1,2,3",normalizer.toString(clause));

    inputClause = new int[]{2, cOr, 1, -2, 3};
    clause = normalizer.transformInputClause(inputClause);
    assertEquals("2: 1v-2v3",normalizer.toString(clause));

    inputClause = new int[]{3, cAtleast, 2, 1, -2, 3};
    clause = normalizer.transformInputClause(inputClause);
    assertEquals("3: >=2 1,-2,3",normalizer.toString(clause));

    inputClause = new int[]{4, cExactly, 2, 1, -2, 3};
    clause = normalizer.transformInputClause(inputClause);
    assertEquals("4: =2 1,-2,3",normalizer.toString(clause));

    inputClause = new int[]{5, cInterval, 2,3, 1, -2, 3};
    clause = normalizer.transformInputClause(inputClause);
    assertEquals("5: [2,3] 1,-2,3",normalizer.toString(clause));
    assertEquals(3,Normalizer.size(clause));
    assertFalse(Normalizer.hasMultiplicities(clause));
}
    public void testAnalyseMultiplicities() throws Unsatisfiable {
        System.out.println("analyseMultiplicities");
        Normalizer normalizer = new Normalizer(10,monitoring);
        int[] inputClause = {1, cAtleast, 2, 1, 2, 3};
        IntArrayList clause = normalizer.transformInputClause(inputClause);
        assertEquals("1: >=2 1,2,3",normalizer.toString(normalizer.analyseMultiplicities(clause,inputClause)));

    }
    public void testNormalizeAtleast() throws Unsatisfiable {
        System.out.println("normalizeAtleast");
        /*
        Normalizer normalizer = new Normalizer(10);
        int[] inputClause = {1, cAtleast, 2, 1, 2, 3};
        IntArrayList clause = normalizer.normalizeAtleast(inputClause);
        assertEquals("1: >=2 1,2,3", normalizer.toString(clause));

        inputClause = new int[]{2, cAtleast, 1, 1, 2, 3};
        clause = normalizer.normalizeAtleast(inputClause);
        assertEquals("2: 1v2v3", normalizer.toString(clause));

        inputClause = new int[]{3, cAtleast, 2, 1, 2, 3, -2};
        clause = normalizer.normalizeAtleast(inputClause);
        assertEquals("3: 1v3", normalizer.toString(clause));

        inputClause = new int[]{4, cAtleast, 2, 1, 2, 3, -2, -1};
        assertNull(normalizer.normalizeAtleast(inputClause));

        inputClause = new int[]{5, cAtleast, 3, 1, 2, 3, -2, -1, 3};
        assertNull(normalizer.normalizeAtleast(inputClause));
        assertEquals("3", normalizer.model.toString());


        inputClause = new int[]{6, cAtleast, 4, 1, 1, 2, 2, 2, 2, 2, 2};
        assertNull(normalizer.normalizeAtleast(inputClause));
        assertEquals("1,2,3", normalizer.model.toString());

        inputClause = new int[]{7, cAtleast, 3, 5,6,6};
        assertNull(normalizer.normalizeAtleast(inputClause));
        assertEquals("1,2,3,5,6", normalizer.model.toString());

        inputClause = new int[]{8, cAtleast, 5, 7,7,8,8,9,10};
        clause = normalizer.normalizeAtleast(inputClause);
        assertEquals("8: 9v10", normalizer.toString(clause));
        assertEquals("1,2,3,5,6,7,8",normalizer.model.toString());

        inputClause = new int[]{9, cAtleast, 4,4,4,5,5,6,6};
        clause = normalizer.normalizeAtleast(inputClause);
        assertEquals("9: >=2 4,5,6", normalizer.toString(clause));

        try {
            inputClause = new int[]{10, cAtleast, 4, 1, 2, 3};
            assertNull(normalizer.normalizeAtleast(inputClause));
            assertEquals(1, normalizer.model.size());
        }
        catch(Unsatisfiable uns) {
            System.out.println(uns.toString());
        }
*/
    }

    public void testNormalizeAtmost() throws Unsatisfiable{
        System.out.println("normalizeAtmost");
        /*
        Normalizer normalizer = new Normalizer(10);
        int[] inputClause = {1, cAtmost, 2, 1, 2, 3, 4};
        IntArrayList clause = normalizer.normalizeAtmost(inputClause);
        assertEquals("1: <=2 1,2,3,4", normalizer.toString(clause));

        inputClause = new int[]{2, cAtmost, 2, 1, 1,2,2};
        clause = normalizer.normalizeAtmost(inputClause);
        assertEquals("2: -1v-2", normalizer.toString(clause));

        inputClause = new int[]{3, cAtmost, 2, 1, 1,1, 2,2};
        assertNull(normalizer.normalizeAtmost(inputClause));
        assertEquals("-1,2", normalizer.model.toString());

        inputClause = new int[]{4, cAtmost, 2, 3,3,3,4,4,4,5};
        assertNull(normalizer.normalizeAtmost(inputClause));
        assertEquals("-1,2,-3,-4,5", normalizer.model.toString());
*/
    }
}