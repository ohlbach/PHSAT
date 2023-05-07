package Solvers.Normalizer;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Unsatisfiable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import junit.framework.TestCase;

import java.util.function.IntSupplier;

public class NormalizerTest extends TestCase {
    private static final int cOr = Quantifier.OR.ordinal();
    private static final int cAnd = Quantifier.AND.ordinal();
    private static final int cEquiv = Quantifier.EQUIV.ordinal();
    private static final int cAtleast = Quantifier.ATLEAST.ordinal();
    private static final int cAtmost = Quantifier.ATMOST.ordinal();
    private static final int cExactly = Quantifier.EXACTLY.ordinal();
    private static final int cInterval = Quantifier.INTERVAL.ordinal();

    private static boolean monitoring = false;
    public void testNormalizeConjunction() throws Unsatisfiable {
        System.out.println("noramlizeConjunction");
        Normalizer normalizer = new Normalizer(10, monitoring);
        int[] inputClause = new int[]{1,cAnd};
        normalizer.normalizeConjunction(inputClause);
        inputClause = new int[]{2,cAnd,1,2,-3};
        normalizer.normalizeConjunction(inputClause);
        assertEquals("1,2,-3",normalizer.model.toString());
        //System.out.println(normalizer.statistics.toString());
        inputClause = new int[]{3,cAnd,3,4,5};
        try{normalizer.normalizeConjunction(inputClause);}
        catch(Unsatisfiable uns) {
            //System.out.println(uns.toString());
        }}

    public void testNormalizeDisjunction() throws Unsatisfiable{
        System.out.println("noramlizeDisjunction");
        Normalizer normalizer = new Normalizer(10, monitoring);

        int[] inputClause = new int[]{1, cOr, 1, 2, 3,2,1,1};
        IntArrayList clause = normalizer.normalizeDisjunction(inputClause);
        assertEquals("1: 1v2v3", normalizer.toString(clause));

        inputClause = new int[]{2, cOr, 1, 2, 3,-2};
        assertNull(normalizer.normalizeDisjunction(inputClause));

        inputClause = new int[]{3, cOr, 1, 1, 1};
        assertNull(normalizer.normalizeDisjunction(inputClause));
        assertEquals("1",normalizer.model.toString());

        inputClause = new int[]{4, cOr, 3,2, 1};
        assertNull(normalizer.normalizeDisjunction(inputClause));

        inputClause = new int[]{5, cOr, 3,-1, 2};
        assertEquals("5: 3v2", normalizer.toString(normalizer.normalizeDisjunction(inputClause)));

        normalizer.model.addImmediately(2);
        inputClause = new int[]{6, cOr, -3,-2, -1};
        assertNull(normalizer.normalizeDisjunction(inputClause));
        assertEquals("1,2,-3",normalizer.model.toString());

        //System.out.println(normalizer.statistics.toString());
        inputClause = new int[] {6, cOr};
        try{normalizer.normalizeDisjunction(inputClause); assertTrue(false);}
        catch(Unsatisfiable uns) {
            //System.out.println(uns.toString());
        }
    }

    public void testNormalizeEquiv1() throws Unsatisfiable{
        System.out.println("normalizeEquiv 1");
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

    public void testNormalizeEquiv2() throws Unsatisfiable {
        System.out.println("normalizeEquiv 2");
        Normalizer normalizer = new Normalizer(10, monitoring);
        normalizer.model.addImmediately(-3);
        int[] inputClause = {1, cEquiv, 2, 3, -1};
        normalizer.normalizeEquivalence(inputClause);
        assertEquals("1,-2,-3",normalizer.model.toString());
        assertTrue(normalizer.equivalences.isEmpty());
        //System.out.println(normalizer.statistics.toString());
    }

    public void testTransformInputClauses1() {
        System.out.println("TransformInputClauses 1");
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
    public void testTransformInputClauses2() throws Unsatisfiable{
        System.out.println("TransformInputClauses 2");
        Normalizer normalizer = new Normalizer(10, monitoring);
        normalizer.model.addImmediately(2);
        int[] inputClause = {1, cEquiv, 5,4,3};
        normalizer.normalizeEquivalence(inputClause);
        normalizer.joinEquivalences();
        StringBuilder st = new StringBuilder();
        normalizer.toStringEquiv(st,"");
        assertEquals("3=4=5",st.toString());

        inputClause = new int[]{2, cAtleast,2 , 6,5,4,-2,6,4};
        IntArrayList clause = normalizer.transformInputClause(inputClause);
        assertEquals("2: >=2 6^2,3^3", normalizer.toString(clause));

        inputClause = new int[]{3, cAtleast,2 , 5,-2};
        clause = normalizer.transformInputClause(inputClause);
        assertEquals("3: >=2 3", normalizer.toString(clause));

        //System.out.println(normalizer.statistics.toString());
}

    public void testAnalyseMultiplicities() throws Unsatisfiable {
        System.out.println("analyseMultiplicities");
        Normalizer normalizer = new Normalizer(10,monitoring);
        int[] inputClause = {1, cAtleast, 2, 1, 2, 3};
        IntArrayList clause = normalizer.transformInputClause(inputClause);
        assertEquals("1: >=2 1,2,3",normalizer.toString(normalizer.analyseMultiplicities(clause,inputClause)));

        inputClause = new int[]{2, cAtleast, 2, 1, 2, 3,1,2,2,3,1,3};
        clause = normalizer.transformInputClause(inputClause);
        assertEquals("2: >=2 1^2,2^2,3^2",normalizer.toString(normalizer.analyseMultiplicities(clause,inputClause)));

        inputClause = new int[]{3, cAtmost, 3, 1,1,1,2,2};
        clause = normalizer.transformInputClause(inputClause);
        assertEquals("3: <=2 1^2,2^2",normalizer.toString(normalizer.analyseMultiplicities(clause,inputClause)));

        inputClause = new int[]{4, cAtmost, 2, 1,1,1,2,2};
        clause = normalizer.transformInputClause(inputClause);
        clause = normalizer.analyseMultiplicities(clause,inputClause);
        assertEquals("4: <=2 2^2",normalizer.toString(clause));
        assertEquals("-1",normalizer.model.toString());
        assertNull(normalizer.optimizeQuantifier(clause,inputClause));
        //System.out.println(normalizer.statistics.toString());

        inputClause = new int[]{5, cAtleast, 4, 2,2,3,3,4};
        clause = normalizer.transformInputClause(inputClause);
        clause = normalizer.analyseMultiplicities(clause,inputClause);
        assertEquals("5: >=0 4",normalizer.toString(clause));
        assertNull(normalizer.optimizeQuantifier(clause,inputClause));
        assertEquals("-1,2,3",normalizer.model.toString());
        //System.out.println(normalizer.statistics.toString());

    }

    public void testOptimizeQuantifier() throws Unsatisfiable {
        System.out.println("optimizeQuantifier");
        Normalizer normalizer = new Normalizer(10, monitoring);
        int[] inputClause = {1, cAtleast, 1, 1, 2, 3};
        IntArrayList clause = normalizer.transformInputClause(inputClause);
        normalizer.optimizeQuantifier(clause,inputClause);
        assertEquals("1: 1v2v3", normalizer.toString(clause));

        inputClause = new int[] {2, cInterval, 0, 3, 1, 2, 3};
        clause = normalizer.transformInputClause(inputClause);
        assertNull(normalizer.optimizeQuantifier(clause,inputClause));

        inputClause = new int[] {3, cInterval, 0, 2, 1, 2, 3};
        clause = normalizer.transformInputClause(inputClause);
        normalizer.optimizeQuantifier(clause,inputClause);
        assertEquals("3: -1v-2v-3", normalizer.toString(clause));

        inputClause = new int[] {4, cInterval, 0, 2};
        clause = normalizer.transformInputClause(inputClause);
        assertNull(normalizer.optimizeQuantifier(clause,inputClause));

        inputClause = new int[] {5, cAtmost, 2, 1, 2, 3};
        clause = normalizer.transformInputClause(inputClause);
        normalizer.optimizeQuantifier(clause,inputClause);
        assertEquals("5: -1v-2v-3", normalizer.toString(clause));

        inputClause = new int[] {6, cInterval, 1, 2};
        clause = normalizer.transformInputClause(inputClause);
        try{
            normalizer.optimizeQuantifier(clause,inputClause);
            assertTrue(false);}
        catch(Unsatisfiable uns) {
            //System.out.println(uns.toString());
        }}

        public void testAtmost2Atleast()  {
            System.out.println("atmost2Atleast and vice versa");
            Normalizer normalizer = new Normalizer(10, monitoring);
            int[] inputClause = {1, cAtmost, 2, 1, 2, 3};
            IntArrayList atmostClause = normalizer.transformInputClause(inputClause);
            IntArrayList atleastClause = Normalizer.atmost2Atleast(atmostClause);
            assertEquals("1: >=1 -1,-2,-3",normalizer.toString(atleastClause));
            IntArrayList atmostClause1 = Normalizer.atleast2Atmost(atleastClause);
            assertTrue(atmostClause.equals(atmostClause1));}

    public void testExactly2Atleast() {
        System.out.println("exactly2Atleast");
        int[] id = new int[]{0};
        IntSupplier newIds = () -> ++id[0];
        Normalizer normalizer = new Normalizer(10, monitoring);
        int[] inputClause = {1, cExactly, 2, 1, 2, 3, 4, 5};
        IntArrayList exactlyClause = normalizer.transformInputClause(inputClause);
        IntArrayList[] atleastClauses = Normalizer.exactlyToAtleast(exactlyClause,newIds);
        assertEquals("1: >=2 1,2,3,4,5",normalizer.toString(atleastClauses[0]));
        assertEquals("2: >=3 -1,-2,-3,-4,-5",normalizer.toString(atleastClauses[1]));
    }

    public void testInterval2Atleast() {
        System.out.println("interval2Atleast");
        int[] id = new int[]{0};
        IntSupplier newIds = () -> ++id[0];
        Normalizer normalizer = new Normalizer(10, monitoring);
        int[] inputClause = {1, cInterval, 2, 4, 1, 2, 3, 4, 5, 6, 7, 8, 9};
        IntArrayList intervalClause = normalizer.transformInputClause(inputClause);
        IntArrayList[] atleastClauses = Normalizer.intervalToAtleast(intervalClause,newIds);
        assertEquals("1: >=2 1,2,3,4,5,6,7,8,9",normalizer.toString(atleastClauses[0]));
        assertEquals("2: >=5 -1,-2,-3,-4,-5,-6,-7,-8,-9",normalizer.toString(atleastClauses[1]));
    }

    }