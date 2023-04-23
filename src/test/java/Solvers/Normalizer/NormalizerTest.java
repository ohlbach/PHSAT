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
    public void testNormalizeConjunction() throws Unsatisfiable {
        System.out.println("noramlizeConjunction");
        Normalizer normalizer = new Normalizer(10);
        int[] inputClause = new int[]{1,cAnd,1,2,-3};
        normalizer.normalizeConjunction(inputClause);
        assertEquals("1,2,-3",normalizer.model.toString());
        inputClause = new int[]{2,cAnd,3,4,5};
        try{normalizer.normalizeConjunction(inputClause);}
        catch(Unsatisfiable uns) {
            //System.out.println(uns.toString());
        }}

    public void testNormalizeDisjunction() throws Unsatisfiable{
        System.out.println("noramlizeDisjunction");
        Normalizer normalizer = new Normalizer(10);
        int[] inputClause = {1, cOr, 1, 2, 3,2,1,1};
        IntArrayList clause = normalizer.normalizeDisjunction(inputClause);
        assertEquals("1: 1v2v3", normalizer.toString(clause));

        inputClause = new int[]{2, cOr, 1, 2, 3,-2};
        assertNull(normalizer.normalizeDisjunction(inputClause));

        inputClause = new int[]{3, cOr, 1, 1, 1};
        assertNull(normalizer.normalizeDisjunction(inputClause));
        assertEquals("1",normalizer.model.toString());}

    public void testNormalizeEquivalence() {
    }
}