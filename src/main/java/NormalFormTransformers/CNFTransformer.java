package NormalFormTransformers;

import Datastructures.Clauses.Connective;
import Utilities.CombinationsIterator;

import java.util.Iterator;
import java.util.function.IntSupplier;

/** Transformation of QU-Sat quantified clauses to conjunctive normalform (CNF).<br>
 * The quantified clauses in InputClauses form are transformed into disjunctions also in InputClauses form. <br>
 * The transformer is an iterator of which the next()-method returns the next disjunction.<br>
 * <br>
 * The iterator needs to be constructed only once. A new clause is submitted to it by the reset-method.
 */
public class CNFTransformer implements Iterator<int[]> {

    private final static int cOr  = Connective.OR.ordinal();
    private final static int cAtleast  = Connective.ATLEAST.ordinal();
    private final static int cAtmost   = Connective.ATMOST.ordinal();
    private final static int cExactly  = Connective.EXACTLY.ordinal();
    private final static int cInterval = Connective.INTERVAL.ordinal();

    /** for generating a new identifier for the clauses. */
    private final IntSupplier nextId;
    private int[] clause;

    private CombinationsIterator iterator1;
    private CombinationsIterator iterator2;
    private CombinationsIterator iterator;
    private int start;
    private int sign1,sign2, sign;

    /** constructs a new transformer
     *
     * @param nextId for generating the next clause identifier.
     */
    public CNFTransformer(IntSupplier nextId) {
        this.nextId = nextId;}

    public void reset(int[] clause) {
        this.clause = clause;
        int length = clause.length;
        switch(Connective.getConnective(clause[1])) {
            case ATLEAST:
                start = 3; sign1 = 1;
                int m = clause[2];
                int n = length - start;
                iterator1 = new CombinationsIterator(n,n-m+1);
                break;
            case ATMOST:
                start = 3; sign1 = -1;
                m = clause[2];
                n = length - start;
                iterator1 = new CombinationsIterator(n,m+1);
                break;
            case EXACTLY:
                start = 3; sign1 = 1;
                m = clause[2];
                n = length - start;
                iterator1 = new CombinationsIterator(n,n-m+1);
                sign2 = -1;
                iterator2 = new CombinationsIterator(n,m+1);
                break;
            case INTERVAL:
                start = 4; sign1 = 1;
                m = clause[2];
                n = length - start;
                iterator1 = new CombinationsIterator(n,n-m+1);
                m = clause[3];
                sign2 = -1;
                iterator2 = new CombinationsIterator(n,m+1);
        }
        iterator = iterator1;
        sign = sign1;
    }

    /** checks if there is another disjunction.
     *
     * @return true if there is another disjunctions.
     */
    @Override
    public boolean hasNext() {
        if(iterator1 != null) {
            if(iterator1.hasNext()) return true;
            else {
                iterator1 = null; iterator = iterator2; sign = sign2;
                if(iterator2 != null) return iterator2.hasNext();}}
        else {if(iterator2 != null) return iterator2.hasNext();}
        return false;}


    /** generates the next disjunction.
     *
     * @return the next disjunction.
     */
    @Override
    public int[] next() {
        int[] pattern = iterator.next();
        int[] disjunction = new int[iterator.n+3];
        disjunction[0] = nextId.getAsInt();
        disjunction[1] = cOr;
        for(int i = 0; i < pattern.length; ++i) {
            disjunction[i+2] = sign * clause[pattern[i]+start];}
        return disjunction;
    }
}
