package NormalFormTransformers;

import Datastructures.Clauses.Quantifier;
import Datastructures.Clauses.InputClauses;
import InferenceSteps.InferenceStep;
import Utilities.CombinationsIterator;

import java.util.Iterator;
import java.util.function.IntSupplier;

/** Transformation of QU-Sat quantified clauses to conjunctive normalform (CNF).<br>
 * The quantified clauses in InputClauses form are transformed into disjunctions also in InputClauses form. <br>
 * The transformer is an iterator of which the next()-method returns the next disjunction.<br>
 * <br>
 * The iterator needs to be constructed only once. A new clause is submitted to it by the reset-method.<br>
 * The input clauses should not contain complementary predicates.
 * They may, however, contain multiple occurrences of predicates.
 * These are eliminated from the output clauses.
 * The shortened clauses may subsume longer clauses. This is not checked.
 */
public class CNFTransformer implements Iterator<int[]> {

    private final static int cOr  = Quantifier.OR.ordinal();
    /** for generating a new identifier for the clauses. */
    private final IntSupplier nextId;
    private int[] clause;
    private CombinationsIterator iterator1;
    private CombinationsIterator iterator2;
    private CombinationsIterator iterator;
    private int start;
    private int sign1,sign2, sign;
    private boolean hasDoubles;

    /** constructs a new transformer
     *
     * @param nextId for generating the next clause identifier.
     */
    public CNFTransformer(IntSupplier nextId) {
        this.nextId = nextId;}

    /** submits a new clause to the iterator and resets all internal values.
     *
     * @param clause a clause.
     */
    public void reset(int[] clause) {
        this.clause = clause;
        int length = clause.length;
        Quantifier quantifier = Quantifier.getQuantifier(clause[1]);
        assert(quantifier != null);
        start = quantifier.firstLiteralIndex;
        switch(quantifier) {
            case ATLEAST:
                sign1 = 1;
                int m = clause[2];
                int n = length - start;
                iterator1 = new CombinationsIterator(n,n-m+1);
                break;
            case ATMOST:
                sign1 = -1;
                m = clause[2];
                n = length - start;
                iterator1 = new CombinationsIterator(n,m+1);
                break;
            case EXACTLY:
                sign1 = 1;
                m = clause[2];
                n = length - start;
                iterator1 = new CombinationsIterator(n,n-m+1);
                sign2 = -1;
                iterator2 = new CombinationsIterator(n,m+1);
                break;
            case INTERVAL:
                sign1 = 1;
                m = clause[2];
                n = length - start;
                iterator1 = new CombinationsIterator(n,n-m+1);
                m = clause[3];
                sign2 = -1;
                iterator2 = new CombinationsIterator(n,m+1);}
        hasDoubles = InputClauses.hadDoubles(clause,start);
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
     * Double predicates are removed.
     *
     * @return the next disjunction.
     */
    @Override
    public int[] next() {
        int[] pattern = iterator.next();
        int[] disjunction = new int[iterator.n+3];
        disjunction[0] = nextId.getAsInt();
        disjunction[1] = cOr;
        if(hasDoubles) {
            int j = 1;
            for(int i = 0; i < pattern.length; ++i) {
                int literal = sign * clause[pattern[i]+start];
                boolean found = false;
                for(int k = 2; k <= j; ++k) {
                    if(literal == disjunction[k]) {found = true; break;}}
                if(!found) disjunction[++j] = literal;}
            int[] shortDisjunction = new int[j+1];
            System.arraycopy(disjunction, 0, shortDisjunction, 0, j+1);
            return shortDisjunction;}
        else {
            for(int i = 0; i < pattern.length; ++i) {
                disjunction[i+2] = sign * clause[pattern[i]+start];}
            return disjunction;}
    }

    /** constructs the inference step for the given transformed disjunction.
     *
     * @param disjunction the transformed disjunction.
     * @return the corresponding inference step.
     */
    public InferenceStep getInferenceStep(int[] disjunction) {
        return null;
        //return new InfClauseToCNF(clause,disjunction);
    }
}
