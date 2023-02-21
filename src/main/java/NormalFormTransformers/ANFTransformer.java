package NormalFormTransformers;

import Datastructures.Clauses.Connective;
import InferenceSteps.InferenceStep;

import java.util.Arrays;
import java.util.function.IntSupplier;

/** Transformation of QU-Sat clauses to Atleast normalform (ANF).
 *  The methods in this class transform atmost-, exactly-, and interval-clauses to atleast-clauses.<br>
 *  The clauses are int-arrays as in InputClauses, and the resulting transformed clauses
 *  are also int-arrays as in InputClauses.<br>
 *  The resulting clauses, however gat a new identifier.
 */
public class ANFTransformer {

    private final static int cAtleast  = Connective.ATLEAST.ordinal();
    private final static int cAtmost   = Connective.ATMOST.ordinal();
    private final static int cExactly  = Connective.EXACTLY.ordinal();
    private final static int cInterval = Connective.INTERVAL.ordinal();

    /** for generating a new identifier for the clauses. */
    private final IntSupplier nextId;

    /** generates a new ANFTransformer
     *
     * @param nextId for generating a new identifier for the clauses.
     */
    public ANFTransformer(IntSupplier nextId) {
        this.nextId = nextId;
    }

    /** transforms an atmost-clause to an atleast-clause.
     * atmost k l1...ln -&gt; atleast n-k -l1...-ln.
     *
     * @param atmostClause the atmost-clause.
     * @return the transformed atleast-clause.
     */
    protected int[] atmost2Atleast(int[] atmostClause) {
        assert(atmostClause[1] == cAtmost);
        int length = atmostClause.length;
        int[] atleastClause = new int[length];
        atleastClause[0] = nextId.getAsInt();
        atleastClause[1] = cAtleast;
        atleastClause[2] = length - atmostClause[2] - 3;
        for(int i = 3; i < length; ++i) atleastClause[i] = -atmostClause[i];
        return atleastClause;}

    /** generates an inference step for atmostClause -&gt; atleastClause.
     *
     * @param atmostClause  the atmost-clause.
     * @param atleastClause the generated atleast-clause.
     * @return the corresponding inference step.
     */
    protected InferenceStep infAtmost2Atleast(int[] atmostClause, int[] atleastClause) {
        return new InfAtmost2Atleast(atmostClause,atleastClause);}

    /** transforms an exactly-clause to two atleast clauses.
     * exactly k l1...ln -&gt; atleast k l1...ln and atleast n-k -l1...ln.
     *
     * @param exactlyClause the exactly-clause.
     * @param atleastClauses an initialized array with two cells, where the transformed clauses can be put into.
     */
    protected void exactly2Atleast(int[] exactlyClause, int[][] atleastClauses) {
        assert (exactlyClause[1] == cExactly);
        int length = exactlyClause.length;
        int[] atleastClause1 = Arrays.copyOf(exactlyClause,length);
        atleastClause1[0] = nextId.getAsInt();
        atleastClause1[1] = cAtleast;

        int[] atleastClause2 = new int[length];
        atleastClause2[0] = nextId.getAsInt();
        atleastClause2[1] = cAtleast;
        atleastClause2[2] = length - exactlyClause[2] - 3;
        for(int i = 3; i < length; ++i) atleastClause2[i] = -exactlyClause[i];
        atleastClauses[0] = atleastClause1;
        atleastClauses[1] = atleastClause2;}

    /** generates n inference step for exactlyClause -&gt; atleastClauses.
     *
     * @param exactlyClause  the exactly-clause.
     * @param atleastClauses the two generated atleast-clauses.
     * @param steps          an initialized array where the inference steps can be put into.
     */
    protected void infExactly2Atleast(int[] exactlyClause, int[][] atleastClauses, InferenceStep[] steps) {
        if(steps == null) return;
        steps[0] = new InfExactly2Atleast(exactlyClause,atleastClauses,0);
        steps[1] = new InfExactly2Atleast(exactlyClause,atleastClauses,1);}

    /** transforms an interval-clause to two atleast-clauses.
     * [min,max] l1...ln -&gt; atleast min l1...ln and atleast n-max -l1...ln.
     *
     * @param intervalClause the interval-clause.
     * @param atleastClauses an initialized array with two cells, where the transformed clauses can be put into.
     */
    protected void interval2Atleast(int[] intervalClause, int[][] atleastClauses) {
        assert (intervalClause[1] == cInterval);
        int length = intervalClause.length;
        int[] atleastClause1 = new int[length-1];
        atleastClause1[0] = nextId.getAsInt();
        atleastClause1[1] = cAtleast;
        atleastClause1[2] = intervalClause[2];
        System.arraycopy(intervalClause, 4, atleastClause1, 3, length - 4);

        int[] atleastClause2 = new int[length-1];
        atleastClause2[0] = nextId.getAsInt();
        atleastClause2[1] = cAtleast;
        atleastClause2[2] = length - intervalClause[3] - 4;
        for(int i = 4; i < length; ++i) atleastClause2[i-1] = -intervalClause[i];
        atleastClauses[0] = atleastClause1;
        atleastClauses[1] = atleastClause2;}

    /** generates n inference step for intervalClause -&gt; atleastClauses.
     *
     * @param intervalClause  the exactly-clause.
     * @param atleastClauses the two generated atleast-clauses.
     * @param steps          an initialized array where the inference steps can be put into.
     */
    protected void infInterval2Atleast(int[] intervalClause, int[][] atleastClauses, InferenceStep[] steps) {
        if(steps == null) return;
        steps[0] = new InfInterval2Atleast(intervalClause,atleastClauses,0);
        steps[1] = new InfInterval2Atleast(intervalClause,atleastClauses,1);}
}
