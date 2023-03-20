package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;

/** This class describes a contradiction where overlapping equivalence clauses have contradictory literals.<br>
 *  Example: p = q = r and q = s = -p. This implies p = -p.
 */
public class UnsatJoinedOverlaps extends Unsatisfiable {

    /** the first class */
    EquivalenceClass eqClass1;

    /** the second class */
    EquivalenceClass eqClass2;

    /** contructs an Unsatifiablity from two overlapping equivalence classes.
     *
     * @param eqClass1 the first class.
     * @param eqClass2 the second class.
     */
    public UnsatJoinedOverlaps(String problemId, String solverId,EquivalenceClass eqClass1, EquivalenceClass eqClass2) {
        super(problemId,solverId);
        this.eqClass1 = eqClass1;
        this.eqClass2 = eqClass2;
        if(eqClass1.inferenceSteps != null) inferenceSteps.addAll(eqClass1.inferenceSteps);
        if(eqClass2.inferenceSteps != null) inferenceSteps.addAll(eqClass2.inferenceSteps);
    }

    @Override
    public String description(Symboltable symboltable) {
        return "Overlapping equivalence classes with complementary literals:\n"+
                eqClass1.toString(symboltable)+"\n"+
                eqClass2.toString(symboltable);}

}
