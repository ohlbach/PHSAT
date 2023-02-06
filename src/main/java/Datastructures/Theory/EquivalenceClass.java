package Datastructures.Theory;

import Datastructures.Clauses.Connective;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import Utilities.TriConsumer;

import java.util.ArrayList;

/** This class represents an equivalence class of literals.
 * Initially the smallest predicate is selected as representative.
 * This may change if further equivalent literals are added.
 */
public class EquivalenceClass {
    /** the representative of the equivalence class. */
    public int representative;

    /** the literals which are equivalent to the representative. */
    public IntArrayList literals;

    /** null or the inference steps which produced the equivalence.
     * The literals-array and the inferenceSteps array are kept synchronized.
     */
    public ArrayList<InferenceStep> inferenceSteps = null;

    /** creates an equivalence class.
     * A new equivalence class is not directly created here from an inputClause because it must be analysed first and
     * may turn out to be superfluous.
     *
     * @param representative the representative of the class.
     * @param literals       the list of equivalent literals.
     * @param inferenceSteps null or the list of inference steps for the equivalence class.
     */
    public EquivalenceClass(int representative, IntArrayList literals, ArrayList<InferenceStep> inferenceSteps) {
        this.representative  = representative;
        this.literals        = literals;
        this. inferenceSteps = inferenceSteps;}

    /** analyses an input clause of type EQUIV and generates an equivalence class.
     *
     * @param inputClause     an input clause (maybe redundant or contradictory)
     * @param model           the global model
     * @param trackReasoning if true then inference steps are added
     * @return               null or a new equivalence class.
     * @throws Unsatisfiable if the class is contradictory (p == -p)
     */
    public static EquivalenceClass makeEquivalenceClass(int[] inputClause, Model model, boolean trackReasoning) throws Unsatisfiable {
        assert(inputClause[1] == Connective.EQUIV.ordinal());
        if(!model.isEmpty() && isAlreadyTrueInModel(inputClause,model,trackReasoning)) return null;

        // the smallest literal becomes the representative.
        int length = inputClause.length;
        int representative = inputClause[2];
        for(int i = 3; i < length; ++i) {
            if(Math.abs(inputClause[i]) < Math.abs(representative)) {
                representative = inputClause[i];}}
        int sign = (representative < 0) ? -1 : +1;
        representative *= sign;

        ArrayList<InferenceStep> inferenceSteps = trackReasoning ? new ArrayList<>(length-3) : null;
        IntArrayList literals = new IntArrayList(length-3);
        for(int i = 3; i < length; ++i) {
            int literal1 = inputClause[i];
            if(literal1 == representative) continue;
            boolean found = false;
            for(int j = 3; j < length; ++j) {
                int literal2 = inputClause[j];
                if(literal1 == literal2) {found = true; break;}
                if(literal1 == -literal2) {throw new UnsatContradictoryEquivalenceClass(inputClause);}}
            if(found) continue;
            literals = new IntArrayList(length-3);
            literals.add(sign*literal1);
            if(trackReasoning) inferenceSteps.add(new InfInputClause(inputClause[1]));}
        if(literals.size() < 1) return null; // clauses like p = p are redundant.
        return new EquivalenceClass(representative,literals,inferenceSteps);}

    /** each literal in an equivalence which is true/false in the model causes the equivalent literals to become true/false.
     * It is very unlikely that this happens. Just to be on the safe side.
     *
     * @param inputClause     an equivalence clause
     * @param model           the model
     * @param trackReasoning  true if the inferences are to be tracked
     * @return                true if the equivalent literals all became true/false
     * @throws Unsatisfiable  if a contradiction was discovered.
     */
    private static boolean isAlreadyTrueInModel(int[] inputClause, Model model, boolean trackReasoning) throws Unsatisfiable {
        int length = inputClause.length;
        for(int i = 2; i < length; ++i) {
            int oldTrueLiteral = inputClause[i];
            int status = model.status(oldTrueLiteral);
            if(status != 0) {                     // oldTrueLiteral is true/false
                for(int j = 2; j < length; ++j) {
                    if(i == j) continue;
                    int newTrueLiteral = inputClause[j];
                    if(model.status(newTrueLiteral) == status) continue;
                    model.add(status*newTrueLiteral, trackReasoning ? // may become unsatisfiable.
                            new InfEquivalentTrueLiterals(inputClause[0],status*oldTrueLiteral,status*newTrueLiteral,
                                        model.getInferenceStep(oldTrueLiteral)) : null);}
                return true;}}
        return false;}

    /** checks if the equivalence class contains the literal.
     *
     * @param literal a literal.
     * @return +1 if the literal is with the same sign, -1 if it is with opposite sign, 0 if it is not contained in the class.
     */
    public int containsLiteral(int literal) {
        if(literal == representative)  return +1;
        if(literal == -representative) return -1;
        for(int literal1 : literals) {
            if(literal == literal1)  return +1;
            if(literal == -literal1) return -1;}
        return 0;}

    /** checks if the other equivalence class overlaps with this class.
     *
     * @param eqClass another equivalence class
     * @return +1 if there is a positive overlap, -1 if there is a negative overlap, 0 if there is no overlap.
     */
    public int overlaps(EquivalenceClass eqClass) {
        if(representative == eqClass.representative) return +1;
        if(representative == -eqClass.representative) return -1;
        for(int literal: eqClass.literals) {
            int sign = containsLiteral(literal);
            if(sign != 0) return sign;}
        return 0;}



    /** adds a new equivalence to the equivalence class.
     * There must be a newly derived equivalence literal1 == literal2 where literal2 is already in the class,
     * (with the right sign). In this case the literal1 is added to the class.
     *
     * @param literal1         a literal which is equivalent to one of the literals in the class
     * @param inferenceStep   which derived the equivalence
     * @throws Unsatisfiable if the negated literal is already in the class.
     */
    public void addNewEquivalence(int literal1, int literal2, InferenceStep inferenceStep,
                                  ArrayList<TriConsumer<Integer,Integer,InferenceStep>> observers) throws Unsatisfiable{
        if(literal1 == representative) return;
        if(literal1 == -representative) throw new UnsatContradictoryEquivalence();
        InferenceStep oldInferenceStep = null;
        for(int i = 0; i < literals.size(); ++i) {
            int literal3 = literals.getInt(i);
            if(literal3 == literal1) return;
            if(literal3 == -literal1) throw new UnsatContradictoryEquivalence();
            if(inferenceSteps != null && literal2 == literal3) oldInferenceStep = inferenceSteps.get(i);}
        literals.add(literal1);
        if(inferenceSteps != null)
            inferenceSteps.add(new InfJoinedEquivalence(literal1,literal2,inferenceStep,oldInferenceStep));}

    public EquivalenceClass joinEquivalenceClass(EquivalenceClass eqClass, int sign,
                                                 ArrayList<TriConsumer<Integer,Integer,InferenceStep>> observers) throws Unsatisfiable {
        EquivalenceClass eqClass1 = this;
        EquivalenceClass eqClass2 = eqClass;
        if(eqClass2.representative < eqClass1.representative) {eqClass1 = eqClass; eqClass2 = this;}
        // now we join eqClass2 into eqClass1
        eqClass1.addLiteral(sign*eqClass2.representative);
        for(int i = 0; i < eqClass2.literals.size(); ++i) {
            eqClass1.addLiteral(sign*eqClass2.literals.getInt(i));}
        return eqClass1;}

    protected void addTrueLiteral(int literal, InferenceStep inferenceStep,ArrayList<TriConsumer<Integer,Integer,InferenceStep>> observers) {
        if(literal == representative) return ;
        if(literal == -representative) throw new UnsatContradictoryEquivalence();
        for(int i = 0; i < literals.size(); ++i) {
            int literal1 = literals.getInt(i);
            if(literal1 == literal) return;
            if(literal1 == -literal1) throw new UnsatContradictoryEquivalence();
            if(inferenceSteps != null ) oldInferenceStep = inferenceSteps.get(i);}
    }

    public String toString(Symboltable symboltable) {
        return "";
    }

    public String toString() {
        return toString(null);}
}
