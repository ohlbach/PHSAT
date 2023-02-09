package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Clauses.Connective;
import Datastructures.Results.UnsatInputClause;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import Utilities.Utilities;
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
        this.inferenceSteps = inferenceSteps;}

    /** clones the equivalence class
     *
     * @return a clone of the equivalence class.
     */
    public EquivalenceClass clone() {
        IntArrayList newLiterals = literals.clone();
        ArrayList<InferenceStep> newSteps = null;
        if(inferenceSteps != null) {
            newSteps = new ArrayList<>();
            newSteps.addAll(inferenceSteps);}
        return new EquivalenceClass(representative,newLiterals,newSteps);}

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
        if(!model.isEmpty() && isAlreadyTrueInModel(inputClause,model,trackReasoning)) return null; // new model entries may be generated

        // the smallest literal becomes the representative.
        int length = inputClause.length;
        int representative = inputClause[2];
        for(int i = 3; i < length; ++i) {
            if(Math.abs(inputClause[i]) < Math.abs(representative)) {
                representative = inputClause[i];}}
        int sign = (representative < 0) ? -1 : +1;
        representative *= sign;

        // check for double literals and complementary literals
        ArrayList<InferenceStep> inferenceSteps = null;
        InferenceStep inferenceStep = null;
        if(trackReasoning) {
            inferenceSteps = new ArrayList<>(length-3);
            inferenceStep  = new InfInputClause(inputClause[1]);}
        IntArrayList literals = new IntArrayList(length-3);
        for(int i = 3; i < length; ++i) {
            int literal1 = inputClause[i];
            if(literal1 == sign*representative) continue;
            boolean found = false;
            for(int j = 3; j < length; ++j) {
                int literal2 = inputClause[j];
                if(literal1 == literal2) {found = true; break;}
                if(literal1 == -literal2) {throw new UnsatInputClause(inputClause);}}
            if(found) continue; // double literals can be ignored
            literals = new IntArrayList(length-3);
            literals.add(sign*literal1);
            if(trackReasoning) inferenceSteps.add(inferenceStep);} // the same inference step for each literal.
        if(literals.size() < 1) return null; // clauses like p = p are redundant.
        return new EquivalenceClass(representative,literals,inferenceSteps);}

    /** each literal in an equivalence which is true/false in the model causes the equivalent literals to become true/false.
     * It is very unlikely that this happens. Just to be on the safe side.
     *
     * @param inputClause     an equivalence clause
     * @param model           the model
     * @param trackReasoning  true if the inferences are to be tracked
     * @return                true if the equivalent literals all became true/false and the clause can be ignored.
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
                    if(model.status(newTrueLiteral) == status) continue; // literal is already true/false
                    model.add(status*newTrueLiteral, trackReasoning ? // may become unsatisfiable.
                            new InfEquivalentTrueLiterals(inputClause[0],status*oldTrueLiteral,status*newTrueLiteral,
                                        model.getInferenceStep(oldTrueLiteral)) : null);}
                return true;}} // clause can be ignored from now on.
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
     * There must be a newly derived equivalence oldLiteral == newLiteral where oldLiteral is already in the class,
     * (with the right sign). In this case the newLiteral is added to the class.
     *
     * @param oldLiteral         a literal which is already contained in the equivalence class.
     * @param newLiteral         a new equivalent literal.
     * @param newInferenceStep   which derived the equivalence oldLiteral = newLiteral.
     * @throws Unsatisfiable     if the newLiteral is already in the class.
     */
    public void addNewEquivalence(int oldLiteral, int newLiteral, InferenceStep newInferenceStep,
                                  ArrayList<TriConsumer<Integer,Integer,InferenceStep>> observers) throws Unsatisfiable{
        if(representative == newLiteral) return;
        if(representative == -newLiteral)
            throw new UnsatContradictoryEquivalence(oldLiteral,representative,
                getInferenceStep(oldLiteral),newInferenceStep);
        InferenceStep oldInferenceStep = null;
        for(int i = 0; i < literals.size(); ++i) {
            int literal = literals.getInt(i);
            if(literal == newLiteral) return;
            if(literal == -newLiteral)
                throw new UnsatContradictoryEquivalence(oldLiteral,literal,
                        getInferenceStep(oldLiteral),newInferenceStep);
            if(inferenceSteps != null && oldLiteral == literal) oldInferenceStep = inferenceSteps.get(i);}
        literals.add(newLiteral);
        if(inferenceSteps != null)
            inferenceSteps.add(new InfAddedLiteral(representative, literals,newLiteral,oldLiteral,newInferenceStep,oldInferenceStep));
        for(TriConsumer<Integer,Integer,InferenceStep> observer : observers)
            observer.accept(representative,newLiteral,newInferenceStep);
    }

    /** finds the inference step that caused representative = literal
     *
     * @param literal a literal in the literals
     * @return null or the inferenceStep that caused representative = literal
     */
    public InferenceStep getInferenceStep(int literal) {
        if(inferenceSteps == null) return null;
        if(literal == representative) return null;
        for(int i = 0; i < inferenceSteps.size(); ++i) {
            if(literals.getInt(i) == literal) return inferenceSteps.get(i);}
        return null;}

    /** joins the given equivalence class into 'this'
     * Example: this = p,q,r and eqClass = a,b,c <br>
     * Because r = c, (see inference step) the resulting class is p,q,r,a,b,c
     * We choose as surviving class the class with the smallest representative.
     *
     * @param eqClass       the class to be joined.
     * @param literal1      a literal in this.
     * @param literal2      a literal in eqClass.
     * @param sign          +1 or -1, the sign for the second class.
     * @param inferenceStep which caused the equivalence of literal1 == literal2.
     * @param observers     for noticing the new equivalences.
     * @return              the surviving class.
     * @throws Unsatisfiable if the resulting class contains complementary literals.
     */

    public EquivalenceClass joinEquivalenceClass(EquivalenceClass eqClass, int literal1, int literal2, int sign, InferenceStep inferenceStep,
                                                 ArrayList<TriConsumer<Integer,Integer,InferenceStep>> observers)  {
        EquivalenceClass eqClass1 = this;    int lit1 = literal1;
        EquivalenceClass eqClass2 = eqClass; int lit2 = literal2;
        if(eqClass2.representative < eqClass1.representative) {
            eqClass1 = eqClass; eqClass2 = this;
            lit1 = literal2; lit2 = literal1;}

        EquivalenceClass joinedClass = eqClass1.clone();
        // now we join eqClass2 into eqClass1
        InferenceStep step1 = null;
        InferenceStep step2 = null;
        if(inferenceSteps != null) {step1 = eqClass1.getInferenceStep(lit1); step2 = eqClass2.getInferenceStep(lit2);}
        joinedClass.literals.add(sign*eqClass2.representative);
        if(inferenceSteps != null) {
            joinedClass.inferenceSteps.add(new InfEquivalenceJoining(step1,inferenceStep,step2));}
        IntArrayList literals1 = joinedClass.literals;
        for(int i = 0; i < eqClass2.literals.size(); ++i) {
            literals1.add(sign*eqClass2.literals.getInt(i));
            if(inferenceSteps != null) {
                joinedClass.inferenceSteps.add(new InfEquivalenceJoining(step1,inferenceStep,step2,
                        eqClass2.inferenceSteps.get(i)));}}
        if(observers != null) {
            for(TriConsumer<Integer,Integer,InferenceStep> observer : observers) {
                for(int i = eqClass1.literals.size(); i < joinedClass.literals.size(); ++i) {
                    observer.accept(joinedClass.representative,joinedClass.literals.getInt(i),
                            ((inferenceSteps == null) ? null : joinedClass.inferenceSteps.get(i)));}}}
        return eqClass1;}

    /** If a literal has turned out to be true/false then all other literals in the class must also be true/false.
     *
     * @param trueLiteral        a literal which has become true.
     * @param sign               +1 if all literals are to become true, -1 if they are to become false.
     * @param trueInferenceStep  which caused the truth of the literal
     * @param model              the global model
     * @throws Unsatisfiable     if a contradiction is encountered.
     */
    protected void applyTrueLiteral(int trueLiteral, int sign, InferenceStep trueInferenceStep, Model model) throws Unsatisfiable {
        int literalInClass = sign*trueLiteral;
        InferenceStep equivInferenceStep = getInferenceStep(literalInClass);
        InferenceStep literalInferenceStep = null;
        if(literalInClass != representative) {
            model.add(representative*sign,(trueInferenceStep == null) ? null :
                    new InfApplyTrueLiteral(trueLiteral,representative*sign,trueInferenceStep,equivInferenceStep,null));}
        for(int i = 0; i < literals.size(); ++i) {
            int literal = literals.getInt(i);
            if(literal == literalInClass) continue;
            if(inferenceSteps != null) literalInferenceStep = inferenceSteps.get(i);
            model.add(literal*sign,(trueInferenceStep == null) ? null :
                    new InfApplyTrueLiteral(trueLiteral,literal*sign,trueInferenceStep,equivInferenceStep,literalInferenceStep));}}

    /** returns the equivalence class as a string 'representative = literal1 = ...'
     *  If the symboltable is provided, it will be used.
     *
     * @param symboltable null or a symboltable.
     * @return the equivalence class as a string 'representative = literal1 = ...'
     */
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append(Symboltable.toString(representative,symboltable));
        for(int literal : literals) st.append(" = ").append(Symboltable.toString(literal,symboltable));
        return st.toString();}

    /** returns the equivalence class as a string 'representative = literal1 = ...'
     * without a symboltable.
     *
     * @return the equivalence class as a string 'representative = literal1 = ...'
     */
    public String toString() {
        return toString(null);}

}
