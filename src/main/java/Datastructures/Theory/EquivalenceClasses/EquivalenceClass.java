package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import Utilities.TriConsumer;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/** This class represents an equivalence class of literals.
 * Equivalence classes can come from the input clauses.<br>
 * They also can be derived, for example from two-literal clauses, for example: p,q and -p,-q.<br>
 * A class can be extended, when new equivalences are derived, for example: p=q=r and r=s yields p=q=r=s.
 *<br>
 * The equivalence class selects one literal as representative.<br>
 * Initially the smallest predicate is selected as representative.<br>
 * The representative remains fixed, even if further equivalences with smaller predicates are added.
 * <br>
 * Although this should not happen, the initial equivalence clauses which come from the input
 * may contain overlapping equivalence classes: p = q and q = r
 * <br>
 * They are either eliminated or cause an Unsatisfiable exception to be thrown.
 */
public class EquivalenceClass {
    /** the representative of the equivalence class. */
    public int representative;

    /** the literals which are equivalent to the representative. */
    public IntArrayList literals;

    /** null or the inference steps which produced the equivalence.
     * The literals-array and the inferenceSteps array are kept synchronized.
     */
    public ArrayList<InferenceStep> inferenceSteps ;

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

    /** clones the equivalence class.
     *
     * @return a clone of the equivalence class.
     */
    protected EquivalenceClass clone() {
        IntArrayList newLiterals = literals.clone();
        ArrayList<InferenceStep> newSteps = null;
        if(inferenceSteps != null) {
            newSteps = new ArrayList<>(inferenceSteps);}
        return new EquivalenceClass(representative,newLiterals,newSteps);}

    /** analyses an input clause of type EQUIV and generates an equivalence class.
     *
     * @param inputClause     an input clause (maybe redundant or contradictory)
     * @param trackReasoning if true then inference steps are added
     * @return               null or a new equivalence class.
     * @throws Unsatisfiable if the class is contradictory (p == -p)
     */
    protected static EquivalenceClass makeEquivalenceClass(int[] inputClause, boolean trackReasoning) throws Unsatisfiable {
        assert(inputClause[1] == Quantifier.EQUIV.ordinal());

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
            inferenceStep  = new InfInputClause(inputClause[0]);}
        IntArrayList literals = new IntArrayList(length-3);
        for(int i = 2; i < length; ++i) {
            int literal1 = sign*inputClause[i];
            if(literal1 != representative) literals.add(literal1);
            if(trackReasoning) inferenceSteps.add(inferenceStep);} // the same inference step for each literal.
        if(literals.size() < 1) return null; // clauses like p = p are redundant.
        return new EquivalenceClass(representative,literals,inferenceSteps);}

    /** checks if the equivalence class contains the literal.
     *
     * @param literal a literal.
     * @return +1 if the literal is with the same sign, -1 if it is with opposite sign, 0 if it is not contained in the class.
     */
    protected int containsLiteral(int literal) {
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
    protected int overlaps(EquivalenceClass eqClass) {
        int sign = containsLiteral(eqClass.representative);
        if(sign != 0) return sign;
        for(int literal: eqClass.literals) {
            sign = containsLiteral(literal);
            if(sign != 0) return sign;}
        return 0;}



    /** adds a new equivalence to the equivalence class.
     * There must be a newly derived equivalence oldLiteral == newLiteral where oldLiteral is already in the class,
     * (with the right sign). In this case the newLiteral is added to the class.
     *
     * @param oldLiteral         a literal which is already contained in the equivalence class.
     * @param newLiteral         a new equivalent literal.
     * @param newInferenceStep   which derived the equivalence oldLiteral = newLiteral.
     * @param observers          function which are called to signal new equivalences.
     * @throws Unsatisfiable     if the newLiteral is already in the class.
     */
    protected void addNewEquivalence(int oldLiteral, int newLiteral, InferenceStep newInferenceStep,
                                  ArrayList<TriConsumer<Integer,Integer,InferenceStep>> observers) throws Unsatisfiable{
        if(representative == newLiteral) return;
        if(representative == -newLiteral)
            throw new UnsatContradictoryEquivalence(null,"EquivalenceClass", oldLiteral,representative,
                getInferenceStep(oldLiteral),newInferenceStep);
        InferenceStep oldInferenceStep = null;
        for(int i = 0; i < literals.size(); ++i) {
            int literal = literals.getInt(i);
            if(literal == newLiteral) return;
            if(literal == -newLiteral)
                throw new UnsatContradictoryEquivalence(null,"EquivalenceClass",oldLiteral,literal,
                        getInferenceStep(oldLiteral),newInferenceStep);
            if(inferenceSteps != null && oldLiteral == literal) oldInferenceStep = inferenceSteps.get(i);}
        if(inferenceSteps != null)
            inferenceSteps.add(new InfAddedLiteral(representative, literals,newLiteral,oldLiteral,newInferenceStep,oldInferenceStep));
        literals.add(newLiteral);
        if(observers != null) {
            for(TriConsumer<Integer,Integer,InferenceStep> observer : observers)
                observer.accept(representative,newLiteral,newInferenceStep);}
    }

    /** finds the inference step that caused representative = literal
     *
     * @param literal a literal in the literals
     * @return null or the inferenceStep that caused representative = literal
     */
    protected InferenceStep getInferenceStep(int literal) {
        if(inferenceSteps == null) return null;
        if(literal == representative) return null;
        for(int i = 0; i < inferenceSteps.size(); ++i) {
            if(literals.getInt(i) == literal) return inferenceSteps.get(i);}
        return null;}

    /** joins two overlapping clauses from the input into one equivalence class
     *
     * @param eqClass the equivalence class to be joined
     * @param sign    +1 or -1
     * @return a new equivalence class
     * @throws Unsatisfiable if the two classes contain contradictory literals.
     */
    protected EquivalenceClass joinOverlappingClasses(EquivalenceClass eqClass, int sign) throws Unsatisfiable {
        EquivalenceClass eqClass1 = this;
        EquivalenceClass eqClass2 = eqClass;
        if(eqClass2.representative < eqClass1.representative) {
            eqClass1 = eqClass; eqClass2 = this;}
        EquivalenceClass joinedClass = eqClass1.clone();
        IntArrayList literals = joinedClass.literals;
        int representative2 = eqClass2.representative;
        int sign1 = joinedClass.containsLiteral(sign*representative2);
        if(sign1 == -1) throw new UnsatJoinedOverlaps(null,"EquivalenceClass", eqClass1,eqClass2);
        if(sign1 == 0) literals.add(sign*representative2);
        for(int literal2 : eqClass2.literals) {
            sign1 = joinedClass.containsLiteral(sign*literal2);
            if(sign1 == -1) throw new UnsatJoinedOverlaps(null,"EquivalenceClass",eqClass1,eqClass2);
            if(sign1 == 0) literals.add(sign*literal2);
        }
        if(eqClass1.inferenceSteps != null) {
            IntArrayList inputClauseIds = Utilities.Utilities.unionIntArrayLists(
                    eqClass1.inferenceSteps.get(0).inputClauseIds(),
                    eqClass2.inferenceSteps.get(0).inputClauseIds());
            InferenceStep joinedStep = new InfInputClause(inputClauseIds);
            ArrayList<InferenceStep> inferenceSteps = new ArrayList<>(literals.size());
            for(int i = 0; i < literals.size(); ++i) inferenceSteps.add(joinedStep);
            joinedClass.inferenceSteps = inferenceSteps;}
        return joinedClass;
    }

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
     * @return              the new joined class.
     */

    protected EquivalenceClass joinEquivalenceClass(EquivalenceClass eqClass, int literal1, int literal2, int sign, InferenceStep inferenceStep,
                                                 ArrayList<TriConsumer<Integer,Integer,InferenceStep>> observers)  {
        EquivalenceClass eqClass1 = this;    int lit1 = literal1;
        EquivalenceClass eqClass2 = eqClass; int lit2 = literal2;
        if(eqClass2.representative < eqClass1.representative) {
            eqClass1 = eqClass; eqClass2 = this;
            lit1 = literal2; lit2 = literal1;}

        EquivalenceClass joinedClass = eqClass1.clone();
        // now we join eqClass2 into a clone of eqClass1
        // we can exploit that the equivalence classes are disjoint.
        joinedClass.literals.add(sign*eqClass2.representative);
        for(int i = 0; i < eqClass2.literals.size(); ++i) {
            joinedClass.literals.add(sign*eqClass2.literals.getInt(i));}

        if(inferenceSteps != null) {
            InferenceStep step1 = eqClass1.getInferenceStep(lit1);
            InferenceStep step2 = eqClass2.getInferenceStep(lit2);
            joinedClass.inferenceSteps.add(new InfConnectedJoining(eqClass1,eqClass2,lit1,lit2,joinedClass, step1,inferenceStep,step2));
            for(int i = 0; i < eqClass2.literals.size(); ++i) {
                joinedClass.inferenceSteps.add(new InfConnectedJoining(eqClass1,eqClass2,lit1,lit2,joinedClass,step1,inferenceStep,step2,
                        eqClass2.inferenceSteps.get(i)));}}

        if(observers != null) {
            for(TriConsumer<Integer,Integer,InferenceStep> observer : observers) {
                for(int i = eqClass1.literals.size(); i < joinedClass.literals.size(); ++i) {
                    observer.accept(joinedClass.representative,joinedClass.literals.getInt(i),
                            ((inferenceSteps == null) ? null : joinedClass.inferenceSteps.get(i)));}}}
        return joinedClass;}

    /** If a literal has turned out to be true/false then all other literals in the class must also be true/false.
     *
     * @param trueLiteral        a literal which has become true.
     * @param sign               +1 if all literals are to become true, -1 if they are to become false.
     * @param trueInferenceStep  which caused the truth of the literal
     * @param model              the global model
     * @param statistics         the statistics object.
     * @throws Unsatisfiable     if a contradiction is encountered.
     */
    protected void applyTrueLiteral(int trueLiteral, int sign, InferenceStep trueInferenceStep,
                                    Model model, EquivalenceStatistics statistics) throws Unsatisfiable {
        int literalInClass = sign*trueLiteral;
        InferenceStep equivInferenceStep = getInferenceStep(literalInClass);
        InferenceStep literalInferenceStep = null;
        if(literalInClass != representative) {
            if(model.status(representative) == 0) ++statistics.derivedTrueLiterals;
            model.add(representative*sign,(trueInferenceStep == null) ? null :
                    new InfApplyTrueLiteral(trueLiteral,representative*sign,trueInferenceStep,equivInferenceStep,null));}
        for(int i = 0; i < literals.size(); ++i) {
            int literal = literals.getInt(i);
            if(literal == literalInClass) continue;
            if(inferenceSteps != null) literalInferenceStep = inferenceSteps.get(i);
            if(model.status(literal) == 0) ++statistics.derivedTrueLiterals;
            model.add(literal*sign,(trueInferenceStep == null) ? null :  // may find a contradiction
                    new InfApplyTrueLiteral(trueLiteral,literal*sign,trueInferenceStep,equivInferenceStep,
                            literalInferenceStep));}}

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
