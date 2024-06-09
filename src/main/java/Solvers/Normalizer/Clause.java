package Solvers.Normalizer;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Solvers.Normalizer.NMInferenceSteps.NMInferenceStep;
import Utilities.BiConsumerWithUnsatisfiable;

import java.util.function.Consumer;
import java.util.function.IntPredicate;

/** This class represents clauses to be used in the Normalizer method.
 * Clause is a subclass of LinkedItem. Therefore, it can become part of a (single) doubly linked list.
 * Since the clause class has variables min and max, it can represent all versions of quantified clauses.
 * Nevertheless, the quantifier variable is always set to the most specific Quantifier.
 * <br>
 * The constructor just creates a new clause without any simplifications.
 * Simplifications are be done in the simplify method.
 * Each simplification keeps the clause's identifier, but increments its version number.
 * The simplifications can be tracked by adding NMInferenceStep objects to the inferenceSteps list.
 * The NMInferenceStep objects keep a clone of the clause before the simplification.
 * Therefore, inference steps can be verified by checking if the simplified clause is true in all
 * models of the original clause (see the verify-method of NMInferenceStep).
 * <br>
 * In most concrete examples of clauses, there should not be redundancies like large multiplicities
 * or true or false clauses. Therefore, the algorithms need not be optimized to simplify clauses as fast as possible.
 */
public class Clause extends Datastructures.Clause<Literal> {

    /** An id for a potential monitor */
    private final String monitorId = "Normalizer.clause";


    /** Creates a Clause object from the given input clause.
     * Multiple occurrences of predicates are comprised into one occurrence with corresponding multiplicities.<br>
     * The clause's quantifier is optimized to represent the most specific Quantifier.
     *
     * @param inputClause the input clause.
     * @param trackReasoning if true then inference steps are generated
     */
    public Clause(int[] inputClause, boolean trackReasoning, Symboltable symboltable) {
        super(inputClause, trackReasoning, (literal) -> new Literal(literal,1),symboltable);
        for(Literal literalObject : literals) literalObject.clause = this;}

    /**
     * Creates and returns a clone of the Clause object.
     *
     * @return a new Clause object that is an identical copy of the original Clause.
     * @throws CloneNotSupportedException if the cloning operation is not supported for the Clause object.
     */
    public Clause clone() {
        return (Clause)super.clone();}



    /** removes a literal to prepare for singleton purity in interval clauses.
     *
     * @param literal            the literal to be removed.
     * @param trackReasoning     controls generation of inference steps
     * @param literalRemover     null or a function to indicate that a literal is removed.
     * @param reportTruth        a function for reporting a true literal.
     * @param monitor            null or a monitor
     * @param symboltable        null or a symboltable
     * @return                   -1 if a contradiction is encountered, +1 if the clause can be removed, 0 otherwise.
     */
    public int removeLiteral(int literal, boolean trackReasoning,
                             Consumer<Literal>literalRemover,BiConsumerWithUnsatisfiable<Integer,InferenceStep> reportTruth,
                             Consumer<String> monitor, Symboltable symboltable) throws Unsatisfiable {
        int[] cloned = (trackReasoning || monitor != null) ? simpleClone() : null;
        removeLiteral(literal,false);
        ++version;
        if(trackReasoning) addInferenceStep(new NMInferenceStep("removedLiteral",cloned));
        if(monitor != null) {
                monitor.accept("From clause " + toString(cloned, symboltable) +
                        " literal " + Symboltable.toString(literal,symboltable) + " removed => "+
                        toString(symboltable,0));}
        return simplify(trackReasoning,literalRemover,reportTruth, monitor,symboltable);}

    /** counts the number of true predicates in the clause
     *
     * @param model maps a literal to true/false
     * @return the number of true predicates in the clause.
     */
    public int trueLiterals(IntPredicate model) {
        int trueLiterals = 0;
        for(Literal literalObject : literals) {
            int literal = literalObject.literal;
            if(model.test(literal)) trueLiterals += literalObject.multiplicity;}
        return trueLiterals;}


    /**
     * Checks the expanded size of the clause by summing the multiplicities of the predicates.
     * If the calculated size is different from the stored expanded size, it returns the calculated size.
     * Otherwise, it returns -1.
     *
     * @return the calculated expanded size if different from the stored expanded size, -1 otherwise
     */
    public int checkExpandedSize() {
        int size = 0;
        for(Literal literalObject : literals) {
            size += literalObject.multiplicity;}
        if(size != expandedSize) return size;
        return -1;}


    /**Turns the inference steps into a string.
     *
     * @param symboltable the symbol table used for generating deductions
     * @return a string containing the deductions
     */
    public String deductions(Symboltable symboltable) {
        String[] deductions = new String[inferenceSteps.size()];
        Clause clause = this;
        for(int i = inferenceSteps.size()-1; i >= 0; --i) { // it is necessary to go from back to front.
            InferenceStep step = inferenceSteps.get(i);
            deductions[i] = (step.toString(symboltable));}
        StringBuilder st = new StringBuilder();
        for(String deduction : deductions) st.append(deduction).append("\n");
        return st.toString();}

    /** turns the clause into a string.
     *
     * @param symboltable null or a symboltable.
     * @param size 0 or the length of the clause number string.
     * @return a string representation of the clause.
     */
    @Override
    public String toString(Symboltable symboltable, int size) {
        StringBuilder st = new StringBuilder();
        String id = Integer.toString(this.id);
        if(version != 0) id += "."+version;
        st.append((size == 0) ? id : String.format("%"+size+"s", id)).append(": ");
        switch (quantifier) {
            case OR: break;
            case INTERVAL:st.append("[").append(min).append(",").append(max).append("] "); break;
            case ATLEAST: st.append(quantifier.abbreviation).append(min).append(" ");break;
            case ATMOST:  st.append(quantifier.abbreviation).append(max).append(" ");break;
            case EXACTLY: st.append(quantifier.abbreviation).append(min).append(" ");break;
            case AND:     st.append(quantifier.abbreviation).append(" ");break;
        }
        for(int i = 0; i < literals.size(); ++i) {
            Literal literalObject = literals.get(i) ;
            st.append(literalObject.toString(symboltable,0));
            if(i < literals.size()-1) st.append(",");}
        return st.toString();}

}
