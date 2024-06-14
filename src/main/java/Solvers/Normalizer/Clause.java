package Solvers.Normalizer;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InfTrueLiteralToClause;
import InferenceSteps.InferenceStep;
import Utilities.BiConsumerWithUnsatisfiable;

import java.util.function.Consumer;

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



    /** removes a literal to prepare for singleton purity in interval clauses.
     *
     * @param literal            the literal to be removed.
     * @param trackReasoning     controls generation of inference steps
     * @param reportTruth        a function for reporting a true literal.
     * @param monitor            null or a monitor
     * @param symboltable        null or a symboltable
     * @return                   -1 if a contradiction is encountered, +1 if the clause can be removed, 0 otherwise.
     */
    public int removeLiteral(int literal, boolean trackReasoning,
                             BiConsumerWithUnsatisfiable<Integer,InferenceStep> reportTruth,
                             Consumer<String> monitor, Symboltable symboltable) throws Unsatisfiable {
        int[] cloned = (trackReasoning || monitor != null) ? simpleClone() : null;
        removeLiteral(literal,0);
        ++version;
        if(trackReasoning) addInferenceStep(new InfTrueLiteralToClause(-literal,null,cloned,this));
        if(monitor != null) {
                monitor.accept("From clause " + toString(cloned, symboltable) +
                        " literal " + Symboltable.toString(literal,symboltable) + " removed => "+
                        toString(symboltable,0));}
        return simplify(trackReasoning,null,reportTruth, monitor,symboltable);}




}
