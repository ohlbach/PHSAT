package Solvers.Normalizer;

import Datastructures.Clauses.InputClauses;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;

/** This class represents two kinds of unsatisfiabilities in equivalences.
 * <br>
 * 1. contradictory literals like p == -p<br>
 * 2. equivalent literals with different truth values in the model: p == q and true(p) and false(q).
 * */
public class UnsatEquivalence extends Unsatisfiable {

    /** the original equivalence */
    private final Equivalence equivalence;

    /** two equivalent literals with different truth values */
    int literal1,literal2;

    /** the corresponding inference steps for the truth values of the literals */
    InferenceStep step1,step2;

    /** An Unsatisfiability caused by complementary equivalent literals like p == -p
     *
     * @param equivalence the original equivalence.
     */
    public UnsatEquivalence(Equivalence equivalence) {
        super(null,null);
        this.equivalence = equivalence;}


    /**
     * Constructs a new UnsatEquivalence object with the given parameters.
     *
     * @param problemId   the problem where the unsatisfiability was discovered
     * @param solverId    the solver which discovered the unsatisfiability
     * @param equivalence the original equivalence
     * @param literal1    the first equivalent literal which is supposed to be true
     * @param literal2    the second equivalent literal which is supposed to be false.
     * @param step1       the corresponding inference step for the truth value of the first literal
     * @param step2       the corresponding inference step for the truth value of the second literal
     */
    public UnsatEquivalence(String problemId, String solverId, Equivalence equivalence, int literal1, int literal2,
                            InferenceStep step1, InferenceStep step2) {
        super(problemId,solverId);
        this.equivalence = equivalence;
        this.literal1 = literal1;
        this.literal2 = literal2;
        this.step1    = step1;
        this.step2    = step2;}


    /** generates a description of the unsatisfiability
     *
     * @param symboltable the symbol table used for predicate name mapping
     * @return a description of the unsatisfiability
     */
    public String description(Symboltable symboltable) {
        if(literal1 == 0)
            return "Input Clause: " + InputClauses.toString(0, equivalence.inputClause,symboltable) +
                    " has contradictory literals.";

        return "Input Clause: " + InputClauses.toString(0, equivalence.inputClause,symboltable) + ": " +
                ": has literals with different truth values: true(" +
                Symboltable.toString(literal1,symboltable) + ") and false(" + Symboltable.toString(literal2,symboltable)+")";}
}
