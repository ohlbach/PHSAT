package InferenceSteps;

import Datastructures.Clause;
import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import java.util.ArrayList;
import java.util.function.Consumer;

import static Utilities.Utilities.isTrue;
import static Utilities.Utilities.modelString;

/** This step documents that the input clause causes some immediate consequences:
 * true literals from AND-clauses, equivalent literals from EQUIV-clauses and transformed clauses from the other types.
 */
public class InfInputClause extends InferenceStep {

    /** one of the input clause transformation type */
    private enum Type {AND,EQUIV,TRANSFORM}

    /** one of the types of the inference step */
    private final Type type;

    /** the original input clause */
    public int[] inputClause;

    /** the first literal of an equivalence */
    public int literal1;

    /** the second literal of an equivalence */
    public int literal2;

    /** the transformed clause as a simple clone. */
    public int[] clause;

    /**
     * Represents an inference step in which the clause is derived from input clauses.
     * <br>
     * The clause is turned into a simpleClone because it may be further simplified later on.
     *
     * @param inputClause the original input clause
     * @param clause either null or the transformed clause.
     * @param reasoner which did the transformation
     */
    public InfInputClause(int[] inputClause, Clause clause, String reasoner) {
        super(reasoner);
        type = Type.TRANSFORM;
        this.inputClause = inputClause;
        if(clause != null) this.clause = clause.simpleClone();}

    /** Represents an inference step in which the clause is derived from input clauses.
        *
        * @param inputClause the original input clause
        * @param literal a literal from an AND-clause.
        * @param reasoner which did the transformation
     */
    public InfInputClause(int[] inputClause, int literal, String reasoner) {
        super(reasoner);
        type = Type.AND;
        this.inputClause = inputClause;
        assert Quantifier.getQuantifier(inputClause[1]) == Quantifier.AND;
        this.literal1 = literal;}

    /** Represents an inference step in which the clause is derived from input clauses.
     *
     * @param inputClause the original input clause
     * @param literal1 a literal from an EQUIV-clause.
     * @param literal2 a literal from an EQUIV-clause.
     * @param reasoner which did the transformation
     */
    public InfInputClause(int[] inputClause, int literal1, int literal2, String reasoner) {
        super(reasoner);
        type = Type.EQUIV;
        this.inputClause = inputClause;
        assert Quantifier.getQuantifier(inputClause[1]) == Quantifier.EQUIV;
        this.literal1 = literal1;
        this.literal2 = literal2;}

    @Override
    public String title() {
        return "Input";}

    @Override
    public String rule() {
        return "Input";}

    /**
     * Verifies the consequences of InputClause to clause.
     *
     * @param monitor      the consumer for printing error messages
     * @param symboltable  the symbol table for mapping predicate names to integers
     * @return true if the verification is successful, false otherwise
     */
    public boolean verify(Consumer<String> monitor, Symboltable symboltable) {
        IntArrayList predicates = InputClauses.predicates(inputClause);
        int models = 1 << predicates.size();
        for(int model = 0; model < models; ++model) {
            if(InputClauses.isTrue(inputClause, model, predicates)) {
                switch(type) {
                    case AND:
                        int position = predicates.indexOf(Math.abs(literal2));
                        if(position == 0) {
                            monitor.accept("Error: Verification failed for extracting literal " +
                                    Symboltable.toString(literal2,symboltable) + " from input clause: " +
                                        InputClauses.toString(0, inputClause, symboltable) +
                                    " literal is not contained in the clause.");
                                return false;}
                        if(!isTrue(model, literal1,predicates)) {
                            monitor.accept("Error: Verification failed for extracting literal " +
                                    Symboltable.toString(literal1,symboltable) + " from input clause: " +
                                    InputClauses.toString(0, inputClause, symboltable) +
                                    " : Literal is not true in the clause's model: " + modelString(model,predicates,symboltable));
                            return false;}
                        return true;
                    case EQUIV:
                        boolean truth1 = isTrue(model, literal1,predicates);
                        boolean truth2 = isTrue(model, literal2,predicates);
                         if(truth1 != truth2) {
                             monitor.accept("Error: Verification failed for equivalence of literals " +
                                     Symboltable.toString(literal1, symboltable) + " and " +
                                     Symboltable.toString(literal2, symboltable) + " in input clause: " +
                                     InputClauses.toString(0, inputClause, symboltable) + "\n  Falsifying model: " +
                                     modelString(model,predicates,symboltable));
                             return false;}
                         return true;
                    case TRANSFORM:
                        int fmodel = model;
                        if(!Clause.isTrue(clause, (literal -> isTrue(fmodel, literal,predicates)))) {
                            monitor.accept("Verification failed for transformation of input clause: " +
                                InputClauses.toString(0,inputClause, symboltable) + " to " +
                                Clause.toString(clause,symboltable) + "\n  Falsifying model: " +
                                    modelString(model,predicates,symboltable));
                    return false;}}}}
        return true;}


    /**
     * Converts the inference step to a string representation.
     *
     * @param symboltable null or the symbol table for mapping predicate names to integers
     * @return the string representation of the inference step.
     */
    @Override
    public String toString(Symboltable symboltable) {
        String inp = "Input clause: "+ InputClauses.toString(0,inputClause,symboltable);
        return switch (type) {
            case AND       -> inp + " => true(" + Symboltable.toString(literal1, symboltable) + ")";
            case EQUIV     -> inp + " => " + Symboltable.toString(literal1, symboltable) + " == " +
                                    Symboltable.toString(literal2, symboltable);
            case TRANSFORM -> inp + " => transformed clause " + Clause.toString(clause, symboltable);
        };
    }


    /**
     * Collects the inference steps culminating in this in the list steps. Double occurrences are to be avoided.
     * Collects the inputClause ids of all clauses causing the current inference.
     *
     * @param steps a list for collecting the inference steps
     * @param ids   a list for collecting the inputClause ids
     * @param reasoners for collecting the reasoners.
     */
    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids, ArrayList<String> reasoners) {
        if(steps.contains(this)) return;
        super.inferenceSteps(steps,ids,reasoners);
        int id = inputClause[0];
        if(!ids.contains(id)) ids.add(id);}
}
