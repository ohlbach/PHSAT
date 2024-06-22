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

    private enum Type {AND,EQUIV,TRANSFORM}

    /** one of the types of the inference step */
    private Type type;

    /** the rule name */
    public static String title = "Input";

    /** the rule itself */
    public static String rule = "Input";

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
     */
    public InfInputClause(int[] inputClause, Clause clause) {
        type = Type.TRANSFORM;
        this.inputClause = inputClause;
        if(clause != null) this.clause = clause.simpleClone();}

    public InfInputClause(int[] inputClause, int literal1) {
        type = Type.AND;
        this.inputClause = inputClause;
        assert Quantifier.getQuantifier(inputClause[1]) == Quantifier.AND;
        this.literal1 = literal1;}

    public InfInputClause(int[] inputClause, int literal1, int literal2) {
        type = Type.EQUIV;
        this.inputClause = inputClause;
        assert Quantifier.getQuantifier(inputClause[1]) == Quantifier.EQUIV;
        this.literal1 = literal1;
        this.literal2 = literal2;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

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
        switch(type) {
            case AND: return inp + " => true(" + Symboltable.toString(literal1,symboltable)+")";
            case EQUIV: return inp + " => " + Symboltable.toString(literal1,symboltable) + " == " +
                    Symboltable.toString(literal2,symboltable);
            case TRANSFORM: return inp + " => transformed clause " + Clause.toString(clause,symboltable);}
    return "";}


    /**
     * Collects the inference steps culminating in this in the list steps. Double occurrences are to be avoided.
     * Collects the inputClause ids of all clauses causing the current inference.
     *
     * @param steps a list for collecting the inference steps
     * @param ids   a list for collecting the inputClause ids
     */
    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        super.inferenceSteps(steps,ids);
        int id = inputClause[0];
        if(!ids.contains(id)) ids.add(id);}
}
