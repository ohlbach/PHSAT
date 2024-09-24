package InferenceSteps;

import Datastructures.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.Consumer;

/** This inference step describes various forms of the extracction of true literals from a clause.
 * <p>
 * Examples: atleast 2 p,q  -&gt; true(p,q)
 */
public class InfTrueLiteralInClause extends InferenceStep{
    /**
     * Retrieves the title of the inference step.
     *
     * @return The title of the inference step.
     */
    @Override
    public String title() {
        return "True Literal from Clause";}

    /**
     * Retrieves the rule for various forms of extraction of true literals from a clause.
     *
     * @return The rule for extracting true literals from a clause.
     */
    @Override
    public String rule() {
        return "Various forms of extraction of true literals from a clause";}

    /** the clause before the extraction */
    private final int[] clauseBefore;

    /** the extracted true literal */
    int literal;

    /** the original inference steps of the clause */
    public ArrayList<InferenceStep> inferenceSteps;

    /**
     * InfTrueLiteralInClause represents an inference step that extracts a true literal from a clause.
     *
     * @param clauseBefore the original clause
     * @param inferenceSteps the original inference steps of the clause
     * @param literal the extracted literal
     */
    public InfTrueLiteralInClause(int[] clauseBefore, ArrayList<InferenceStep> inferenceSteps, int literal, String reasoner) {
        super(reasoner);
        this.clauseBefore = clauseBefore;
        this.literal = literal;
        this.inferenceSteps = inferenceSteps;}

    /**
     * Verifies the extraction of a true literal from a clause.
     * <br>
     * All models satisfying the clause must also satisfy the literal
     *
     * @param monitor      the consumer for printing error messages
     * @param symboltable  the symbol table for mapping predicate names to integers
     * @return true if the verification is successful, false otherwise
     */
    @Override
    public boolean verify(Consumer<String> monitor, Symboltable symboltable) {
        IntArrayList predicates = Clause.predicates(clauseBefore);
        int position = predicates.indexOf(Math.abs(literal));
        int nModels = 1 << predicates.size();
        for(int model = 0; model < nModels; ++model) {
            if(Clause.isTrue(clauseBefore, model)) {
                if(!((literal > 0) ? ((model & (1 << position)) != 0) : ((model & (1 << position)) == 0))) {
                    monitor.accept("Verification failed for extraction of true literal : " +
                        Symboltable.toString(literal,symboltable) + " from clause " +
                        Clause.toString(clauseBefore,symboltable) + "\nFalsifying model: " +
                        Clause.modelString(model,predicates,symboltable) );
                    return false;}}}
        return true;}

    /**
     * Converts the object to a string representation.
     *
     * @param symboltable the symbol table for mapping predicate names to integers
     * @return the string representation of the object
     */
    @Override
    public String toString(Symboltable symboltable) {
        return title() + " by " + reasoner + ": " +
                Clause.toString(clauseBefore, symboltable) + " -> true(" + Symboltable.toString(literal, symboltable)+")";}

    /**
     * Collects the inference steps culminating in this in the list "steps". Double occurrences are to be avoided.
     * Collects the inputClause ids of all clauses causing the current inference and stores them in the "ids" list.
     *
     * @param steps     A list for collecting the inference steps.
     * @param ids       A list of identifiers of input clauses.
     * @param reasoners A list for collecting the reasoners.
     */
    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids, ArrayList<String> reasoners) {
        super.inferenceSteps(steps,ids,reasoners);
        int id = clauseBefore[0];
        if(!ids.contains(id)) ids.add(id);
        if(inferenceSteps != null) {
            for(InferenceStep step : inferenceSteps) {step.inferenceSteps(steps,ids,reasoners);}}}
}
