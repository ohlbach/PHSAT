package InferenceSteps;

import Datastructures.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.function.Consumer;

/** This class represents Merge-Resolution and Linked Merge-Resolution steps.
 * <br>
 * Examples: <br>
 * Merge-Resolution:  p,phi,psi and -p,phi =&gt; phi,psi (destructively)<br>
 * Linked Merge-Resolution: p,q and -p,phi,psi and -q,phi =&gt; phi,psi (destructively).
 */
public class InfMergeResolution extends InferenceStep {
    /** the title of the inference step.
     */
    @Override
    public String title() {
        return link == null ? "Merge Resolution" : "Linked Merge Resolution";}

    /** a description of the inference rule */
    @Override
    public String rule() {
        return link == null ?
                "p,phi and -p,phi,psi => phi,psi" :
                "p,q and -p,phi and -q,phi,psi => phi,psi";}

    /** the two-literal clause which serves as link in the Linked Merge-Resolution */
    int[] link;
    /** the first parent clause */
    int[] parent1;
    /** the second parent clause */
    int[] parent2;
    /** the resolvent, the parent clauses which has been shortened */ 
    int[] resolvent;

    /** creates the Merge-Resolution inference step.
     *
     * @param parent1 the first parent
     * @param parent2 the second parent
     * @param resolvent the merge-resolvent
     */
    public InfMergeResolution(int[] parent1, int[] parent2, Clause resolvent, String reasoner) {
        super(reasoner);
        this.parent1 = parent1;
        this.parent2 = parent2;
        this.resolvent = resolvent.simpleClone();}

    /** creates the Linked Merge-Resolution inference step.
     *
     * @param link   the two-literal clause which serves as link in the Linked Merge-Resolution
     * @param parent1 the first parent
     * @param parent2 the second parent
     * @param resolvent the merge-resolvent
     */
    public InfMergeResolution(Clause link, int[] parent1, int[] parent2, Clause resolvent) {
        this.link = link.simpleClone();
        this.parent1 = parent1;
        this.parent2 = parent2;
        this.resolvent = resolvent.simpleClone();
    }

    /** model-based verification the inference step
     * <br>
     * If the verification failed an error message is sent to the monitor.
     *
     * @param monitor      the consumer for monitoring the verification process
     * @param symboltable  the symbol table for mapping predicate names to integers
     * @return true if the verification succeeded
     */
    public boolean verify(Consumer<String> monitor, Symboltable symboltable) {
        IntArrayList predicates = Clause.predicates(parent1,parent2);
        for(int model : Clause.getModels(parent2,predicates)) {
            if(Clause.isTrue(parent1, model,predicates) && (link == null || Clause.isTrue(link,model,predicates))) {
                if (!Clause.isTrue(resolvent, model, predicates)) {
                    monitor.accept("Error: " + toString(symboltable) +
                            "\n Resolution failed for model:  " + Clause.modelString(model, predicates, symboltable));
                    return false;}}}
        return true;}

    /** describes the inference step.
     *
     * @param symboltable null or a symboltable
     * @return a description of the inference step.
     */
    @Override
    public String toString(Symboltable symboltable) {
        if(link == null)
            return title() + ": " + Clause.toString(parent1,symboltable) + " and " + Clause.toString(parent2,symboltable) +
                " => " + Clause.toString(resolvent,symboltable);
        return title() + ": " + Clause.toString(link,symboltable) + " and " + Clause.toString(parent1,symboltable) + " and " +
                Clause.toString(parent2,symboltable) +
                " => " + Clause.toString(resolvent,symboltable);
    }
}
