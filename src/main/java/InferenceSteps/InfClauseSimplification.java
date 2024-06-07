package InferenceSteps;

import Datastructures.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.Consumer;

public class InfClauseSimplification extends InferenceStep{
    @Override
    public String title() {
        return "Clause Simplification";}

    @Override
    public String rule() {
        return "Various simplifications of clauses themselves";
    }

    private final int[] clauseBefore;
    private final int[] clauseAfter;

    public InfClauseSimplification(int[] clauseBefore, Clause clauseAfter) {
        super();
        this.clauseBefore = clauseBefore;
        this.clauseAfter = clauseAfter == null ? null : clauseAfter.simpleClone();}

    @Override
    public String toString(Symboltable symboltable) {
        String after = clauseAfter == null ? "unsatisfiable" : Clause.toString(clauseAfter, symboltable);
        return "Clause Simplification: " + Clause.toString(clauseBefore, symboltable) + " -> " + after;
    }

    /**
     * Verifies whether the simplified clause is a consequence of the original clause.
     * <br>
     * All models of the original clause must satisfy the simplified clause.
     *
     * @param monitor      a consumer that accepts a string message for reporting
     * @param symboltable  the symboltable used for predicate names
     * @return true if the simplification is sound, false otherwise
     */
    @Override
    public boolean verify(Consumer<String> monitor, Symboltable symboltable) {
        IntArrayList predicates = Clause.predicates(clauseBefore);
        int nModels = 1 << predicates.size();
        for(int model = 0; model < nModels; model++) {
            if(Clause.isTrue(clauseBefore,model))  {
                 if(clauseAfter == null) { // must be an unsatisfiable clause
                     monitor.accept("Clause " + Clause.toString(clauseBefore,symboltable) +
                             " is supposed to be unsatsifiable, but satisfied by " + Clause.modelString(model,predicates,symboltable));
                     continue;}
                 int fModel = model; // final
                 if(!Clause.isTrue(clauseAfter,
                         (literal) -> {
                            int position = predicates.indexOf(Math.abs(literal));
                            return ((literal > 0) ? ((fModel & (1 << position)) != 0) : ((fModel & (1 << position)) == 0));})) {
                     monitor.accept("Model "+ Clause.modelString(model,predicates,symboltable) + " of clause " +
                             Clause.toString(clauseBefore,symboltable) + " does not satisfy simplified clause " +
                             Clause.toString(clauseAfter,symboltable)+"\n");
                     return false;}}}
        return true;}

    /**
     * Collects the inference steps culminating in this step in the list `steps`. Double occurrences are to be avoided.
     * Additionally, it collects the inputClause ids of all clauses causing the current inference.
     *
     * @param steps A list for collecting the inference steps.
     * @param ids   An IntArrayList for collecting the inputClause ids.
     */
    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        super.inferenceSteps(steps,ids);
        int id = clauseBefore[0];
        if(!ids.contains(id)) ids.add(id);}

}
