package InferenceSteps;

import Datastructures.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.Consumer;

public class InfTrueLiteralInClause extends InferenceStep{
    @Override
    public String title() {
        return "True Literal in Clause";
    }

    @Override
    public String rule() {
        return "Various forms of extraction of true literals from a clause";
    }
    int[] clauseBefore;
    int literal;

    public InfTrueLiteralInClause(int[] clauseBefore, int literal) {
        this.clauseBefore = clauseBefore;
        this.literal = literal;}

    /**
     * Verifies the extraction of a true literal from a clause
     * <br>
     * All models satisfying the clause must also satisfy the literal
     *
     * @param monitor      the consumer for printing error messages
     * @param symboltable  the symbol table for mapping predicate names to integers
     * @return true if the verification is successful, false otherwise
     */
    @Override
    public boolean verify(Consumer<String> monitor, Symboltable symboltable) {
        IntArrayList predicates = Clause.literals(clauseBefore);
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

    @Override
    public String toString(Symboltable symboltable) {
        return title() + ": " + Clause.toString(clauseBefore, symboltable) + " -> " + Symboltable.toString(literal, symboltable);}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps, IntArrayList ids) {
        if(!steps.contains(this)) {
            int id = clauseBefore[0];
            steps.add(this);
            if(!ids.contains(id)) ids.add(id);}}
}
