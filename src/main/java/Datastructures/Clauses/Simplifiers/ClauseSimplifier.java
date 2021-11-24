package Datastructures.Clauses.Simplifiers;

import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import Management.Monitor;
import Management.ProblemSupervisor;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.function.IntSupplier;
import java.util.function.IntUnaryOperator;

public class ClauseSimplifier {
    private final ProblemSupervisor problemSupervisor;
    private final Monitor monitor;
    private final boolean monitoring;
    private final String monitorId;
    private final Model model;
    private final Thread thread;
    private final Symboltable symboltable;
    private final EquivalenceClasses equivalenceClasses;
    protected boolean trackReasoning;
    private  IntSupplier nextId;
    private final IntUnaryOperator getRepresentative;

    /** generates a Clause Transformer.
     * It can simplify all clause types and transform them into conjunctive normal form.
     *
     * @param problemSupervisor that supervises the problem solution
     * @param monitor for monitoring the actions.
     */
    public ClauseSimplifier(ProblemSupervisor problemSupervisor, Monitor monitor, String monitorId, Thread thread) {
        this.problemSupervisor = problemSupervisor;
        this.monitor = monitor;
        this.monitorId = monitorId;
        monitoring = monitor != null;
        model = problemSupervisor.model;
        this.thread = thread;
        symboltable = model.symboltable;
        equivalenceClasses = problemSupervisor.equivalenceClasses;
        trackReasoning = problemSupervisor.globalParameters.trackReasoning;
        if(trackReasoning) nextId = () -> problemSupervisor.nextClauseId();
        getRepresentative = (int literal) -> equivalenceClasses.getRepresentative(literal);
    }

    public Clause simplify(Clause clause) throws Unsatisfiable {
        clause = replaceEquivalences(clause);
        return clause;
    }

    IntArrayList intList1 = new IntArrayList();
    IntArrayList intList2 = new IntArrayList();

    /** replaces literals by equivalent literals, for every clause type
     * If trackReasoning then replacements are done in a clone of the clause,
     * otherwise in the original clause.
     *
     * @param oldClause the original clause
     * @return either the unchanged old clause, or a new clause with the replaced literals.
     */
    protected Clause replaceEquivalences(Clause oldClause) {
        if(equivalenceClasses.isEmpty()) return oldClause;

        if(trackReasoning) {
            intList1.clear();
            Clause newClause = oldClause;
            for(int i = 0; i < newClause.size(); ++i) {
                int oldLiteral = newClause.getLiteral(i);
                int newLiteral = equivalenceClasses.getRepresentative(oldLiteral);
                if (oldLiteral == newLiteral) continue;
                intList1.add(i);
                if(newClause == oldClause) {
                    newClause = oldClause.clone(problemSupervisor.nextClauseId());}
                newClause.cliterals.get(i).literal = newLiteral;}
            if(newClause == oldClause) return oldClause;

            InferenceStep step = new EquivalenceReplacements(oldClause,newClause, intList1,equivalenceClasses);
            newClause.inferenceStep = step;
            if(monitoring) monitor.print(monitorId,step.toString(symboltable));
            return newClause;}
        else{
            for(CLiteral cLiteral : oldClause) { // destructive replacements
                cLiteral.literal = equivalenceClasses.getRepresentative(cLiteral.literal);}
            return oldClause;}}
}
