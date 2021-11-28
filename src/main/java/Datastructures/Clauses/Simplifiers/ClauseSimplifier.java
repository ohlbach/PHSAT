package Datastructures.Clauses.Simplifiers;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseStructure;
import Datastructures.Clauses.Connective;
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
    protected IntSupplier nextId;
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

    /** adds the literals of an AND-clause to the model
     *
     * @param clause an AND-Clause
     * @throws Unsatisfiable if the model finds an inconsistency
     */
    protected void simplifyAnd(Clause clause) throws Unsatisfiable {
        assert clause.connective == Connective.AND;
        InferenceStep step = null;
        if(trackReasoning) {
            step = clause.inferenceStep;
            if(monitoring) {monitor.print(monitorId,step.toString(symboltable));}}
        for(CLiteral cLiteral : clause) {model.add(cLiteral.literal,step,thread);}}

    IntArrayList intList1 = new IntArrayList();
    IntArrayList intList2 = new IntArrayList();

    /** replaces literals by equivalent literals, for every clause type
     * If trackReasoning then replacements are done in a clone of the clause,
     * otherwise in the original clause.
     *
     * @param oldClause the original clause
     * @return either the old clause, or a new clause with the replaced literals.
     */
    protected Clause replaceEquivalences(Clause oldClause){
        if(equivalenceClasses.isEmpty()) return oldClause;
        Clause newClause = oldClause.replaceEquivalences(getRepresentative, nextId, intList1);
        if(intList1.isEmpty()) return oldClause;
        if(trackReasoning) {
            InferenceStep step = new InfEquivalenceReplacements(oldClause,newClause, intList1,equivalenceClasses);
            newClause.inferenceStep = step;
            if(monitoring) monitor.print(monitorId,step.toString(symboltable));}
        return newClause;}

    /** removes double literals (from or-clauses) and complementary literals.
     * The clause may become contradictory or a tautology, or and and-clause.
     * The and-clause is inserted into the model.
     *
     * @param oldClause a clause
     * @return null or the simplified clause
     * @throws Unsatisfiable if a contradiction is discovered
     */
    protected Clause removeDoubleAndComplementaryLiterals(Clause oldClause) throws Unsatisfiable {
        Clause newClause = oldClause.removeDoubleAndComplementaryLiterals(nextId,intList1,intList2);
        if(newClause.structure == ClauseStructure.TAUTOLOGY) return null;
        if(intList1.isEmpty() && intList2.isEmpty()) return newClause;
        InferenceStep step = null;
        if(trackReasoning) {
            step = new InfDoubleAndComplementaryLiterals(oldClause,newClause,intList1,intList2);
            newClause.inferenceStep = step;
            if(monitoring) monitor.print(monitorId,step.toString(symboltable));}
        if(newClause.structure == ClauseStructure.CONTRADICTORY) {
            throw new Unsatisfiable(newClause);}
        if(newClause.connective == Connective.AND) {
            for(CLiteral cLiteral : newClause) model.add(cLiteral.literal,step,thread);
            return null;}
        return newClause;}


}
