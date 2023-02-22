package Datastructures.Clauses.SimplifiersOld;

import Datastructures.Clauses.AllClauses.InitializerSimplifier;
import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseStructure;
import Datastructures.Clauses.Connective;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Results.UnsatisfiableClause;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses.EquivalenceClasses;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import Management.Monitor.MonitorLife;
import Management.ProblemSupervisor;
import Utilities.BucketSortedIndex;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.function.IntSupplier;
import java.util.function.IntUnaryOperator;

public class ClauseSimplifier {
    private final ProblemSupervisor problemSupervisor;
    private final MonitorLife monitor;
    private final boolean monitoring;
    private final String monitorId;
    private final Model model;
    private final Thread thread;
    private final Symboltable symboltable;
    private final EquivalenceClasses equivalenceClasses;
    protected boolean trackReasoning;
    protected IntSupplier nextId;
    private final IntUnaryOperator getRepresentative;
    private final IntUnaryOperator getTruthStatus;
    private final BucketSortedIndex<CLiteral> literalIndex;
    private final InitializerSimplifier clauses;

    /** generates a Clause Transformer.
     * It can simplify all clause types and transform them into conjunctive normal form.
     *
     * @param clauses which keep all the clauses.
     * @param thread the calling thread
     */
    public ClauseSimplifier(InitializerSimplifier clauses, Thread thread) {
        this.clauses = clauses;
        problemSupervisor = clauses.problemSupervisor;
        monitor = clauses.monitor;
        monitorId = clauses.monitorId;
        monitoring = monitor != null;
        model = clauses.model;
        this.thread = thread;
        symboltable = null; //model.symboltable;
        equivalenceClasses = problemSupervisor.equivalenceClasses;
        this.literalIndex = clauses.literalIndex;
        trackReasoning = clauses.trackReasoning;
        if(trackReasoning) nextId = problemSupervisor::nextClauseId;
        getRepresentative = equivalenceClasses::getRepresentative;
        getTruthStatus    = model::status;
    }

    public Clause simplify(Clause clause) throws Unsatisfiable {
        if(!equivalenceClasses.isEmpty()) clause = replaceEquivalences(clause);
        clause = removeMultipleAndComplementaryLiterals(clause);
        if(clause.connective == Connective.AND) {
            andToModel(clause); return null;}
        clause = removeTrueFalseLiterals(clause);
        if(clause.connective == Connective.AND) {
            andToModel(clause); return null;}
        return clause;}

    /** adds the literals of an AND-clause to the model
     *
     * @param clause an AND-Clause
     * @throws Unsatisfiable if the model finds an inconsistency
     */
    public void andToModel(Clause clause) throws Unsatisfiable {
        assert clause.connective == Connective.AND;
        InferenceStep step = null;
        if(trackReasoning) {
            step = clause.inferenceStep;
            if(monitoring) {monitor.print(monitorId,step.toString(symboltable));}}
        for(CLiteral cLiteral : clause) {model.add(cLiteral.literal,step);}}

    IntArrayList intList1 = new IntArrayList();
    IntArrayList intList2 = new IntArrayList();

    /** replaces literals by equivalent literals, for every clause type
     * If trackReasoning then replacements are done in a clone of the clause,
     * otherwise in the original clause.
     *
     * @param oldClause the original clause
     * @return either the old clause, or a new clause with the replaced literals.
     */
    protected Clause replaceEquivalences(Clause oldClause)  throws Unsatisfiable {
        Clause newClause = oldClause.replaceEquivalences(equivalenceClasses, nextId);
        if(intList1.isEmpty()) return oldClause;
        if(trackReasoning) {
            InferenceStep step = null; //new InfEquivalenceReplacements(oldClause,newClause, intList1,equivalenceClasses);
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
    protected Clause removeMultipleAndComplementaryLiterals(Clause oldClause) throws Unsatisfiable {
        Clause newClause = oldClause.removeComplementaryLiterals(nextId);
        if(newClause.structure == ClauseStructure.TAUTOLOGY) return null;
        if(intList1.isEmpty() && intList2.isEmpty()) return newClause;
        InferenceStep step = null;
        if(trackReasoning) {
            step = new InfMultipleAndComplementaryLiterals(oldClause,newClause,intList1,intList2);
            newClause.inferenceStep = step;
            if(monitoring) monitor.print(monitorId,step.toString(symboltable));}
        if(newClause.structure == ClauseStructure.CONTRADICTORY) {throw new UnsatisfiableClause(newClause);}
        if(newClause.connective == Connective.AND) {
            for(CLiteral cLiteral : newClause) model.add(cLiteral.literal,step);
            return null;}
        return newClause;}

    protected Clause removeTrueFalseLiterals(Clause oldClause) throws Unsatisfiable {
        Clause newClause = oldClause.removeTrueFalseLiterals(getTruthStatus,nextId);
        if(newClause.structure == ClauseStructure.TAUTOLOGY) return null;
        if(intList1.isEmpty() && intList2.isEmpty()) return newClause;
        InferenceStep step = null;
        if(trackReasoning) {
            step = new InfTrueFalseLiterals(oldClause,newClause,intList1,intList2,model);
            newClause.inferenceStep = step;
            if(monitoring) monitor.print(monitorId,step.toString(symboltable));}
        if(newClause.structure == ClauseStructure.CONTRADICTORY) {throw new UnsatisfiableClause(newClause);}
        if(newClause.connective == Connective.AND) {
            for(CLiteral cLiteral : newClause) model.add(cLiteral.literal,step);
            return null;}
        return newClause;}



}
