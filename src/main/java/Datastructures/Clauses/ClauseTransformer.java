package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import InferenceSteps.*;
import Management.Monitor;
import Management.ProblemSupervisor;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class ClauseTransformer {
    private ProblemSupervisor problemSupervisor;
    private Monitor monitor;
    private boolean monitoring;
    private Model model;
    private EquivalenceClasses equivalenceClasses;
    private boolean trackReasoning;

    public ClauseTransformer(ProblemSupervisor problemSupervisor, Monitor moritor) {
        this.problemSupervisor = problemSupervisor;
        this.monitor = monitor;
        monitoring = moritor != null;
        model = problemSupervisor.model;
        equivalenceClasses = problemSupervisor.equivalenceClasses;
        trackReasoning = problemSupervisor.globalParameters.trackReasoning;
    }

    // PROCESSING OF AND-CLAUSES

    /** adds AND-clauses to the model
     *
     * @param clause an AND-Clause
     * @param thread where the clause is processed
     * @throws Unsatisfiable if the model finds an inconsistency
     */
    public Clause analyseAnd(Clause clause, Thread thread) throws Unsatisfiable {
        assert clause.clauseType == ClauseType.AND;
        InferenceStep step = clause.inferenceStep;
        for(CLiteral cLiteral : clause) {
            model.add(cLiteral.literal,step,thread);}
        return null;}

    // PROCESSING OF OR-CLAUSES

    /** checks an OR-clause for possible simplifications.
     * - double literals are removed<br>
     * - tautologies are detected: the clause can be ignored <br>
     * - true literals are detected: the clause can be ignored <br>
     * - literals are replaced by equivalent literals <br>
     * - false literals are removed <br>
     * - empty clauses cause Unsatisfiable to be thrown<br>
     * - unit clauses are put into the model
     *
     * @param clause    an OR-clause
     * @param monitorId for monitoring the actions
     * @param thread    where the clause is processed
     * @return          null or the original unchanged clause or a new changed clause
     * @throws Unsatisfiable if a contradiction is found.
     */
    public Clause analyseOr(Clause clause,String monitorId,Thread thread) throws Unsatisfiable {
        assert clause.clauseType == ClauseType.OR;
        clause = replaceEquivalences(clause,monitorId);
        clause.removeDoubles(); // will not be monitored
        clause = check01InOr(clause,thread);
        if(clause == null) return null;

        int size = clause.size();
        for(CLiteral cLiteral : clause) { // check for true literals
            if(model.isTrue(cLiteral.literal)) {
                if(monitor != null) {
                    monitor.print(monitorId,"Clause " + clause.toString(0, model.symboltable) +
                    " is true in the current model");}
                return null;}} // will be ignored

        for(int i = 0; i < size; ++i) {  // check for tautologies
            int literal = clause.getLiteral(i);
            for(int j = i+1; j < size; ++j) {
                if(literal == -clause.getLiteral(j)) {
                    if(monitor != null) {
                        monitor.print(monitorId,"Clause " + clause.toString(0, model.symboltable) +
                                " is a tautology");}
                    return null;}}}
        Clause newClause = deleteFalseLiteralsInOr(clause,monitorId);
        return (newClause == clause) ? clause : check01InOr(newClause,thread);}

    /** checks if the clause is empty or contains just one literal
     *
     * @param clause a clause to be checked
     * @param thread where the clause is processed
     * @return either null or the unchanged clause
     * @throws Unsatisfiable if a unit clause causes a contradiction
     */
    public Clause check01InOr(Clause clause, Thread thread) throws Unsatisfiable{
        switch(clause.size()) {
            case 0: throw new Unsatisfiable(clause.inferenceStep);
            case 1: model.add(clause.getLiteral(0),clause.inferenceStep,thread);
                    return null;}
        return clause;}

    /** replaces literals by equivalent literals, in any clause type
     *
     * @param oldClause the original clause
     * @param monitorId for monitoring replacements
     * @return either the unchanged old clause or a new clause with the replaced literals.
     */
    public Clause replaceEquivalences(Clause oldClause, String monitorId) {
        if(equivalenceClasses.isEmpty()) return oldClause;
        Clause newClause = null;
        IntArrayList positions = null;
        for(int i = 0; i < oldClause.size(); ++i) {
            int oldLiteral = oldClause.getLiteral(i);
            int newLiteral = equivalenceClasses.getRepresentative(oldLiteral);
            if (oldLiteral == newLiteral) continue;
            if(newClause == null) {
                newClause = oldClause.clone(problemSupervisor.nextClauseId());
                positions = new IntArrayList();}
            newClause.getCLiteral(i).literal = newLiteral;
            positions.add(i);}
        if(newClause == null) return oldClause;

        if(trackReasoning) {
            InferenceStep step = new EquivalenceReplacements(oldClause,newClause,positions,equivalenceClasses);
            newClause.inferenceStep = step;
            if(monitoring) monitor.print(monitorId,step.toString());}
    return newClause;}

    /** delete false literals in OR-clauses
     *
     * @param oldClause a clause
     * @param monitorId for monitoring the deletion
     * @return either the unchanged old clause or a new shorter clause.
     */
    public Clause deleteFalseLiteralsInOr(Clause oldClause, String monitorId) {
        assert oldClause.clauseType == ClauseType.OR;
        IntArrayList positions = null;
        int size = oldClause.size();
        for(int i = 0; i < size; ++i) {
            if(model.isFalse(oldClause.getLiteral(i))) {
                positions = new IntArrayList();}
                positions.add(i);}
        if(positions == null) return oldClause;
        Clause newClause = new Clause(problemSupervisor.nextClauseId(),ClauseType.OR,size-positions.size());
        for(int i = 0; i < size; ++i) {
            if(!positions.contains(i)) newClause.add(oldClause.getLiteral(i));}
        if(trackReasoning) {
            InferenceStep step = new FalseLiteralDeletion(oldClause,newClause,positions,model);
            newClause.inferenceStep = step;
            if(monitoring) monitor.print(monitorId,step.toString());}
        return newClause;}

    // PROCESSING OF EQUIV-CLAUSES

    /** checks an EQUIV-clause for possible simplifications.
     *  - equivalent literals are replaced.
     *  - double literals are removed <br>
     *  - a unit clause can be ignored
     *  - p == -p is a contradiction
     *  - a true/false literal causes all other literals to become true/false
     *
     * @param clause    the clause to be checked
     * @param monitorId for monitoring the actions
     * @param thread    which processes the clause
     * @return null or the original clause or a new changed clause
     * @throws Unsatisfiable if a contradiction is discovered
     */
    public Clause analyseEquiv(Clause clause, String monitorId, Thread thread) throws Unsatisfiable {
        assert clause.clauseType == ClauseType.EQUIV;
        clause = replaceEquivalences(clause,monitorId);
        clause.removeDoubles();             // will not be monitored
        int size = clause.size();
        if(size <= 1) return null;          // can be ignored

        int literal = 0; int status = 0;

        for(int i = 0; i < size; ++i) {        // check for p = -p  is unsatisfiable
            int literal1 = clause.getLiteral(i);
            status = model.status(literal1);
            if(status != 0) {literal = literal1;}
            for(int j = i+1; j < size; ++j) {
                int literal2 = clause.getLiteral(j);
                if(literal1 == -literal2) {
                    throw new Unsatisfiable(new EquivalenceInconsistency(clause,literal1,literal2));}}}

        // literal has a truth value. All other literals must get the same truth value
        if(status != 0) {
            IntArrayList trueLiterals = new IntArrayList();
            for(int i = 0; i < size; ++i) {          // collect all the other true literals
                int literal1 = clause.getLiteral(i);
                int status1 = model.status(literal1);
                if(literal1 != literal && (status1 == 0 || status1 != status)) trueLiterals.add(status*literal1);}
            InferenceStep step = null;
            if(trackReasoning) {
                step = new EquivalentTrueLiterals(clause,status*literal,trueLiterals,model);
                if(monitoring) monitor.print(monitorId,step.toString());}
            for(int trueLiteral : trueLiterals) model.add(trueLiteral,step,thread);
            return null;}
        return clause;}

    //  PROCESSING OF ATLEAST-CLAUSES

    /** analyses atleast clauses
     * The following situations are treated:
     * - equivalent literals are replaced.<br>
     * - atleast 0 p,q,r is always true<br>
     * - atleast 1 p,q,r -> p | q | r<br>
     * - atleast 3 p,q,r -> p & q & r<br>
     * - atleast 3 p,-p,q,r -> atleast 2 q,r<br>
     * - atleast 2 p,q,r and true(p) -> atleast 1 q,r<br>
     * - atleast 2 p,q,r and false(p) -> atleast 2 q,r<br>
     *
     * Double literals are kept as they are
     *
     *
     * @param clause     the clause to be analysed
     * @param monitorId  for monitoring the steps
     * @param thread     which processes the clause
     * @return           null or an or-clause or a shortened atleast-clause or the original clause
     * @throws Unsatisfiable if a contradiction is detected
     */
    public Clause analyseAtleast(Clause clause, String monitorId, Thread thread) throws Unsatisfiable {
        assert clause.clauseType == ClauseType.ATLEAST;

        clause = analyseAtleastSimpleCases(clause,monitorId,thread);
        if(clause == null) return null;
        if(clause.clauseType != ClauseType.ATLEAST) return clause;

        int quantifier = clause.quantifier;
        int size = clause.size();

        clause = replaceEquivalences(clause,monitorId);

        // atleast 2 p,q,r and true(p) -> atleast 1 q,r
        // atleast 2 p,q,r and false(p) -> atleast 2 q,r

        IntArrayList falseLiterals = null;
        IntArrayList trueLiterals  = null;
        for(int i = 0; i < size; ++i) {
            int literal = clause.getLiteral(i);
            switch(model.status(literal)) {
                case -1: if(falseLiterals == null) falseLiterals = new IntArrayList();
                        falseLiterals.add(i);
                    break;
                case +1: if(trueLiterals == null) trueLiterals = new IntArrayList();
                        trueLiterals.add(i);}}

        if(falseLiterals != null || trueLiterals != null) {
            Clause newClause = new Clause(problemSupervisor.nextClauseId(),ClauseType.ATLEAST);
            for(int i = 0; i < size; ++i) {
                if((falseLiterals != null && falseLiterals.contains(i)) ||
                        trueLiterals != null && trueLiterals.contains(i)) continue;
                newClause.add(clause.getLiteral(i));}
            newClause.quantifier = quantifier;
            if(trueLiterals != null) newClause.quantifier -= trueLiterals.size();

            InferenceStep step = null;
            if(trackReasoning) {
                step = new AtleastTrueFalse(clause,newClause,trueLiterals,falseLiterals,model);
                if(monitoring) monitor.print(monitorId, step.toString());}
            newClause.inferenceStep = step;

            newClause = analyseAtleastSimpleCases(newClause,monitorId,thread);
            if(newClause == null) return null;
            if(newClause.clauseType != ClauseType.ATLEAST) return clause;
            clause = newClause;}

        // atleast 2 p,-p q -> atleast 1 q
        return analyseAtleastPNotP(clause,monitorId,thread);
    }

    /** analyses complementary literals in atleast-clauses
     *  atleast 2 p,-p,q,r -> atleast 1 q,r
     *
     * @param clause    the clause to be analysed
     * @param monitorId  for monitoring the steps
     * @param thread     which processes the clause
     * @return           null or an or-clause or a shortened atleast-clause or the original clause
     * @throws Unsatisfiable if a contradiction is detected
     */
    private Clause analyseAtleastPNotP(Clause clause, String monitorId, Thread thread) throws Unsatisfiable{
        int size = clause.size();
        for(int i = 0; i < size; ++i) {
            int literal = clause.getLiteral(i);
            for(int j = i+1; j < size; ++j) {
                if(literal == -clause.getLiteral(j)) {
                    Clause newClause = new Clause(problemSupervisor.nextClauseId(),ClauseType.ATLEAST);
                    for(int k = 0; k < size; ++k) {
                        if(k != i && k != j) newClause.add(clause.getLiteral(k));}
                    newClause.quantifier = clause.quantifier-1;
                    if(trackReasoning) {
                        InferenceStep step = new AtleastPNotP(clause,newClause);
                        newClause.inferenceStep = step;
                        if(monitoring) {monitor.print(monitorId,step.toString(model.symboltable));}}

                    Clause nextClause = analyseAtleastSimpleCases(newClause,monitorId,thread);
                    if(nextClause == null) return null;
                    if(nextClause.clauseType != ClauseType.ATLEAST) return nextClause;
                    // recursive call ir several p,-p literals are in the clause
                    return analyseAtleastPNotP(nextClause,monitorId,thread);}}}
        return clause;}

    /** analyses some simple cases of atleast-clauses
     * - atleast 0 p,q,r is always true<br>
     * - atleast 1 p,q,r is p | q | r<br>
     * - atleast 3 p,q,r is p & q & r
     *
     * @param clause    the clause to be analysed
     * @param monitorId for monitoring the step
     * @param thread    which processes the clause
     * @return null or an or-clause or the original clause
     * @throws Unsatisfiable if a contradiction is detected
     */
    private Clause analyseAtleastSimpleCases(Clause clause, String monitorId, Thread thread) throws Unsatisfiable {
        int quantifier = clause.quantifier;
        if(quantifier == 0) return null; // atleast 0 p,q,r is always true

        if(quantifier == 1) {   // atleast 1 p,q,r is equivalent to p or q or r
            clause.clauseType = ClauseType.OR;
            clause.quantifier = 0;
            return analyseOr(clause,monitorId,thread);}

        int size = clause.size();

        if(quantifier > size) {      // atleast 3 p,q  is the same as atleast 3 p,q,true is the same as atleast 2 p,q
            clause.quantifier = size;
            quantifier = size;}

        if(quantifier == size) { // atleast 3 p,q,r is equivalent to p & q & r
            Clause andClause = clause.clone(problemSupervisor.nextClauseId());
            andClause.clauseType = ClauseType.AND;
            andClause.quantifier = 0;
            if(trackReasoning) {
                InferenceStep step = new AtleastToAnd(clause,andClause);
                andClause.inferenceStep = step;
                if(monitoring) monitor.print(monitorId,step.toString());}
            return analyseAnd(andClause,thread);}

        return clause;
    }

}
