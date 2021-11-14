package Datastructures.Clauses.Simplifiers;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.Connective;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import InferenceSteps.InferenceStep;
import InferenceSteps.UnitResolution;
import Management.Monitor;
import Management.ProblemSupervisor;
import it.unimi.dsi.fastutil.ints.IntArrayList;

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
    }

    public Clause simplify(Clause clause) throws Unsatisfiable {
        clause = replaceEquivalences(clause);
        switch(clause.connective) {
            case AND:     simplifyAnd(clause); return null;
            case OR:      return simplifyOr(clause);
            case ATLEAST: return simplifyAtleast(clause);
            case ATMOST:
            case EXACTLY:
        }
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

    /** simplifies an Or-oldClause (destructively)
     * - multiple occurrences of literals are removed<br>
     * - tautologies cause null to be returned <br>
     * - true literals cause null to be returned<br>
     * - false literals are removed.
     * - the empty oldClause causes Unsatisfiable to be thrown <br>
     * Multiple literal removal is done on the original clause.<br>
     * False literal removal generates a clone of the clause, but only if trackReasoning = true
     *
     * @param oldClause an Or-clause
     * @return the old clause or the new clause or null (tautology or true literal)
     * @throws Unsatisfiable if the clause became empty
     */
    protected Clause simplifyOr(Clause oldClause) throws Unsatisfiable {
        intList1.clear();
        Clause newClause = oldClause;
        for(int i = 0; i < newClause.size(); ++i) {
            int literal = newClause.cliterals.get(i).literal;
            switch(newClause.contains(literal,i)) {
                case -1: return null; // tautology
                case +1: newClause.removeAtPosition(i--); continue;}
            switch(model.status(literal)) {
                case +1: return null; // oldClause is true
                case -1:
                    if(trackReasoning && newClause == oldClause) {
                        newClause = oldClause.clone(problemSupervisor.nextClauseId());}
                    if(!intList1.contains(-literal)) intList1.add(-literal);
                    newClause.removeAtPosition(i--); continue;}}
        if(oldClause != newClause) {
            InferenceStep step = new UnitResolution(oldClause,newClause, intList1,model);
            newClause.inferenceStep = step;
            if(monitoring) monitor.print(monitorId,step.toString(symboltable));}
        if(newClause.size() == 0) throw new Unsatisfiable(newClause);
        return newClause;}

    /** simplifies and atleast clause
     * - multiple occurrences of literals are removed: atleast m p,p,q,... -> atleast m-1 p,q,...<br>
     * - tautologies are removed: atleast m p,-p,q,... -> atleast m-1 q,...  <br>
     * - true literals are removed:  atleast m p,q,... and true(p) -> atleast m-1 q,...  <br>
     * - false literals are removed atleast m p,q,... and false(p) -> atleast m q,... .
     * - too few literals causes Unsatisfiable to be thrown <br>
     * - atleast 0 ... is always true <br>
     * - atleast m p_1,...p_m -> true(p_1,...,p_m)
     *
     * If trackReasoning then the simplification are done on a clone of the clause,
     * otherwise destructively at the original clause
     *
     * @param clause the clause to be simplified
     * @return either the original clause or a simplified clause or null
     * @throws Unsatisfiable if a contradiction was found.
     */
    protected Clause simplifyAtleast(Clause clause) throws Unsatisfiable  {
        return trackReasoning ?
                simplifyAtleastCloning(clause) :
                simplifyAtleastDestructively(clause);}

    /** This is the version of simplifyAtleast which works directly at the clause
     *
     * @param clause the clause to be simplified
     * @return null or the clause, possibly simplified
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected Clause simplifyAtleastDestructively(Clause clause) throws Unsatisfiable {
        removeSuperfluousLiteralsDestructively(clause);
        int quAmount = clause.quAmount;
        if(quAmount <= 0) return null; // atleast 0 is always true
        if(quAmount > clause.size()) {throw new Unsatisfiable(clause);}
        if(quAmount == clause.size()) {
            for(CLiteral cLiteral : clause) {
                model.add(cLiteral.literal,null,thread);}
            return null;}
        if(quAmount == 1) {clause.connective = Connective.OR;}
        return clause;}

    /** This is the version of simplifyAtleast which works at a clone of the clause
     *
     * @param clause the clause to be simplified
     * @return null or the orignal clause or a simplified clone
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected Clause simplifyAtleastCloning(Clause clause) throws Unsatisfiable {
        clause = removeSuperfluousLiteralsCloning(clause);
        int quAmount = clause.quAmount;
        if(quAmount <= 0) return null;                      // atleast 0 is always true
        if(quAmount > clause.size()) {throw new Unsatisfiable(clause);} // atleast 3 p,q is false
        if(quAmount == clause.size()) {                  // atleast 3 p,q,r -> true(p,q,r)
            for(CLiteral cLiteral : clause) {
                model.add(cLiteral.literal,new QuantifiedToModel(clause,cLiteral.literal),thread);}
            return null;}
        if(quAmount == 1) {clause.connective = Connective.OR;}
        return clause;}

    /** simplifies and atmost clause
     * - multiple occurrences of literals are removed: atmost m p,p,q,... -> atmost m-1 p,q,...<br>
     * - tautologies are removed: atmost m p,-p,q,... -> atmost m-1 q,...  <br>
     * - true literals are removed:  atmost m p,q,... and true(p) -> atmost m-1 q,...  <br>
     * - false literals are removed atmost m p,q,... and false(p) -> atmost m q,... .
     * - atmost 0 p_1,...p_m -> false(p_1,...,p_m)
     * - atmost m p_1,...,p_m,q,... is always true
     *
     * If trackReasoning then the simplification are done on a clone of the clause,
     * otherwise destructively at the original clause
     *
     * @param clause the clause to be simplified
     * @return either the original clause or a simplified clause or null
     * @throws Unsatisfiable if a contradiction was found.
     */
    protected Clause simplifyAtmost(Clause clause) throws Unsatisfiable  {
        return trackReasoning ?
                simplifyAtmostCloning(clause) :
                simplifyAtmostDestructively(clause);}

    /** This is the version of simplifyAtmost which works directly at the clause
     *
     * @param clause the clause to be simplified
     * @return null or the clause, possibly simplified
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected Clause simplifyAtmostDestructively(Clause clause) throws Unsatisfiable {
        removeSuperfluousLiteralsDestructively(clause);
        int quAmount = clause.quAmount;
        if(quAmount >= clause.size()) {return null;}  // atmost 3 p,q  is always true
        if(quAmount <= 0) {                           // atmost 0 p,q -> false(p,q)
            for(CLiteral cLiteral : clause) {
                model.add(-cLiteral.literal,null,thread);}
            return null;}
        return clause;}

    /** This is the version of simplifyAtmost which works at a clone og the clause
     *
     * @param clause the clause to be simplified
     * @return null or the original clause or a simplified clone
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected Clause simplifyAtmostCloning(Clause clause) throws Unsatisfiable {
        clause = removeSuperfluousLiteralsCloning(clause);
        int quAmount = clause.quAmount;
        if(quAmount >= clause.size()) {return null;}  // atmost 3 p,q  is always true
        if(quAmount <= 0) {                           // atmost 0 p,q -> false(p,q)
            for(CLiteral cLiteral : clause) {
                model.add(-cLiteral.literal,new QuantifiedToModel(clause,-cLiteral.literal),thread);}
            return null;}
        return clause;}


    /** simplifies and exactly clause
     * - multiple occurrences of literals are removed: exactly m p,p,q,... -> exactly m-1 p,q,...<br>
     * - tautologies are removed:    exactly m p,-p,q,... -> exactly m-1 q,...  <br>
     * - true literals are removed:  exactly m p,q,... and true(p) -> exactly m-1 q,...  <br>
     * - false literals are removed  exactly m p,q,... and false(p) -> exactly m q,... .
     * - exactly 0 p_1,...p_m -> false(-p_1,...,-p_m)
     * - exactly m p_1,...p_m -> true(p_1,...,p_m)
     * - exactly m p_1,...,p_m,q,... is false
     *
     * If trackReasoning then the simplification are done on a clone of the clause,
     * otherwise destructively at the original clause
     *
     * @param clause the clause to be simplified
     * @return either the original clause or a simplified clause or null
     * @throws Unsatisfiable if a contradiction was found.
     */
    protected Clause simplifyExactly(Clause clause) throws Unsatisfiable  {
        return trackReasoning ?
                simplifyExactlyCloning(clause) :
                simplifyExactlyDestructively(clause);}

    /** This is the version of simplifyExactly which works directly at the clause
     *
     * @param clause the clause to be simplified
     * @return null or the clause, possibly simplified
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected Clause simplifyExactlyDestructively(Clause clause) throws Unsatisfiable {
        removeSuperfluousLiteralsDestructively(clause);
        int quAmount = clause.quAmount;
        if(quAmount > clause.size()) {throw new Unsatisfiable(clause);} // exactly 3 p,q is false
        if(quAmount == clause.size()) {     // exactly 3 p,q,r -> true(p,q,r)
            for(CLiteral cLiteral : clause) {
                model.add(cLiteral.literal,null,thread);}
            return null;}
        if(quAmount <= 0) {  // exactly 0 p,q,r -> false(p,q,r)
            for(CLiteral cLiteral : clause) {
                model.add(-cLiteral.literal,null,thread);}
            return null;}
        return clause;}

    /** This is the version of simplifyExactly which works at a clone of the clause
     *
     * @param clause the clause to be simplified
     * @return null or the clause, possibly simplified
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected Clause simplifyExactlyCloning(Clause clause) throws Unsatisfiable {
        clause = removeSuperfluousLiteralsCloning(clause);
        int quAmount = clause.quAmount;
        if(quAmount > clause.size()) {throw new Unsatisfiable(clause);} // exactly 3 p,q is false
        if(quAmount == clause.size()) {     // exactly 3 p,q,r -> true(p,q,r)
            for(CLiteral cLiteral : clause) {
                model.add(cLiteral.literal,new QuantifiedToModel(clause,cLiteral.literal),thread);}
            return null;}
        if(quAmount <= 0) {  // exactly 0 p,q,r -> false(p,q,r)
            for(CLiteral cLiteral : clause) {
                model.add(-cLiteral.literal,new QuantifiedToModel(clause,-cLiteral.literal),thread);}
            return null;}
        return clause;}


    /** removes superfluous literals directly from the clause
     * - multiple occurrences of literals are removed: exactly m p,p,q,... -> exactly m-1 p,q,...<br>
     * - tautologies are removed:    exactly m p,-p,q,... -> exactly m-1 q,...  <br>
     * - true literals are removed:  exactly m p,q,... and true(p) -> exactly m-1 q,...  <br>
     * - false literals are removed  exactly m p,q,... and false(p) -> exactly m q,... .
     *
     * @param clause a clause
     */
    private void removeSuperfluousLiteralsDestructively(Clause clause) {
        for(int i = 0; i < clause.size(); ++i) {
            int literal1 = clause.getLiteral(i);
            switch (model.status(literal1)) {
                case +1: --clause.quAmount; clause.removeAtPosition(i--); continue; // one true literal less
                case -1: clause.removeAtPosition(i--); continue;}  // false literals don't count
            for(int j = 0; j < i; ++j) {
                int literal2 = clause.getLiteral(j);
                if(literal1 == literal2) {--clause.quAmount; clause.removeAtPosition(i--); continue;}
                if(literal1 == -literal2) {
                    --clause.quAmount;                 // exactly one literal must be true
                    clause.removeAtPosition(i);
                    clause.removeAtPosition(j);
                    i -= 2; continue;}}}}

    /** removes superfluous literals from a clone of the clause
     * - multiple occurrences of literals are removed: exactly m p,p,q,... -> exactly m-1 p,q,...<br>
     * - tautologies are removed:    exactly m p,-p,q,... -> exactly m-1 q,...  <br>
     * - true literals are removed:  exactly m p,q,... and true(p) -> exactly m-1 q,...  <br>
     * - false literals are removed  exactly m p,q,... and false(p) -> exactly m q,... .
     *
     * @param newClause a clause
     */
    protected Clause removeSuperfluousLiteralsCloning(Clause newClause) throws Unsatisfiable {
        Clause oldClause = newClause;
        intList1.clear();
        for(int i = 0; i < newClause.size(); ++i) { // remove true/false literals
            int literal1 = newClause.getLiteral(i);
            int status = model.status(literal1);
            if(status != 0) {
                if(oldClause == newClause) {newClause = oldClause.clone(problemSupervisor.nextClauseId());}
                newClause.removeAtPosition(i--);
                switch (status) {
                    case +1: if(!intList1.contains(literal1)) intList1.add(literal1); --newClause.quAmount;  break;
                    case -1: if(!intList2.contains(literal1)) intList2.add(literal1); break;}
                continue;}}
        if(newClause != oldClause) {
            InferenceStep step = new TrueFalseLiteralsQuantified(oldClause,newClause,intList1,intList2,model);
            newClause.inferenceStep = step;
            if(monitoring) {monitor.print(monitorId,step.toString(symboltable));}
            oldClause = newClause;}

        for(int i = 0; i < newClause.size(); ++i) { // remove double/complementary literals
            int literal1 = newClause.getLiteral(i);
            for(int j = 0; j < i; ++j) {
                int literal2 = newClause.getLiteral(j);
                if(literal1 == literal2) {
                    if(oldClause == newClause) {newClause = oldClause.clone(problemSupervisor.nextClauseId());}
                    --newClause.quAmount; newClause.removeAtPosition(i--);
                    continue;}
                if(literal1 == -literal2) {
                    if(oldClause == newClause) {newClause = oldClause.clone(problemSupervisor.nextClauseId());}
                    --newClause.quAmount;                 // exactly one literal must be true
                    newClause.removeAtPosition(i);
                    newClause.removeAtPosition(j);
                    i -= 2; continue;}}}
        if(newClause != oldClause) {
            InferenceStep step = new MultipleLiteralsQuantified(oldClause,newClause);
            newClause.inferenceStep = step;
            if(monitoring) {monitor.print(monitorId,step.toString(symboltable));}}
        return newClause;}

}
