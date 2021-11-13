package Datastructures.Clauses;

import Datastructures.Clauses.QuantifiedToCNF.AtleastToCNF;
import Datastructures.Clauses.QuantifiedToCNF.AtmostToCNF;
import Datastructures.Clauses.QuantifiedToCNF.ExactlyToCNF;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import InferenceSteps.*;
import Management.Monitor;
import Management.ProblemSupervisor;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;


public class ClauseTransformer {
    private final ProblemSupervisor problemSupervisor;
    private final Monitor monitor;
    private final boolean monitoring;
    private final Model model;
    private final Symboltable symboltable;
    private final EquivalenceClasses equivalenceClasses;
    private final boolean trackReasoning;
    private static Method atleastSimpleCases = null;
    private static Method atmostSimpleCases = null;
    private static Method exactlySimpleCases = null;
    static {
        try{atleastSimpleCases = ClauseTransformer.class.getDeclaredMethod("analyseAtleastSimpleCases",
            Clause.class,String.class,Thread.class);
            atmostSimpleCases = ClauseTransformer.class.getDeclaredMethod("analyseAtmostSimpleCases",
                    Clause.class,String.class,Thread.class);
            exactlySimpleCases = ClauseTransformer.class.getDeclaredMethod("analyseExactlySimpleCases",
                Clause.class,String.class,Thread.class);}
        catch(Exception ex) {
            System.out.println(ex.toString());
            System.exit(1);}}

    /** generates a Clause Transformer.
     * It can simplify all clause types and transform them into conjunctive normal form.
     *
     * @param problemSupervisor that supervises the problem solution
     * @param monitor for monitoring the actions.
     */
    public ClauseTransformer(ProblemSupervisor problemSupervisor, Monitor monitor) {
        this.problemSupervisor = problemSupervisor;
        this.monitor = monitor;
        monitoring = monitor != null;
        model = problemSupervisor.model;
        symboltable = model.symboltable;
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
    public Clause analyseAnd(Clause clause, String monitorId, Thread thread) throws Unsatisfiable {
        assert clause.connective == Connective.AND;
        InferenceStep step = null;
        if(trackReasoning) {
            step = new AndToModel(clause);
            if(monitoring) {monitor.print(monitorId,step.toString(symboltable));}}
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
        assert clause.connective == Connective.OR;
        clause = replaceEquivalences(clause,monitorId);
        clause.removeDoubles(); // will not be monitored
        clause = check01InOr(clause,monitorId,thread);
        if(clause == null) return null;

        int size = clause.size();
        for(CLiteral cLiteral : clause) { // check for true literals
            if(model.isTrue(cLiteral.literal)) {
                if(monitor != null) {
                    monitor.print(monitorId,"Clause " + clause.toString(0, symboltable) +
                    " is true in the current model");}
                return null;}} // will be ignored

        for(int i = 0; i < size; ++i) {  // check for tautologies
            int literal = clause.getLiteral(i);
            for(int j = i+1; j < size; ++j) {
                if(literal == -clause.getLiteral(j)) {
                    if(monitor != null) {
                        monitor.print(monitorId,"Clause " + clause.toString(0, symboltable) +
                                " is a tautology");}
                    return null;}}}
        Clause newClause = deleteFalseLiteralsInOr(clause,monitorId);
        return (newClause == clause) ? clause : check01InOr(newClause,monitorId,thread);}

    /** checks if the clause is empty or contains just one literal
     *
     * @param clause a clause to be checked
     * @param thread where the clause is processed
     * @return either null or the unchanged clause
     * @throws Unsatisfiable if a unit clause causes a contradiction
     */
    public Clause check01InOr(Clause clause, String monitorId, Thread thread) throws Unsatisfiable{
        switch(clause.size()) {
            case 0: throw new Unsatisfiable(clause.inferenceStep);
            case 1:
                InferenceStep step = new AndToModel(clause);
                if(monitoring) monitor.print(monitorId,step.toString(symboltable));
                model.add(clause.getLiteral(0),step,thread);
                return null;}
        return clause;}


    /** delete false literals in OR-clauses
     *
     * @param oldClause a clause
     * @param monitorId for monitoring the deletion
     * @return either the unchanged old clause or a new shorter clause.
     */
    public Clause deleteFalseLiteralsInOr(Clause oldClause, String monitorId) {
        assert oldClause.connective == Connective.OR;
        IntArrayList positions = null;
        int size = oldClause.size();
        for(int i = 0; i < size; ++i) {
            if(model.isFalse(oldClause.getLiteral(i))) {
                if(positions == null) positions = new IntArrayList();
                positions.add(i);}}
        if(positions == null) return oldClause;
        Clause newClause = new Clause(problemSupervisor.nextClauseId(), Connective.OR,size-positions.size());
        for(int i = 0; i < size; ++i) {
            if(!positions.contains(i)) newClause.add(oldClause.getLiteral(i));}
        if(trackReasoning) {
            InferenceStep step = new FalseLiteralDeletion(oldClause,newClause,positions,model);
            newClause.inferenceStep = step;
            if(monitoring) monitor.print(monitorId,step.toString(symboltable));}
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
        assert clause.connective == Connective.EQUIV;
        clause = replaceEquivalences(clause,monitorId);
        clause.removeDoubles();             // will not be monitored
        int size = clause.size();
        if(size <= 1) return null;          // can be ignored

        int literal = 0; int status = 0;

        for(int i = 0; i < size; ++i) {        // check for p = -p  is unsatisfiable
            int literal1 = clause.getLiteral(i);
            if(literal == 0) {
                status = model.status(literal1);
                if(status != 0) {literal = literal1;}}
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
                if(monitoring) monitor.print(monitorId,step.toString(symboltable));}
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
        assert clause.connective == Connective.ATLEAST;

        clause = replaceEquivalences(clause,monitorId);
        clause = analyseAtleastSimpleCases(clause,monitorId,thread);
        if(clause == null) return null;
        if(clause.connective != Connective.ATLEAST) return clause;

        // atleast 2 p,-p q -> atleast 1 q
        clause = analysePNotP(clause,monitorId,thread,atleastSimpleCases);
        if(clause == null) return null;

        clause = analyseTrueFalse(clause,monitorId,thread,atleastSimpleCases);

        return clause;}


            /** analyses some simple cases of atleast-clauses
             * - atleast 0 p,q,r is always true<br>
             * - atleast 1 p,q,r is p | q | r<br>
             * - atleast 3 p,q,r is p & q & r
             * - atleast 2 p,p,q -> true(p)
             *
             * @param clause    the clause to be analysed
             * @param monitorId for monitoring the step
             * @param thread    which processes the clause
             * @return null or an or-clause or the original clause
             * @throws Unsatisfiable if a contradiction is detected
             */
    private Clause analyseAtleastSimpleCases(Clause clause, String monitorId, Thread thread) throws Unsatisfiable {
        int quantifier = clause.quAmount;
        if(quantifier == 0) return null; // atleast 0 p,q,r is always true

        if(quantifier == 1) {   // atleast 1 p,q,r is equivalent to p or q or r
            clause.connective = Connective.OR;
            return analyseOr(clause,monitorId,thread);}

        int size = clause.size();

        if(quantifier > size) {      // atleast 3 p,q  is the same as atleast 3 p,q,true is the same as atleast 2 p,q
            clause.quAmount = size;
            quantifier = size;}

        if(quantifier == size) { // atleast 3 p,q,r is equivalent to p & q & r
            Clause andClause = clause.clone(problemSupervisor.nextClauseId());
            andClause.connective = Connective.AND;
            andClause.quAmount = 1;
            if(trackReasoning) {
                InferenceStep step = new AtleastToAnd(clause,andClause);
                andClause.inferenceStep = step;
                if(monitoring) monitor.print(monitorId,step.toString(symboltable));}
            return analyseAnd(andClause,monitorId,thread);}

        if(quantifier == 2) { // atleast 2 pppq  -> p
            int doubleLit = 0;
            for(int i = 0; i < size; ++i) {
                int literal = clause.getLiteral(i);
                for(int j = i+1; j < size; ++j) {if(literal == clause.getLiteral(j)) {doubleLit = literal; break;}}
                if(doubleLit != 0) break;}
            if(doubleLit == 0) return clause;

            int literals = 0;
            for(int i = 0; i < size; ++i) {
                int literal = clause.getLiteral(i);
                if(literal != doubleLit) {
                    if(literals++ == 1) return clause;}}

            InferenceStep step = null;
            if(trackReasoning) {
                step = new AtleastPPQ(clause,doubleLit);
                if(monitoring) monitor.print(monitorId,step.toString(symboltable));}
            model.add(doubleLit,step,thread);
            return null;}
        return clause;
    }



    // ATMOST-CLAUSES

    /** analyses atmost clauses
     * The following typical situations are treated:<br>
     * - equivalent literals are replaced.<br>
     * - atmost 0 p,q,r -> true(-p,-q,-r)<br>
     * - atmost n p_1,...,p_n -> true<br>
     * - atmost 3 p,-p,q,r -> atmost 2 q,r<br>
     * - atmost 2 p,q,r and true(p) -> atmost 1 q,r<br>
     * - atmost 2 p,q,r and false(p) -> atmost 2 q,r<br>
     *
     * Double literals are kept as they are
     *
     * @param clause     the clause to be analysed
     * @param monitorId  for monitoring the steps
     * @param thread     which processes the clause
     * @return           null or an or-clause or a shortened atleast-clause or the original clause
     * @throws Unsatisfiable if a contradiction is detected
     */
    public Clause analyseAtmost(Clause clause, String monitorId, Thread thread) throws Unsatisfiable {
        assert clause.connective == Connective.ATMOST;
        clause = replaceEquivalences(clause, monitorId);
        clause = analyseAtmostSimpleCases(clause,monitorId,thread);
        if(clause == null) return null;
        clause = analysePNotP(clause,monitorId,thread, atmostSimpleCases);
        if(clause == null) return null;
        clause = analyseTrueFalse(clause,monitorId,thread, atmostSimpleCases);
        return clause;
    }
    /** analyses simple cases of atmost clauses
     * The following typical situations are treated:<br>
     * - equivalent literals are replaced.<br>
     * - atmost 0 p,q,r -> true(-p,-q,-r)<br>
     * - atmost n p_1,...,p_n -> true<br>
     * - atmost n p,..,p,Q -> false(p) and atmost n Q  if there are n+1 occurrences of p
     *
     * @param clause     the clause to be analysed
     * @param monitorId  for monitoring the steps
     * @param thread     which processes the clause
     * @return           null or an or-clause or a shortened atleast-clause or the original clause
     * @throws Unsatisfiable if a contradiction is detected
     */
    private Clause analyseAtmostSimpleCases(Clause clause, String monitorId, Thread thread) throws Unsatisfiable {
        int quantifier = clause.quAmount;
        int size = clause.size();
        if(quantifier >= size) return null;  // is always true

        if(quantifier == 0) { // all literals must be false
            InferenceStep step = null;
            if(trackReasoning) {
                step = new AllToModel(clause,-1);
                if(monitoring) monitor.print(monitorId,step.toString(symboltable));}
            for(CLiteral cLiteral : clause) {model.add(-cLiteral.literal,step,thread);}
            return null;}

        if(clause.hasDoubles()) { //atmost n p,..,p,Q -> false(p) and atmost n Q  if there are n+1 occurrences of p
            return analyseMultiples(clause,monitorId,thread,atmostSimpleCases);}
        return clause;}


    // EXACTLY-CLAUSES

    /** analyses exactly clauses
     * The following typical situations are treated:<br>
     * - equivalent literals are replaced.<br>
     * - exactly 0 p,q,r -> true(-p,-q,-r)<br>
     * - exactly n p_1,...,p_n -> true(p_1,...,p_n)<br>
     * - exactly 3 p,-p,q,r -> exactly 2 q,r<br>
     * - exactly 2 p,q,r and true(p) -> exactly 1 q,r<br>
     * - exactly 2 p,q,r and false(p) -> exactly 2 q,r<br>
     *
     * Double literals are kept as they are
     *
     * @param clause     the clause to be analysed
     * @param monitorId  for monitoring the steps
     * @param thread     which processes the clause
     * @return           null or an or-clause or a shortened atleast-clause or the original clause
     * @throws Unsatisfiable if a contradiction is detected
     */
    public Clause analyseExactly(Clause clause, String monitorId, Thread thread) throws Unsatisfiable {
        assert clause.connective == Connective.EXACTLY;
        clause = replaceEquivalences(clause, monitorId);
        clause = analyseExactlySimpleCases(clause,monitorId,thread);
        if(clause == null) return null;
        clause = analysePNotP(clause,monitorId,thread, exactlySimpleCases);
        if(clause == null) return null;
        clause = analyseTrueFalse(clause,monitorId,thread, exactlySimpleCases);
        return clause;
    }

    /** analyses simple cases of exactly clauses
     * The following typical situations are treated:<br>
     * - equivalent literals are replaced.<br>
     * - exactly 3 p,q -> false
     * - exactly 0 p,q,r -> true(-p,-q,-r)<br>
     * - exactly n p_1,...,p_n -> true(p_1,...,p_n)<br>
     * - exactly n p,..,p,Q -> false(p) and exactly n Q  if there are n+1 occurrences of p
     *
     * @param clause     the clause to be analysed
     * @param monitorId  for monitoring the steps
     * @param thread     which processes the clause
     * @return           null or an or-clause or a shortened atleast-clause or the original clause
     * @throws Unsatisfiable if a contradiction is detected
     */
    private Clause analyseExactlySimpleCases(Clause clause, String monitorId, Thread thread) throws Unsatisfiable {
        int quantifier = clause.quAmount;
        int size = clause.size();
        if(quantifier > size) quantifier = size; // exactly 3 p,q is the same as exactly 3 p,q,true
        clause.quAmount = quantifier;          // is the same as exactly 2 p,q

        int sign = 0;
        if(quantifier == 0) {sign = -1;}         // exactly 0 p,q -> false(p,q)
        else {if(quantifier == size) sign = +1;} // exactly 2 p,q -> true(p,q)

        if(sign != 0) {
            InferenceStep step = null;
            if(trackReasoning) {
                step = new AllToModel(clause,sign);
                if(monitoring) monitor.print(monitorId,step.toString(symboltable));}
            for(CLiteral cLiteral : clause) {model.add(sign*cLiteral.literal,step,thread);}
            return null;}

        if(clause.hasDoubles()) { //exactly n p,..,p,Q -> false(p) and atmost n Q  if there are n+1 occurrences of p
            return analyseMultiples(clause,monitorId,thread,exactlySimpleCases);}
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
            if(monitoring) monitor.print(monitorId,step.toString(symboltable));}
        return newClause;}


    /** analyses complementary literals in numeric clauses
     *  atleast 2 p,-p,q,r -> atleast 1 q,r<br>
     *  atmost  2 p,-p,q,r -> atmost  1 q,r<br>
     *  exactly 2 p,-p,q,r -> exactly 1 q,r<br>
     *  simple cases are treated after each removal of complementary literals.
     *
     * @param clause     the clause to be analysed
     * @param monitorId   for monitoring the steps
     * @param thread      which processes the clause
     * @param simpleCases method for treating simple cases
     * @return           null or an or-clause or a shortened clause or the original clause
     */
    private Clause analysePNotP(Clause clause, String monitorId, Thread thread, Method simpleCases) {
        Connective type = clause.connective;
        int size = clause.size();
        for(int i = 0; i < size; ++i) {
            int literal = clause.getLiteral(i);
            for(int j = i+1; j < size; ++j) {
                if(literal == -clause.getLiteral(j)) { // complementary literals detected
                    Clause newClause = new Clause(problemSupervisor.nextClauseId(),type);
                    for(int k = 0; k < size; ++k) {  // add all the non-complementary literals to the new clause
                        if(k != i && k != j) newClause.add(clause.getLiteral(k));}
                    newClause.quAmount = clause.quAmount -1;     // one true pair is eliminated.
                    if(trackReasoning) {
                        InferenceStep step = new NumericPNotP(clause,newClause);
                        newClause.inferenceStep = step;
                        if(monitoring) {monitor.print(monitorId,step.toString(symboltable));}}
                    Clause nextClause = null;
                    try{nextClause = (Clause)simpleCases.invoke(this,newClause,monitorId,thread);}
                    catch(Exception ex) {
                        System.out.println(ex.toString());
                        System.exit(1);} // Method Invocation Error should not happen.
                    if(nextClause == null) return null;
                    if(nextClause.connective != type) return nextClause;
                    // recursive call if several p,-p literals are in the clause
                    return analysePNotP(nextClause,monitorId,thread, simpleCases);}}}
        return clause;}

    /** eliminates true and false literals from numeric clauses
     * atleast 2 p,q,r and true(p) - > atleast 1 q,r
     * atleast 2 p,q,r and false(p) -> atleast 2 q,r
     *
     * @param clause      a numeric clause
     * @param monitorId   for monitoring the action
     * @param thread      which processes the clause
     * @param simpleCases Method for treating simple cases
     * @return            null or a simplified clause or the original clause
     */
    private Clause analyseTrueFalse(Clause clause, String monitorId, Thread thread, Method simpleCases) {

        Connective type = clause.connective;
        int size = clause.size();
        int quantifier = clause.quAmount;

        IntArrayList falseLiterals = null;
        IntArrayList trueLiterals  = null;

        for(int i = 0; i < size; ++i) { // find true and false literals
            int literal = clause.getLiteral(i);
            switch(model.status(literal)) {
                case -1: if(falseLiterals == null) falseLiterals = new IntArrayList();
                    falseLiterals.add(i);
                    break;
                case +1: if(trueLiterals == null) trueLiterals = new IntArrayList();
                    trueLiterals.add(i);}}

        if(falseLiterals != null || trueLiterals != null) {
            Clause newClause = new Clause(problemSupervisor.nextClauseId(), type);
            for (int i = 0; i < size; ++i) { // add the literals which have to truth value.
                int literal = clause.getLiteral(i);
                if(model.status(literal) == 0) newClause.add(literal);}

            newClause.quAmount = quantifier;
            if (trueLiterals != null) newClause.quAmount -= trueLiterals.size();

            if (trackReasoning) {
                InferenceStep  step = new NumericTrueFalse(clause, newClause, trueLiterals, falseLiterals, model);
                newClause.inferenceStep = step;
                if(monitoring) monitor.print(monitorId, step.toString(symboltable));}
            try{newClause = (Clause)simpleCases.invoke(this,newClause,monitorId,thread);}
            catch(Exception ex) {
                System.out.println(ex.toString());
                System.exit(1);}
            return newClause;}
        return clause;}

    /** simplifies some multiple occurrences of literals.
     * Examples: <br>
     * - atmost  2 ppp,q,r -> false(p) and atmost  2 q,r<br>
     * - exactly 2 ppp,q,r -> false(p) and exactly 2 q,r<br>
     * This is not possible with atleast-clauses
     *   atleast 2 ppp,q,r -> true(p) OR atleast 2 q,r
     *
     * @param clause      a atmost- or exactly clause
     * @param monitorId   for monitoring the action
     * @param thread      which processes the clause
     * @param simpleCases for treating simple cases
     * @return            null or the smaller clause or the original clause
     * @throws Unsatisfiable if a contradiction is discovered
     */
    private Clause analyseMultiples(Clause clause, String monitorId, Thread thread, Method simpleCases) throws Unsatisfiable {
        assert clause.connective == Connective.ATMOST || clause.connective == Connective.EXACTLY;

        int quantifier = clause.quAmount;
        HashMap<Integer,Integer> occurrences = new HashMap<>();

        // we collect the number of occurrences of the literals
        for(CLiteral cLiteral : clause) {
            int literal = cLiteral.literal;
            occurrences.merge(literal, 1, Integer::sum);
        }
        IntArrayList literals = new IntArrayList();
        occurrences.forEach((literal,occurrence) -> {if(occurrence > quantifier) literals.add((int)literal);});
        if(literals.isEmpty()) return clause;  // no simplification possible

        // now literals contains the literals with too many occurrences
        Clause newClause = new Clause(problemSupervisor.nextClauseId(),clause.connective);
        newClause.quAmount = quantifier;
        InferenceStep step = null;
        for(CLiteral cLiteral : clause) {
            int literal = cLiteral.literal;
            if(!literals.contains(literal)) newClause.add(literal);}
        if(trackReasoning) {
            step = new NumericMultipleP(clause,newClause,literals);
            newClause.inferenceStep = step;
            if(monitoring) monitor.print(monitorId,step.toString());}
        for(int literal : literals) {model.add(-literal,step,thread);}

        try{newClause = (Clause)simpleCases.invoke(this,newClause,monitorId,thread);}
        catch(Exception ex) {
            System.out.println(ex.toString());
            System.exit(1);}
        return newClause;}

}
