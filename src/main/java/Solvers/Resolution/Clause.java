package Solvers.Resolution;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import Solvers.Normalizer.Normalizer;
import Utilities.BiConsumerWithUnsatisfiable;
import Utilities.ByteFunction;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.IntSupplier;

/** A Clause object is essentially a collection of Literal objects.
 *  A clause can only be a disjunction (OR-clause) or an ATLEAST-clause.<br>
 *  Other types are not supported.<br>
 *  Each Clause object may be part of a doubly connected list of clauses. <br>
 *  Clauses can be destructively changed.<br>
 *  Clause are identified by an identifier and possibly a sub-identifier.<br>
 *  This allows one to transform interval-type clauses to several clauses, while keeping
 *  the identifier of the original clause.<br>
 *  A sub-identifier 0 is ignored.
 */
public class Clause {
    /** the identifier for the clause. */
    final int identifier;

    /** another identifier for clauses which originated from interval-type clauses */
    int subIdentifier = 0;

    /** the quantifier, OR or ATLEAST. */
    Quantifier quantifier;

    /** true if the quantifier is OR. */
    boolean isDisjunction;

    /** the minimum quantification for ATLEAST clauses. */
    int limit = 1;

    /** the sum of all multiplicities of the literals. */
    int expandedSize = 0;

    /** the list of all Literal objects in the clause. */
    ArrayList<Literal> literals;

    /** characterises the distribution of positive and negative literals in a clause. */
    ClauseType clauseType;

    /** flag to indicate that the clause still exists. */
    boolean exists = true;

    /** true if there are literals with multiplicities &gt; 1. */
    boolean hasMultiplicities = false;

    /** the inference step which caused the derivation of this clause. */
    ArrayList<InferenceStep> inferenceSteps;

    /** the task the clause is involved in */
    Task task;

    /** a timestamp to be used by various algorithms. */
    IntArrayList timestamps = new IntArrayList(3);

    /** a timestamp to be used by subsumption checks. */
    int timestampSubsumption = 0;

    /** a pointer to the previous clause in a doubly connected list. */
    Clause previousClause;

    /** a pointer to the next clause in a doubly connected list. */
    Clause nextClause;

    /** The constructor turns an InputClause int[]-array into a Clause object.
     * <br>
     * The constructor is used for testing purposes.
     *
     * @param inputClause InputClause int[]-array.
     */
    public Clause(int[] inputClause) {
        identifier = inputClause[0];
        quantifier = Quantifier.getQuantifier(inputClause[1]);
        assert(quantifier != null && (quantifier == Quantifier.OR || quantifier == Quantifier.ATLEAST));
        isDisjunction = quantifier == Quantifier.OR;
        limit = isDisjunction ? 1 : inputClause[2];
        int length = inputClause.length;
        int start = quantifier.firstLiteralIndex;
        literals = new ArrayList<>(length-start);
        timestamps.add(0);
        if(isDisjunction)
            for(int i = start; i < length; ++i) {
                int literal = inputClause[i];
                boolean found = false;
                for(int j = i+1; j < length; ++j) {if(literal == inputClause[j]) {found = true; break;}}
                if(found) continue; // double literal. The second one will be recorded.
                Literal literalObject = new Literal(literal,1);
                literalObject.clause = this;
                ++expandedSize;
                literals.add(literalObject);}
        else {
            inputClause = Arrays.copyOf(inputClause,length);
            for(int i = start; i < length; ++i) {
                int literal1 = inputClause[i];
                if(literal1 == 0) continue;
                int multiplicity = 1;
                for(int j = i+1; j < length; ++j) {
                    if(literal1 == inputClause[j]) {
                        ++multiplicity;
                        inputClause[j] = 0;}}
                expandedSize += multiplicity;
                Literal literalObject = new Literal(literal1,multiplicity);
                literalObject.clause = this;
                literals.add(literalObject);}}
        determineClauseType();
        hasMultiplicities = expandedSize > literals.size();}

    /** This is a constructor for a disjunction.
     * <br>
     * It is in particular used for constructing 2-literal clauses, which, when simplified, are always disjunctions.
     *
     * @param identifier             the identifier.
     * @param literalNumbers the literals of the disjunction.
     */
    public Clause(int identifier, int... literalNumbers) {
        this.identifier = identifier;
        quantifier = Quantifier.OR;
        isDisjunction = true;
        expandedSize = literalNumbers.length;
        literals = new ArrayList<>(expandedSize);
        for(int literal : literalNumbers) {
            Literal literalObject = new Literal(literal,1);
            literalObject.clause = this;
            literals.add(literalObject);}
        determineClauseType();}

    /** constructs a new clause from previously constructed Literals.
     * <br>
     * The literal's multiplicities are automatically reduced to 'limit'.
     * This constructor is used for constructing new resolvents.
     *
     * @param identifier the identifier for the clause.
     * @param quantifier the quantifier.
     * @param limit      the limit for the quantifier.
     * @param literals   the literals.
     */
    public Clause(int identifier, Quantifier quantifier, int limit, ArrayList<Literal> literals) {
        this.identifier = identifier;
        this.quantifier = quantifier;
        isDisjunction   = quantifier == Quantifier.OR;
        this.limit = limit;
        this.literals = literals;
        for(Literal literalObject: literals) {
            literalObject.clause = this;
            literalObject.multiplicity = Math.min(limit,literalObject.multiplicity);
            expandedSize += literalObject.multiplicity;}
        hasMultiplicities = expandedSize > literals.size();
        determineClauseType();}


    /** this constructor turns a normalizedClause to a clause for the Resolution solver.
     * <br>
     * A normalizedClause is in interval-normalform.<br>
     * Therefor this constructor works only for OR- and ATLEAST-clauses.<br>
     * The structure of the normalizedClause is: <br>
     * identifier, quantifier, min, max, expandedSize, literal1, multiplicity1,...
     *
     * @param normalizedClause a normalized and simplified clause from the Normalizer.
     * @param subIdentifier a secondary identifier for multiple clauses resulting from interval-type clauses.
     *                      subIdentifier = 0 will be ignored.
     */
    public Clause(IntArrayList normalizedClause, int subIdentifier) {
        identifier         = normalizedClause.getInt(0);
        this.subIdentifier = subIdentifier;
        quantifier         = Normalizer.getQuantifier(normalizedClause);
        assert quantifier == Quantifier.OR || quantifier == Quantifier.ATLEAST;
        isDisjunction      = quantifier == Quantifier.OR;
        limit              = Normalizer.getMin(normalizedClause);
        expandedSize       = Normalizer.getExpandedSize(normalizedClause);
        hasMultiplicities  = Normalizer.hasMultiplicities(normalizedClause);
        literals           = new ArrayList<>(expandedSize);
        for(int i = Normalizer.literalsStart; i <= normalizedClause.size()-2; i +=2) {
            Literal literal = new Literal(normalizedClause.getInt(i),normalizedClause.getInt(i+1));
            literals.add(literal);
            literal.clause = this;}
        determineClauseType();}

    /** adds a new inference step to the list of inference steps.
     *
     * @param step an inference step.
     */
    void addInferenceStep(InferenceStep step) {
        if(inferenceSteps == null) inferenceSteps = new ArrayList<>();
        inferenceSteps.add(step);}


    /** finds the Literal with the given literal.
     *
     * @param literal a literal.
     * @return null or a Literal with the given literal.
     */
    Literal findLiteral(final int literal) {
        for(final Literal literalObject : literals) {
            if(literalObject.literal == literal) return literalObject;}
        return null;}

    /** tests if the given literal is contained in the clause's literals
     *
     * @param literal a literal
     * @return +1 (literal is contained), -1 (-literal is contained), 0 (literal is not contained)
     */
    byte contains(final int literal) {
        for(final Literal literalObject : literals) {
            int lit = literalObject.literal;
            if(lit ==  literal) return  1;
            if(lit == -literal) return -1;}
        return 0;}

    /** returns for a three-literal clause the third literal.
     *
     * @param literal1 a literal in the clause.
     * @param literal2 a literal in the clause.
     * @return null or the third literal in the clause.
     */
    Literal findThirdLiteral(final int literal1, final int literal2) {
        assert size() == 3;
        for(final Literal literalObject : literals) {
            int literal = literalObject.literal;
            if(literal != literal1 && literal != literal2)
                return literalObject;}
        return null;}

    /** returns for a two-literal clause the other literal.
     *
     * @param literalObject one of the clause's literalObjects.
     * @return the other literalObject.
     */
    protected Literal otherLiteral(final Literal literalObject) {
        assert(literals.size() == 2);
        return literalObject == literals.get(0) ? literals.get(1) : literals.get(0);}




    /** removes all literals where modelStatus != 0.
     * <br>
     * modelStatus == 1 means the literal is true, and therefor the limit has to be reduced by the literal's multiplicity.<br>
     * modelStatus == -1 means the literal is false and can just be removed.<br>
     * - all possible simplifications are done. <br>
     * - all superfluous literals are removed.<br>
     *   Example: &gt;= 2 p,q^2,r^2,s and false(p) -> &gt;= 2 q^2,r^2,s -> q,r <br>
     * - for all derivable literals, trueLiterals is called.<br>
     *   Example:  &gt;= 2 p^2,q,r. and false(q) -> &gt;= 2 p^2,r -> p must be true.<br>
     * If the resulting clause is a unit clause, trueLiterals is applied to it.
     * The result +1 may mean: tautology, or unit clause with removed literal.
     * <br>
     * The remover is applied to all removed literals. It may, for example, remove the literals from the index.
     *
     * @param modelStatus  returns +1 (true), -1 (false) or 0 (undefined).
     * @param stepFunction to be applied to the clauseBefore and clauseAfter and returns the actual InferenceStep.
     * @param remover      is applied to the removed literals.
     * @param symboltable  null or the symboltable.
     * @param monitor      null or a monitor function.
     * @param trueLiterals is applied to all derivable true literals.
     * @return +1 (not necessary any more), -1 (unsatisfiable), 0 undefined.
     * @throws Unsatisfiable if a contradiction is found.
     */
    byte removeLiterals(ByteFunction<Literal> modelStatus, BiFunction<String,String,InferenceStep> stepFunction, Consumer<Literal> remover,
                        BiConsumerWithUnsatisfiable<Integer,InferenceStep> trueLiterals,
                        Symboltable symboltable, Consumer<String> monitor) throws Unsatisfiable {
        for(int i = 0; i < literals.size(); ++i) {
            final Literal literalObject = literals.get(i);
            final int status = modelStatus.apply(literalObject);
            if(status == 0) continue;
            String clauseBefore = (stepFunction != null || monitor != null) ? toString(symboltable,0) : null;
            literalObject.clause = null;
            if(remover != null) remover.accept(literalObject);
            literals.remove(i--);
            expandedSize -= literalObject.multiplicity;
            if(status == 1) limit -= literalObject.multiplicity;
            InferenceStep step = (stepFunction != null) ? stepFunction.apply(clauseBefore,toString(symboltable,0)) : null;
            if(step != null) {inferenceSteps.add(step);
                              if(monitor != null ) monitor.accept(step.toString(symboltable));};
            if(limit <= 0) { // tautology
                if(monitor != null) monitor.accept("removeLiterals: clause" + toString(symboltable,0) + "became a tautology.");
                if(remover != null) for(Literal litObject : literals) remover.accept(litObject);
                literals.clear();
                return 1;}}

        if(expandedSize < limit) {return -1;}

        if(literals.size() == 1) { // unit clause, must be true.
            InfUnitClause step = (inferenceSteps == null && monitor == null) ? null : new InfUnitClause(this);
            if(inferenceSteps != null) inferenceSteps.add(step);
            trueLiterals.accept(literals.get(0).literal,step); // may throw Unsatisfiable
            if(remover != null) remover.accept(literals.get(0));
            return 1;}

        if(isDisjunction) { // clause was a disjunction and is still a disjunction.
            determineClauseType();
            return (expandedSize < limit) ? (byte)-1: 0;} // could be the empty clause

        if(limit == 1) { // clause turned into a disjunction
            reduceToDisjunction();
            determineClauseType();
            return (expandedSize < limit) ? (byte)-1: 0;}

        // In the remaining literals some of them may not have enough literals to satisfy the limit.
        // Example:  >= 2 p^2,q,r. and false(q) -> >= 2 p^2,r -> p must be true.
        if(!reduceByTrueLiterals(remover,trueLiterals, symboltable,monitor)) return 1;

        // Example: >= 2 p,q^2,r^2,s and false(p) -> >= 2 q^2,r^2,s -> q,r
        if(reduceToEssentialLiterals(remover,trueLiterals,symboltable, monitor)) {
                divideByGCD(symboltable, monitor);}
        return 0;}

    /** removes the literal from the clause and simplifies the resulting clause.
     *
     * @param literal       a literal
     * @param inferenceStep null or the inference step which causes the removal of the literal.
     * @param remover       to be applied to removed literals.
     * @param trueLiterals  to be applied if some true literals can be derived.
     * @param symboltable   null or a symboltable.
     * @param monitor       null or a function to be applied to a string.
     * @return              +1 (removed), -1 (unsatisfiable), 0 undefined.
     * @throws Unsatisfiable
     */
    byte removeLiteral(int literal, InferenceStep inferenceStep, Consumer<Literal> remover,
                       BiConsumerWithUnsatisfiable<Integer,InferenceStep> trueLiterals, Symboltable symboltable,
                       Consumer<String> monitor) throws Unsatisfiable {
        return removeLiterals((literalObject -> (literalObject.literal == literal)? (byte)-1 : 0),
                              inferenceStep == null ? null : (literalObject -> (literalObject.literal == literal) ? inferenceStep : null),
                remover,trueLiterals,symboltable,monitor);}

    /** reduces the literals to the essential literals.
         * <br>
         * Example: &gt;= 2 q^2,r^2,s -> p,q <br>
         * If such a reduction is possible then the clause becomes a disjunction.<br>
         * The literal's size shrinks if a reduction was achieved.<br>
         * The remover is applied to all removed literals.<br>
         * If a literal becomes true (e.g. unit clause), the trueLiterals is applied to the single literal,
         * and the remover is applied to the literal.
         *
         * @param remover   applied to removed literals
         * @param trueLiterals applied to a remaining single literal (must be true).
         * @return true if the clause has not been changed.
         */
     boolean reduceToEssentialLiterals(Consumer<Literal> remover, BiConsumerWithUnsatisfiable<Integer,InferenceStep> trueLiterals,
                                   Symboltable symboltable, Consumer<String> monitor) throws Unsatisfiable{
        int remainingMultiplicity = 0;
        for(Literal literalObject : literals) {
            literalObject.multiplicity = Math.min(limit,literalObject.multiplicity);
            if(literalObject.multiplicity < limit) remainingMultiplicity += literalObject.multiplicity;}

        if(remainingMultiplicity < limit) { // the literals with together multiplicity < limit are not enough.
                           // one of the literals with multiplicity == limit must be true.
            String clauseBefore = (monitor != null || inferenceSteps != null) ? toString(symboltable,0) : null;
            for(int i = 0; i < literals.size(); ++i) {
                Literal literalObject = literals.get(i);
                if(literalObject.multiplicity < limit) { // the literals with multiplicity < limit are removed.
                    if(remover != null) remover.accept(literalObject);
                    literals.remove(i--);}}
            reduceToDisjunction();
            InferenceStep step = (monitor != null || inferenceSteps != null) ?
                    new InfReduceToEssentialLiterals(clauseBefore,this,symboltable) : null;
            if(inferenceSteps != null) inferenceSteps.add(step);
            if(monitor != null) monitor.accept(step.toString(symboltable));
            if(literals.size() == 1) {
                step = (monitor != null || inferenceSteps != null) ?
                        new InfUnitClause(this): null;
                if(monitor != null) monitor.accept(step.toString(symboltable));
                trueLiterals.accept(literals.get(0).literal,step);
                if(remover != null) remover.accept(literals.get(0));}
            return false;}
        return true;} // nothing changed

    /** to be used by divideByGCD. */
    private final IntArrayList numbers = new IntArrayList();

    /** divides the limit and the multiplicities by their greatest common divisor.
     *
     * @return true if the clause is changed.
     */
    protected boolean divideByGCD(Symboltable symboltable, Consumer<String> monitor) {
        numbers.clear(); numbers.add(limit);
        boolean stop = false;
        for(final Literal literalObject : literals) {
            int multiplicity = literalObject.multiplicity;
            if(multiplicity == 1) {stop = true; break;}
            numbers.add(multiplicity);}
        if(!stop) {
            int gcd = Utilities.gcd(numbers);
            if(gcd > 1) {
                String clauseBefore = (inferenceSteps == null) ? null : toString(symboltable,0);
                expandedSize = 0;
                limit /= gcd;
                for(final Literal literalObject : literals) {
                    literalObject.multiplicity = Math.min(limit, literalObject.multiplicity / gcd);
                    expandedSize += literalObject.multiplicity;}
                if(limit == 1) {quantifier = Quantifier.OR; isDisjunction = true;}
                InferenceStep step = (inferenceSteps != null || monitor != null) ?
                        new InfGCDReduction(gcd, clauseBefore, toString(symboltable,0)) : null;
                if(inferenceSteps != null) {inferenceSteps.add(step);}
                if(monitor != null) monitor.accept(step.toString(symboltable));
            return true;}}
        return false;}


    /** Tries to find true literals in a list of literals.
     * <br>
     * Example:  &gt;= 3 p,q,r -> all literals must be true<br>
     * Example:  &gt;= 2 p^2,r -> p must be true.<br>
     * True literals are removed and the trueLiterals consumer is applied.<br>
     * If there is still one literal left, it is also made true.<br>
     * If the clause became true, all literals are removed.
     *
     * @param remover       is applied to the removed literalObjects.
     * @param trueLiterals  is applied to the true literals.
     * @return              true if the clause still exists and is not a unit clause.
     */
    boolean reduceByTrueLiterals(final Consumer<Literal> remover, final BiConsumerWithUnsatisfiable<Integer,InferenceStep> trueLiterals,
                             Symboltable symboltable, Consumer<String> monitor) throws Unsatisfiable {
        String clauseBefore = (inferenceSteps == null && monitor == null) ? null : toString(symboltable,0);
        InfTrueLiteralReduction step = null;
         for(int i = 0; i < literals.size(); ++i) {
            Literal literalObject = literals.get(i);
            if(expandedSize - literalObject.multiplicity < limit) {
                int literal = literalObject.literal;
                if(monitor != null)
                    monitor.accept("reduceByTrueLiterals: Removing true literal " +
                        Symboltable.toString(literal,symboltable) + " from  clause" + clauseBefore);
                if(inferenceSteps != null && step == null) step = new InfTrueLiteralReduction(clauseBefore);
                if(step != null) step.addLiteral(literal);
                if(remover != null) remover.accept(literalObject);
                trueLiterals.accept(literal,
                        step == null ? null : new InfTrueLiteralReduction(clauseBefore,literal));  // The literal must be true.
                literals.remove(i--);
                limit -= literalObject.multiplicity;
                expandedSize -= literalObject.multiplicity;
                if(limit <= 0) {
                    if(monitor != null)
                        monitor.accept( "reduceByTrueLiterals: limit " + limit + " < 0 after removing true literals from clause " + clauseBefore);
                    if(remover != null) {for(Literal litObject : literals) remover.accept(litObject);}
                    literals.clear();
                    return false;}}}
            if(literals.size() == 1) { // unit clause, must be true.
                if(monitor != null)
                    monitor.accept("reduceByTrueLiterals: clause " + clauseBefore + " becomes a unit clause after removing true literals");
                trueLiterals.accept(literals.get(0).literal,((step == null) ? null : new InfUnitClause(this)));
                if(remover != null) remover.accept(literals.get(0));
                return false;}
            if(step != null) {
                step.setClauseAfter(toString(symboltable,0));
                inferenceSteps.add(step);
                if(monitor != null) monitor.accept(step.toString(symboltable));}
        return true;}


    /** reduces the clause to a disjunction.
     * <br>
     * All multiplicities are set to 1.<br>
     * expandedSize = size.<br>
     * hasMultiplicities = false.<br>
     * quantifier = or.
     */
    protected void reduceToDisjunction() {
        limit = 1;
        for(final Literal literalObject : literals) literalObject.multiplicity = 1;
        expandedSize = literals.size();
        isDisjunction = true;
        hasMultiplicities = false;
        quantifier = Quantifier.OR;}


    /** creates a resolvent for two clauses and performs all possible simplifications.
     * <br>
     * - multiple occurrences of literals are merged into one occurrence.<br>
     * - complementary literals are removed.<br>
     * - derivable true literals are identified and removed. <br>
     *    Example: -p,q and atleast 2 p,q,r  ->  atleast 2 q^2,r.  q must be true.<br>
     * - literals are reduces to essential literals: <br>
     *    Example: -p,q and atleast 2 p,q,r^2,s  -> atleast 2 q^2,r^2,s  ->  p,q.
     *
     * @param literalObject1  the first parent literal.
     * @param literalObject2  the second parent literal.
     * @param id              for generating a new identifier.
     * @param trueLiterals    applied to derived true literals.
     * @param symboltable     null or the symboltable.
     * @param monitor         null or a monitor function.
     * @return                null if the resolvent is a tautology, otherwise the new resolvent.
     */
    static Clause resolve(final Literal literalObject1, final Literal literalObject2, final IntSupplier id,
                          final BiConsumerWithUnsatisfiable<Integer,InferenceStep> trueLiterals,
                          Symboltable symboltable, Consumer<String> monitor) throws Unsatisfiable{
        assert literalObject1.literal == -literalObject2.literal;
        Clause clause1 = literalObject1.clause;
        Clause clause2 = literalObject2.clause;
        int newLimit = clause1.limit + clause2.limit - Math.max(literalObject1.multiplicity,literalObject2.multiplicity);
        ArrayList<Literal> newLiterals = new ArrayList<>(clause1.literals.size() + clause2.literals.size()-2);
        int expandedSize = 0;
        for(final Literal litObject1 : clause1.literals) {
            if(litObject1 == literalObject1) continue;
            expandedSize += litObject1.multiplicity;
            newLiterals.add(new Literal(litObject1.literal,litObject1.multiplicity));}

        for(final Literal litObject2 : clause2.literals) {
            if(litObject2 == literalObject2) continue;
            int multiplicity2 = litObject2.multiplicity;
            boolean found = false;
            for(int i = 0; i < newLiterals.size(); ++i) {
                Literal litObject1  = newLiterals.get(i);
                int multiplicity1 = litObject1.multiplicity;
                if(litObject1.literal == litObject2.literal) {
                    found = true;
                    expandedSize -= litObject1.multiplicity;
                    litObject1.multiplicity = Math.min(newLimit, multiplicity1 + multiplicity2);
                    expandedSize += litObject1.multiplicity;
                    break;} // litObject2 is not necessary
                if(litObject1.literal == -litObject2.literal) {
                    found = true;
                    if(multiplicity1 == multiplicity2) {
                        newLimit -= multiplicity1;
                        newLiterals.remove(i);} // p,-p can both be removed.
                    else {
                        if(multiplicity1 > multiplicity2) {
                            litObject1.multiplicity -= multiplicity2;
                            newLimit -= multiplicity2;
                            expandedSize -= multiplicity2;} // we keep the old literal
                        else {
                            expandedSize -= litObject1.multiplicity;
                            litObject1.multiplicity = multiplicity2 - multiplicity1;
                            newLimit -= multiplicity1;
                            expandedSize += litObject1.multiplicity;
                            litObject1.literal *= -1;}}  // we keep the negated old literal
                    if(newLimit <= 0) return null; // tautology
                    break;}}
            if(!found) {
                newLiterals.add(new Literal(litObject2.literal,litObject2.multiplicity));
                expandedSize += litObject2.multiplicity;}}

        assert (expandedSize >= newLimit);
        Clause resolvent = new Clause(id.getAsInt(), (newLiterals.size() == expandedSize)? Quantifier.ATLEAST: Quantifier.OR,
                newLimit,newLiterals);
        InferenceStep step = (monitor != null || clause1.inferenceSteps != null || clause2.inferenceSteps != null) ?
                new InfResolution(clause1,clause2,resolvent,symboltable,null) : null;
        if(clause1.inferenceSteps != null || clause2.inferenceSteps != null) {
            resolvent.inferenceSteps = new ArrayList<>();
            resolvent.inferenceSteps.add(step);}
        if(monitor != null) monitor.accept(step.toString(symboltable));
        if(newLiterals.size() == 1) {
            trueLiterals.accept(newLiterals.get(0).literal,step); return null;} // unit clause
        if(newLimit == 1) return resolvent;

        if(!resolvent.reduceByTrueLiterals(null, trueLiterals,symboltable, monitor)) return resolvent;

        if(resolvent.reduceToEssentialLiterals(null,trueLiterals, symboltable,monitor) &&
            resolvent.quantifier == Quantifier.ATLEAST)resolvent.divideByGCD(symboltable,monitor);
        return resolvent;}

    /** replaces the oldLiteralObject by a new literal (typically equivalence replacement).
     * <br>
     * The method checks if the new literal or its negation is already in the clause.<br>
     * Superfluous literals are removed from the clause.<br>
     * The changed clause is simplified.
     *
     * @param oldLiteralObject  a oldLiteralObject of some clause.
     * @param newLiteral     a literal (may already be in the clause).
     * @param remover        applied to removed literals.
     * @param adder          applied to newly added literals.
     * @param trueLiterals   applied to derivable true literals.
     * @return               1 if the clause has become superfluous (e.g. tautology), 0 if it survived.
     */
    int replaceLiteral(Literal oldLiteralObject, int newLiteral,
                              Consumer<Literal> remover, Consumer<Literal> adder,
                              InferenceStep infEquivalence,
                              BiConsumerWithUnsatisfiable<Integer,InferenceStep> trueLiterals,
                              Symboltable symboltable, Consumer<String> monitor) throws Unsatisfiable{
        BiFunction<String,String,InferenceStep> stepFunction = (inferenceSteps == null) ? null :
                ((clauseBefore,clauseAfter) -> new InfEquivalenceReplacement(infEquivalence,clauseBefore,clauseAfter));
        Literal newLiteralObject = findLiteral(newLiteral);
        if(newLiteralObject != null) { // the newLiteral merges into oldLiteralObject:  p,q and p == q -> p^2
            int newMultiplicity = newLiteralObject.multiplicity;
            oldLiteralObject.multiplicity = Math.min(limit, newLiteralObject.multiplicity + oldLiteralObject.multiplicity);
            expandedSize +=  newLiteralObject.multiplicity - newMultiplicity;
            return removeLiterals((l -> (l == newLiteralObject) ? (byte)-1:0),stepFunction,remover,trueLiterals,symboltable,monitor);}
        else {
            final Literal negNewLiteralObject = findLiteral(-newLiteral);
            if(negNewLiteralObject != null) { // could be tautology  p^n,-q^m and p == q -> p^(n-m)
                if(negNewLiteralObject.multiplicity == oldLiteralObject.multiplicity) { // is tautology: n == m
                    if(monitor != null) monitor.accept("replaceLiteral: Replacing " +
                            Symboltable.toString(oldLiteralObject.literal,symboltable) + " by " + Symboltable.toString(newLiteral,symboltable) +
                    " in clause " + toString(symboltable,0) + " yields a tautology");
                    for(Literal litObject : literals) remover.accept(litObject);
                    return 1;}
                else { // n != m
                    if(negNewLiteralObject.multiplicity > oldLiteralObject.multiplicity) { // Example; p^3 -q^5 (q -> p)  -> q^2
                        negNewLiteralObject.multiplicity -= oldLiteralObject.multiplicity;
                        expandedSize -= oldLiteralObject.multiplicity;
                        return removeLiterals((l -> (l == oldLiteralObject) ? (byte)-1:0), stepFunction, remover,trueLiterals,symboltable,monitor);}
                    else {oldLiteralObject.multiplicity -= negNewLiteralObject.multiplicity; // Example; -p^3 q^5 (q -> p)  -> p^2
                        expandedSize -= negNewLiteralObject.multiplicity;
                        remover.accept(oldLiteralObject);
                        final Literal newLitObject = new Literal(newLiteral,oldLiteralObject.multiplicity);
                        newLitObject.clause = this;
                        exchangeLiterals(oldLiteralObject,newLitObject);
                        adder.accept(newLitObject);
                        return removeLiterals(((Literal l) -> (l == negNewLiteralObject) ? (byte)-1:0),stepFunction,remover,trueLiterals,symboltable,monitor);}}}}
        remover.accept(oldLiteralObject);
        String clauseBefore = (stepFunction == null) ? null : toString(symboltable,0);
        final Literal newLitObject = new Literal(newLiteral,oldLiteralObject.multiplicity);
        newLitObject.clause = this;
        exchangeLiterals(oldLiteralObject,newLitObject);
        adder.accept(newLitObject);
        determineClauseType();
        if(stepFunction != null) {
            InferenceStep step = stepFunction.apply(clauseBefore,toString(symboltable,0));
            inferenceSteps.add(step);
            if(monitor != null ) monitor.accept(step.toString(symboltable));};
        return 0;}

    /** exchanges the oldLiteral with the newLiteral.
     *
     * @param oldLiteral a literalObject in the clause.
     * @param newLiteral a new literalObject.
     */
    private void exchangeLiterals(final Literal oldLiteral, final Literal newLiteral) {
        for(int i = 0; i < literals.size(); ++i) {
            if(literals.get(i) == oldLiteral) {literals.set(i,newLiteral); return;}}}

    /** investigates the distribution of positive and negative literals and determines the ClauseType.*/
    void determineClauseType() {
       int positiveLiterals = 0; int negativeLiterals = 0;
       for(final Literal literalObject : literals) {
           if(literalObject.literal > 0) positiveLiterals += literalObject.multiplicity;
           else negativeLiterals += literalObject.multiplicity; }
       if(positiveLiterals == expandedSize) {clauseType = ClauseType.POSITIVE; return;}
       if(negativeLiterals == expandedSize) {clauseType = ClauseType.NEGATIVE; return;}
       if(positiveLiterals >= limit && negativeLiterals >= limit) {clauseType = ClauseType.POSITIVENEGATIVE; return;}
       if(positiveLiterals >= limit) {clauseType = ClauseType.MIXEDPOSITIVE; return;}
       if(negativeLiterals >= limit) {clauseType = ClauseType.MIXEDNEGATIVE; return;}
       clauseType = ClauseType.MIXEDMIXED;}

    /** returns the number of Literal objects in the clause.
     *
     * @return the number of Literal objects in the clause.
     */
    public int size() {return literals.size();}

    /** returns either the identifier itself or identifier.subidentifier.
     *
     * @return either the identifier itself or identifier.subidentifier.
     */
    public String identifier() {
        return (subIdentifier == 0) ? Integer.toString(identifier) : identifier+"."+subIdentifier;
    }

    /** returns the sum of the literal's multiplicities.
     *
     * @return the sum of the literal's multiplicities.
     */
    public int expandedSize() {return expandedSize;}

    /** sets the timestamp at the given level to the given amount.
     *
     * @param timestampLevel the resursion level
     * @param timestamp the timestamp.
     */
    public void setTimestamp(int timestampLevel, int timestamp) {
        if(timestamps.size() <= timestampLevel) timestamps.add(timestamp);
        else timestamps.set(timestampLevel,timestamp);}


    /** increases the timestamp at the given level.
     *
     * @param timestampLevel the resursion level
     * @return the increased timestamp.
     */
    public int increaseTimestamp(int timestampLevel) {
        if(timestamps.size() <= timestampLevel) {timestamps.add(1); return 1;}
        else {int timestamp = timestamps.getInt(timestampLevel)+1;
            timestamps.set(timestampLevel,timestamp);
            return timestamp;}}


    /** returns the timestamp at the given level.
     *
     * @param timestampLevel the recursion level of the timestamp
     * @return the timestamp at the given level.
     */
    public int getTimestamp(int timestampLevel) {
        if(timestamps.size() <= timestampLevel) {timestamps.add(1); return 1;}
        else return timestamps.getInt(timestampLevel);}

    /** checks if the timestamp at the given recursion level equals the timestamp.
     *
     * @param timestampLevel the recursion level
     * @param timestamp an integer
     * @return true if the timestamp at the given level equals the given timestamp.
     */
    public boolean eqTimestamp(int timestampLevel, int timestamp) {
        return timestamps.getInt(timestampLevel) == timestamp;}

    /** turns the clause into a string
     *
     * @return a string representation of the clause.
     */
    public String toString() {return toString(null,0);}

    /** turns the clause into a string.
     *
     * @param symboltable null or a symboltable.
     * @param size 0 or the length of the clause number string.
     * @return a string representation of the clause.
     */
    public String toString(final Symboltable symboltable, final int size) {
        StringBuilder st = new StringBuilder();
        String id = Integer.toString(identifier);
        if(subIdentifier != 0) id += "."+subIdentifier;
        st.append((size == 0) ? id : String.format("%"+size+"s", id)).append(": ");
        if(quantifier == Quantifier.ATLEAST) st.append(">= ").append(limit).append(" ");
        if(literals.size() > 0) {
            int length = literals.size()-1;
            for(int i = 0; i < length; ++i) {
                st.append(literals.get(i).toString(symboltable)).append(quantifier.separator);}
            st.append(literals.get(length).toString(symboltable));}
        return st.toString();}

}
