package Solvers.Resolution;

import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import Solvers.Normalizer.Normalizer;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.IntConsumer;
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
    int identifier;

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
    ArrayList<Literal> literals = new ArrayList<>();

    /** characterises the distribution of positive and negative literals in a clause. */
    ClauseType clauseType;

    /** flag to indicate that the clause still exists. */
    boolean exists = true;

    /** true if there are literals with multiplicities &gt; 1. */
    boolean hasMultiplicities = false;

    /** the inference step which caused the derivation of this clause. */
    InferenceStep inferenceStep;

    /** a timestamp to be used by various algorithms. */
    int timestamp1 = 0;

    /** a timestamp to be used by other algorithms. */
    int timestamp2 = 0;

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
        inferenceStep = new InfInputClause(identifier);
        int length = inputClause.length;
        int start = quantifier.firstLiteralIndex;
        literals = new ArrayList<>(length-start);
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
        inferenceStep      = new InfInputClause(identifier);
        for(int i = Normalizer.literalsStart; i <= normalizedClause.size()-2; i +=2) {
            Literal literal = new Literal(normalizedClause.getInt(i),normalizedClause.getInt(i+1));
            literals.add(literal);
            literal.clause = this;}
        determineClauseType();}


    /** finds the Literal with the given literal.
     *
     * @param literal a literal.
     * @return null or a Literal with the given literal.
     */
    protected Literal findLiteral(int literal) {
        for(Literal literalObject : literals) {
            if(literalObject.literal == literal) return literalObject;}
        return null;}

    /** returns for a two-literal clause the other literal.
     *
     * @param literalObject one of the clause's literalObjects.
     * @return the other literalObject.
     */
    protected Literal otherLiteral(Literal literalObject) {
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
     * If the resulting clause is a unit clause, trueLiterals is applied to it, and the literal is removed.
     * In this case the clause is empty, but nor false.<br>
     * The result +1 may therefor mean: tautology, or unit clause with removed literal.
     * <br>
     * The remover is applied to all removed literals. It may, for example, remove the literals from the index.
     *
     * @param modelStatus  returns +1 (true), -1 (false) or 0 (undefined).
     * @param remover      is applied to the removed literals.
     * @param trueLiterals is applied to all derivable true literals.
     * @return +1 (removed), -1 (unsatisfiable), 0 undefined.
     */
    byte removeLiterals(Function<Literal,Integer> modelStatus, Consumer<Literal> remover, IntConsumer trueLiterals){
        for(int i = 0; i < literals.size(); ++i) {
            Literal literalObject = literals.get(i);
            int status = modelStatus.apply(literalObject);
            if(status == 0) continue;
            literalObject.clause = null;
            remover.accept(literalObject);
            literals.remove(i--);
            expandedSize -= literalObject.multiplicity;
            if(status == 1) {
                limit -= literalObject.multiplicity;
                if(limit <= 0) { // tautology
                    for(Literal litObject : literals) remover.accept(litObject);
                    literals.clear();
                    return 1;}}}

        if(literals.size() == 1) { // unit clause, must be true.
            trueLiterals.accept(literals.get(0).literal);
            remover.accept(literals.get(0));
            literals.clear();
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
        limit = reduceByTrueLiterals(limit,expandedSize,literals,remover,trueLiterals);
        if(limit == 0) return 1;
        if(limit == 1) {
           reduceToDisjunction();
           determineClauseType();
           return (expandedSize < limit) ? (byte)-1: 0;}

        // Example: >= 2 p,q^2,r^2,s and false(p) -> >= 2 q^2,r^2,s -> q,r
        int size = literals.size();
        expandedSize = reduceToEssentialLiterals(limit,literals,remover,trueLiterals);
        if(expandedSize == 0) return 1;
        if(literals.size() < size) reduceToDisjunction();
        determineClauseType();
        return 0;}

    /** reduces the literals to the essential literals.
     * <br>
     * Example: &gt;= 2 q^2,r^2,s -> p,q <br>
     * If such a reduction is possible then the clause becomes a disjunction.<br>
     * The literal's size shrinks if a reduction was achieved.<br>
     * The remover is applied to all removed literals.<br>
     * If a literal becomes true (e.g. unit clause), the trueLiterals is applied to the single literal,
     * and the literal is removed as well.
     *
     * @param limit     the clause's limit
     * @param literals  the literals
     * @param remover   applied to removed literals
     * @param trueLiterals applied to a remaining single literal (must be true).
     * @return 0 if the clause has become true, otherwise the new expandedSize.
     */
    static int reduceToEssentialLiterals(int limit, ArrayList<Literal> literals,  Consumer<Literal> remover, IntConsumer trueLiterals) {
        int remainingMultiplicity = 0; int expandedSize = 0;
        for(Literal literalObject : literals) {
            literalObject.multiplicity = Math.min(limit,literalObject.multiplicity);
            expandedSize += literalObject.multiplicity;
            if(literalObject.multiplicity < limit) remainingMultiplicity += literalObject.multiplicity;}
        if(remainingMultiplicity < limit) {
            expandedSize = 0;
            for(int i = 0; i < literals.size(); ++i) {
                Literal literalObject = literals.get(i);
                if(literalObject.multiplicity < limit) {
                    remover.accept(literalObject);
                    literals.remove(i--);}
                else {literalObject.multiplicity = 1; ++expandedSize;}}
            if(literals.size() == 1) {
                trueLiterals.accept(literals.get(0).literal);
                remover.accept(literals.get(0)); literals.clear();
                return 0;}}
        return expandedSize;}

    /** reduces the clause's literals to the essential literals.
     * <br>
     * Example: &gt;= 2 q^2,r^2,s -> p,q <br>
     * The literal's size shrinks if a reduction was achieved.
     *
     * @param remover   applied to removed literals.
     * @param trueLiterals applied to a remaining single literal (must be true).
     * @return true if the clause survived, otherwise false (all literals removed).
     */
    boolean reduceToEssentialLiterals(Consumer<Literal> remover, IntConsumer trueLiterals) {
        int newExpandedSize = reduceToEssentialLiterals(limit, literals,remover,trueLiterals);
        if(newExpandedSize == 0) return false;
        if(newExpandedSize < expandedSize) reduceToDisjunction();
        return true;}

    /** to be used by divideByGCD. */
    private final IntArrayList numbers = new IntArrayList();

    /** divides the limit and the multiplicities by their greatest common divisor.
     *
     * @return true if the clause is changed.
     */
    protected boolean divideByGCD() {
        numbers.clear(); numbers.add(limit);
        boolean stop = false;
        for(Literal literalObject : literals) {
            int multiplicity = literalObject.multiplicity;
            if(multiplicity == 1) {stop = true; break;}
            numbers.add(multiplicity);}
        if(!stop) {
            int gcd = Utilities.gcd(numbers);
            if(gcd > 1) {
                expandedSize = 0;
                limit /= gcd;
                for(Literal literalObject : literals) {
                    literalObject.multiplicity = Math.min(limit, literalObject.multiplicity / gcd);
                    expandedSize += literalObject.multiplicity;}
                if(limit == 1) {
                    quantifier = Quantifier.OR; isDisjunction = true;}
            return true;}}
        return false;}

    /** to be used by reduceByTrueLiterals. */
    private final ArrayList<Literal> auxiliaryLiterals = new ArrayList<>(5);


    /** extracts all literals which must be true because there are not enough other literals to satisfy the limit.
     * <br>
     * All superfluous literals are removed.
     *
     * @param remover       applied to removed literals
     * @param trueLiterals  applied  to true literals
     * @return              +1 if the entire clause is removed, otherwise 0.
     */
    byte reduceByTrueLiterals(Consumer<Literal> remover, IntConsumer trueLiterals) {
        return removeLiterals(literalObject -> (expandedSize-literalObject.multiplicity < limit) ? 1 : 0,remover,trueLiterals);}

    /** Tries to find true literals in a list of literals.
     * <br>
     * Example:  &gt;= 3 p,q,r -> all literals must be true<br>
     * Example:  &gt;= 2 p^2,r -> p must be true.<br>
     * True literals are removed and the trueLiterals consumer is applied.<br>
     * If there is still one literal left, it is also made true.<br>
     * If the clause became true, all literals are removed.
     *
     * @param limit         the clause's limit
     * @param expandedSize  the clause's expandedSize
     * @param literals      the literalObjects
     * @param remover       is applied to the removed literalObjects.
     * @param trueLiterals  is applied to the true literals.
     * @return              0 if the clause itself became true, otherwise the possibly reduced limit.
     */
    static int reduceByTrueLiterals(int limit, int expandedSize, ArrayList<Literal> literals,
                                    Consumer<Literal> remover, IntConsumer trueLiterals) {
        for(int i = 0; i < literals.size(); ++i) {
            Literal literalObject = literals.get(i);
            if(expandedSize - literalObject.multiplicity < limit) {
                remover.accept(literalObject);
                trueLiterals.accept(literalObject.literal);  // The literal must be true.
                literals.remove(i--);
                limit -= literalObject.multiplicity;
                expandedSize -= literalObject.multiplicity;
                if(limit <= 0) {
                    for(Literal litObject : literals) remover.accept(litObject);
                    literals.clear();
                    return 0;}}}
            if(literals.size() == 1) { // unit clause, must be true.
                trueLiterals.accept(literals.get(0).literal);
                remover.accept(literals.get(0));
                literals.clear();
                return 0;}
        return limit;}


    /** reduces the clause to a disjunction.
     */
    protected void reduceToDisjunction() {
        limit = 1;
        for(Literal literalObject : literals) literalObject.multiplicity = 1;
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
     * @param id              for generating a new identifier.
     * @param literalObject1  the first parent literal.
     * @param literalObject2  the second parent literal.
     * @param trueLiterals    applied to derived true literals.
     * @return                null if the resolvent is a tautology, otherwise the new resolvent.
     */
    static Clause resolve( Literal literalObject1, Literal literalObject2, IntSupplier id,IntConsumer trueLiterals){
        assert literalObject1.literal == -literalObject2.literal;
        Clause clause1 = literalObject1.clause;
        Clause clause2 = literalObject2.clause;
        int newLimit = clause1.limit + clause2.limit - Math.max(literalObject1.multiplicity,literalObject2.multiplicity);
        ArrayList<Literal> newLiterals = new ArrayList<>(clause1.literals.size() + clause2.literals.size()-2);
        int expandedSize = 0;
        for(Literal litObject1 : clause1.literals) {
            if(litObject1 == literalObject1) continue;
            expandedSize += litObject1.multiplicity;
            newLiterals.add(new Literal(litObject1.literal,litObject1.multiplicity));}

        for(Literal litObject2 : clause2.literals) {
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
        if(newLiterals.size() == 1) {trueLiterals.accept(newLiterals.get(0).literal); return null;} // unit clause
        if(newLimit == 1) return  new Clause(id.getAsInt(),Quantifier.OR,1,newLiterals);

        newLimit = reduceByTrueLiterals(newLimit,expandedSize, newLiterals,(l->{}),trueLiterals);
        if(newLimit == 0) return null;
        if(newLimit == 1) return new Clause(id.getAsInt(),Quantifier.OR,1,newLiterals);

        int size = newLiterals.size();
        expandedSize = reduceToEssentialLiterals(newLimit,newLiterals,(l->{}),trueLiterals);
        if(expandedSize == 0) return null;
        return new Clause(id.getAsInt(), (newLiterals.size() == size)? Quantifier.ATLEAST: Quantifier.OR,
                newLimit,newLiterals); }

    /** replaces the literalObject by a new literal (typically equivalence replacement).
     * <br>
     * The method checks if the new literal or its negation is already in the clause.<br>
     * Superfluous literals are removed from the clause.<br>
     * The changed clause is simplified.
     *
     * @param literalObject  a literalObject of some clause.
     * @param newLiteral     a literal (may already be in the clause).
     * @param remover        applied to removed literals.
     * @param adder          applied to newly added literals.
     * @param trueLiterals   applied to derivable true literals.
     * @return               1 if the clause has become superfluous (e.g. tautology), 0 if it survived.
     */
    static int replaceLiteral(final Literal literalObject, int newLiteral,
                              Consumer<Literal> remover, Consumer<Literal> adder, IntConsumer trueLiterals) {
        Clause clause = literalObject.clause;
        Function<Literal,Integer> decider = ((Literal l) -> (l == literalObject) ? -1:0);
        Literal newLiteralObject = clause.findLiteral(newLiteral);
        if(newLiteralObject != null) { // the two literals merge into one.
            int multiplicity = newLiteralObject.multiplicity;
            newLiteralObject.multiplicity = Math.min(clause.limit, newLiteralObject.multiplicity + literalObject.multiplicity);
            clause.expandedSize +=  newLiteralObject.multiplicity - multiplicity;
            return clause.removeLiterals(decider,remover,trueLiterals);}
        else {
            final Literal negNewLiteralObject = clause.findLiteral(-newLiteral);
            if(negNewLiteralObject != null) { // could be tautology
                if(negNewLiteralObject.multiplicity == literalObject.multiplicity) { // is tautology
                    for(Literal litObject : clause.literals) remover.accept(litObject);
                    clause.literals.clear();
                    return 1;}
                else {
                    if(negNewLiteralObject.multiplicity > literalObject.multiplicity) { // Example; -p^5 q^3 (q -> p)  -> -p^2
                        negNewLiteralObject.multiplicity -= literalObject.multiplicity;
                        return clause.removeLiterals(decider,remover,trueLiterals);}
                    else {literalObject.multiplicity -= negNewLiteralObject.multiplicity; // Example; -p^3 q^5 (q -> p)  -> p^2
                        remover.accept(literalObject);
                        literalObject.literal = newLiteral;
                        adder.accept(literalObject);
                        return clause.removeLiterals(((Literal l) -> (l == negNewLiteralObject) ? -1:0),remover,trueLiterals);}}}}
        remover.accept(literalObject);
        literalObject.literal = newLiteral;
        adder.accept(literalObject);
        clause.determineClauseType();
        return 0;}

    /** investigates the distribution of positive and negative literals and determines the ClauseType.*/
    void determineClauseType() {
       int positiveLiterals = 0; int negativeLiterals = 0;
       for(Literal literalObject : literals) {
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
    public String toString(Symboltable symboltable, int size) {
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
