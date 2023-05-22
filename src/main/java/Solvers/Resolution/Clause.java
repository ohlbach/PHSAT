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
 *  Clauses can be destructively changed.
 */
public class Clause {
    /** the identifier for the clause. */
    int identifier;

    /** another identifier for clauses which originated from interval-type clauses */
    int subIdentifier = 0;

    /** the connective, OR or ATLEAST. */
    Quantifier quantifier;

    /** true if the connective is OR. */
    boolean isDisjunction;

    /** the quantifier for ATLEAST clauses. */
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

    /** constructs a new clause.
     * The literal's multiplicities are automatically reduced to 'limit'.
     *
     * @param identifier         the identifier for the clause.
     * @param quantifier the quantifier.
     * @param limit      the limit for the quantifier.
     * @param literals   the literals.
     */
    public Clause(int identifier, Quantifier quantifier, int limit, ArrayList<Literal> literals) {
        this.identifier = identifier;
        this.quantifier = quantifier;
        isDisjunction = quantifier == Quantifier.OR;
        this.limit = limit;
        this.literals = literals;
        for(Literal literalObject: literals) {
            literalObject.clause = this;
            literalObject.multiplicity = Math.min(limit,literalObject.multiplicity);
            expandedSize += literalObject.multiplicity;}
        determineClauseType();
        hasMultiplicities = expandedSize > literals.size();}


    /** this constructor turns a normalizedClause to a clause for the Resolution solver.
     *
     * @param normalizedClause a normalized and simplified clause from the Normalizer.
     * @param subIdentifier a secondary identifier for multiple clauses resulting from interval-type clauses.
     *                      subIdentifier = 0 will be ignored.
     */
    public Clause(IntArrayList normalizedClause, int subIdentifier) {
        identifier = normalizedClause.getInt(0);
        this.subIdentifier = subIdentifier;
        quantifier = Normalizer.getQuantifier(normalizedClause);
        isDisjunction = quantifier == Quantifier.OR;
        limit = Normalizer.getMin(normalizedClause);
        expandedSize = Normalizer.getExpandedSize(normalizedClause);
        hasMultiplicities = Normalizer.hasMultiplicities(normalizedClause);
        inferenceStep = new InfInputClause(identifier);
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

    /** removes a literal object from the clause.
     * All internal data are updated (limit, multiplicity of the literals, connective).<br>
     * If the clause is a disjunction then the literal is just removed.<br>
     * If the clause is an atleast-clause then the literals multiplicities are adjusted to the limit.<br>
     * If the resulting limit is 1 then the clause is turned into a disjunction.<br>
     * If the new limit is &lt;= 0 then the clause may be removed. Therefor only the limit is changed.<br>
     * Other simplifications are not performed.
     *
     * @param literalObject    the literal to be removed.
     * @param reduceLimit      if true then the limit is reduced by the literal's multiplicity.
     * @return                 +1 (tautology) -1 (unsatisfiable) 0 otherwise.
     */
    protected byte removeLiteral(Literal literalObject, boolean reduceLimit) {
        literals.remove(literalObject);
        literalObject.clause = null;
        determineClauseType();
        if(reduceLimit) {
            limit -= literalObject.multiplicity;
            if(limit <= 0) {return +1;}} // tautology
        expandedSize -= literalObject.multiplicity;
        if(expandedSize < limit) return -1; // not enough literals to satisfy the limit.
        if(isDisjunction) return 0;

        if(limit == 1) { // clause turns into a disjunction
            isDisjunction = true;
            quantifier = Quantifier.OR;
            for(Literal literalObject1 : literals) {literalObject1.multiplicity = 1;}
            expandedSize = literals.size();
            hasMultiplicities = true;
            return 0;}

        if(reduceLimit) { // adjust all multiplicities
            for(Literal literalObject1 : literals) {
                int multiplicity = literalObject1.multiplicity;
                if(multiplicity > limit) {
                    expandedSize -= limit - multiplicity;
                    literalObject1.multiplicity = limit;}}}
        hasMultiplicities = expandedSize > literals.size();
        return 0;}

    /** removes all literals where modelStatus != 0.
     * <br>
     * - all possible simplifications are done. <br>
     * - all superfluous literals are removed.<br>
     * - for all derivable literals, trueLiterals is called.
     *
     * @param modelStatus returns +1 (true), -1 (false) or 0 (undefined)
     * @param remover is applied to the removed literals.
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
     * Example: &gt= 2 q^2,r^2,s -> p,q <br>
     * The literal's size shrinks if a reduction was achieved.
     *
     * @param limit     the clause's limit
     * @param literals  the literals
     * @param remover   applied to removed literals
     * @param trueLiterals applied to a remaining single literal (must be true).
     * @return 0 if the clause has become true, otherwise the old essential literals
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


    /** removes complementary pairs from the clause.
     * Example: atleast 4 p^3, -p^2, q, r -> atleast 2 p,q,r. <br>
     * Example: atleast 2 p^2, -p^1,q,r -> atleast 0 q,r -> true.<br>
     * The clause may be turned into a disjunction.
     *
     * @param complementaries for counting the removal of complementary literals.
     * @return true if the clause became a true clause (tautology).
     */
    protected boolean removeComplementaryLiterals(IntConsumer complementaries, Consumer<Literal> literalRemover) {
        boolean complementariesFound = false;
        for(int i = 0; i < literals.size()-1; ++i) {
            Literal literalObject1 = literals.get(i);
            int literal1 = literalObject1.literal;
            int multiplicity1 = literalObject1.multiplicity;
            for(int j = i+1; j < literals.size(); ++j) {
                Literal literalObject2 = literals.get(j);
                if(literalObject2.literal == -literal1) {
                    complementariesFound = true;
                    int multiplicity2 = literalObject2.multiplicity;
                    if(multiplicity1 == multiplicity2) {
                        complementaries.accept(multiplicity1);
                        limit -= multiplicity1;
                        if(limit <= 0) return true;
                        literals.remove(j);
                        literals.remove(i--);
                        if(literalRemover != null) {
                            literalRemover.accept(literalObject1);
                            literalRemover.accept(literalObject2);}
                        expandedSize -= multiplicity1;
                        break;}
                    if(multiplicity1 > multiplicity2) {
                        complementaries.accept(multiplicity2);
                        limit -= multiplicity2;
                        if(limit <= 0) return true;
                        literalObject1.multiplicity -= multiplicity2;
                        literals.remove(j);
                        if(literalRemover != null) literalRemover.accept(literalObject2);
                        expandedSize -= multiplicity2;
                        break;}
                    complementaries.accept(multiplicity1);
                    limit -= multiplicity1;
                    if(limit <= 0) return true;
                    literalObject2.multiplicity -= multiplicity1;
                    literals.remove(i--);
                    if(literalRemover != null) literalRemover.accept(literalObject1);
                    expandedSize -= multiplicity1;
                    break;}}}
        if(!complementariesFound) return false;
        if(literals.isEmpty()) return true;
        determineClauseType();
        adjustMultiplicitiesToLimit();
        reduceToDisjunction();
        return false;}

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


    /** shrinks the literals to the essential literals, if possible, and turns the clause to a disjunction.
     * Example: atleast 2 p^2,q^2,r.<br>
     * In this case either p or q must be true. If both are false then r is not enough to get 2 true literals.<br>
     * The clause shrinks to the disjunction: p,q.
     *
     * @param removedLiterals for collecting the removed literals (for purity check).
     * @return true if literals have been removed.
     */
    protected boolean reduceToEssentialLiterals(ArrayList<Literal> removedLiterals) {
        if(!hasMultiplicities) return false;
        auxiliaryLiterals.clear(); // collect the literals with multiplicities < limit
        int multiplicities = 0;
        for(Literal literalObject: literals) {
            if(literalObject.multiplicity != limit) {
                auxiliaryLiterals.add(literalObject);
                multiplicities += literalObject.multiplicity;}}
        if(multiplicities >= limit) return false; // the remaining literals can satisfy the clause.

        for(Literal literalObject : auxiliaryLiterals) {
            removedLiterals.add(literalObject);
            literals.remove(literalObject);
            literalObject.clause = null;}
        limit = 1;
        adjustMultiplicitiesToLimit();
        reduceToDisjunction();
        determineClauseType();
        return true;}




    /** replaces the oldLiteral in the clause by the newLiteral.
     * The literal's multiplicities are adjusted to the limit.
     *
     * @param oldLiteral  one of the clause's literals
     * @param newLiteral  the literal which replaces the old literal.
     */
    protected void replaceLiteral(Literal oldLiteral, Literal newLiteral) {
        for(int i = 0; i < literals.size(); ++i) {
            if(literals.get(i) == oldLiteral) {
                literals.set(i,newLiteral);
                if(limit > 1) adjustMultiplicitiesToLimit();
                break;}}
        determineClauseType();}


    /** reduces the literal's multiplicity to the clause's limit.
     */
    protected void adjustMultiplicitiesToLimit() {
        expandedSize = 0;
        for(Literal literalObject : literals) {
            literalObject.multiplicity = Math.min(limit,literalObject.multiplicity);
            expandedSize += literalObject.multiplicity;}
        hasMultiplicities = expandedSize > literals.size();}


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
