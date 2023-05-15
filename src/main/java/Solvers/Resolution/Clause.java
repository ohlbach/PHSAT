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
    protected int id;

    /** the connective, OR or ATLEAST. */
    protected Quantifier quantifier;

    /** true if the connective is OR. */
    protected boolean isDisjunction;

    /** the quantifier for ATLEAST clauses. */
    protected int limit = 1;

    /** the sum of all multiplicities of the literals. */
    protected int expandedSize = 0;

    /** the list of all Literal objects in the clause. */
    protected ArrayList<Literal> literals = new ArrayList<>();

    /** characterises the distribution of positive and negative literals in a clause. */
    protected ClauseType clauseType;

    /** flag to indicate that the clause still exists. */
    protected boolean exists = true;

    /** true if there are literals with multiplicities &gt; 1. */
    protected boolean hasMultiplicities = false;

    /** the inference step which caused the derivation of this clause. */
    protected InferenceStep inferenceStep;

    /** a timestamp to be used by various algorithms. */
    protected int timestamp1 = 0;
    /** a timestamp to be used by other algorithms. */
    protected int timestamp2 = 0;

    /** a pointer to the previous clause in a doubly connected list. */
    protected Clause previousClause;

    /** a pointer to the next clause in a doubly connected list. */
    protected Clause nextClause;

    /** The constructor turns an InputClause int[]-array into a Clause object.
     *
     * @param inputClause InputClause int[]-array.
     */
    public Clause(int[] inputClause) {
        id = inputClause[0];
        quantifier = Quantifier.getQuantifier(inputClause[1]);
        assert(quantifier != null && (quantifier == Quantifier.OR || quantifier == Quantifier.ATLEAST));
        isDisjunction = quantifier == Quantifier.OR;
        limit = isDisjunction ? 1 : inputClause[2];
        inferenceStep = new InfInputClause(id);
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

    /** constructs a new clause from a list of literal,multiplicity pairs.
     * This constructor is basically for test purposes.
     *
     * @param id         the identifier.
     * @param quantifier the connective.
     * @param limit the quantifier.
     * @param items      pairs literal,multiplicity.
     */
    public Clause(int id, Quantifier quantifier, int limit, int... items) {
        this.id = id;
        this.limit = limit;
        this.quantifier = quantifier;
        isDisjunction = quantifier == Quantifier.OR;
        literals = new ArrayList<>(items.length/2);
        for(int i = 0; i < items.length; i +=2) {
            int literal = items[i];
            int multiplicity = Math.min(items[i+1], limit);
            expandedSize += multiplicity;
            Literal literalObject = new Literal(literal,multiplicity);
            literalObject.clause = this;
            literals.add(literalObject);}
        determineClauseType();
        hasMultiplicities = expandedSize > literals.size();}

    /** This is a constructor for a disjunction.
     *
     * @param id             the identifier.
     * @param literalNumbers the literals of the disjunction.
     */
    public Clause(int id,  int... literalNumbers) {
        this.id = id;
        quantifier = Quantifier.OR;
        expandedSize = literalNumbers.length;
        for(int literal : literalNumbers) {
            Literal literalObject = new Literal(literal,1);
            literalObject.clause = this;
            literals.add(literalObject);}
        determineClauseType();}

    /** constructs a new clause.
     * The literal's multiplicities are automatically reduced to 'limit'.
     *
     * @param id         the identifier for the clause.
     * @param quantifier the quantifier.
     * @param limit      the limit for the quantifier.
     * @param literals   the literals.
     */
    public Clause(int id, Quantifier quantifier, int limit, ArrayList<Literal> literals) {
        this.id = id;
        this.quantifier = quantifier;
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
     */
    public Clause(IntArrayList normalizedClause) {
        id = normalizedClause.getInt(0);
        quantifier = Normalizer.getQuantifier(normalizedClause);
        isDisjunction = quantifier == Quantifier.OR;
        limit = Normalizer.getMin(normalizedClause);
        expandedSize = Normalizer.getExpandedSize(normalizedClause);
        hasMultiplicities = Normalizer.hasMultiplicities(normalizedClause);
        inferenceStep = new InfInputClause(id);
        for(int i = Normalizer.literalsStart; i <= normalizedClause.size()-2; i +=2) {
            Literal literal = new Literal(normalizedClause.getInt(i),normalizedClause.getInt(i+1));
            literals.add(literal);
            literal.clause = this;}
        determineClauseType();}



    /** checks if the atleast-clause is true (limit &lt;= 0).
     *
     * @return true if the atleast-clause is true (limit &lt;= 0).
     */
    protected boolean isTrue() {
        return limit <= 0;}

    /** checks if the atleast-clause is false (limit &gt; expandedSize).
     *
     * @return true if the atleast-clause is false (limit &gt; extendedSize).
     */
    protected boolean isFalse() {
        return limit > expandedSize;}




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
     * @return true if the clause still exists.
     */
    protected boolean removeLiteral(Literal literalObject, boolean reduceLimit) {
        literals.remove(literalObject);
        if(literals.isEmpty()) return false;
        determineClauseType();
        if(reduceLimit) {
            limit -= literalObject.multiplicity;
            if(limit <= 0) {return false;}}
        expandedSize -= literalObject.multiplicity;
        literalObject.clause = null;
        if(isDisjunction) return true;
        if(limit == 1) { // clause turns into a disjunction
            isDisjunction = true;
            quantifier = Quantifier.OR;
            for(Literal literalObject1 : literals) {literalObject1.multiplicity = 1;}
            expandedSize = literals.size();
            hasMultiplicities = true;
            return true;}

        if(reduceLimit) { // adjust all multiplicities
            for(Literal literalObject1 : literals) {
                int multiplicity = literalObject1.multiplicity;
                if(multiplicity > limit) {
                    expandedSize -= limit -multiplicity;
                    literalObject1.multiplicity = limit;}}}
        hasMultiplicities = expandedSize > literals.size();
        return true;}


    /** removes complementary pairs from the clause.
     * Example: atleast 4 p^3, -p^2, q, r -> atleast 2 p,q,r. <br>
     * Example: atleast 2 p^2, -p^1,q,r -> atleast 0 q,r -> true.<br>
     * The clause may be turned into a disjunction.
     *
     * @param complementaries for counting the removal of complementary literals.
     * @return true if the clause became a true clause.
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
        determineClauseType();
        if(!complementariesFound) return false;
        if(literals.isEmpty()) return true;

        if(limit == 1) {
            quantifier = Quantifier.OR;
            isDisjunction = true;
            hasMultiplicities = false;
            for(Literal literalObject : literals) literalObject.multiplicity = 1;}
        else {
            for(Literal literalObject : literals) {
                if(literalObject.multiplicity > limit) {
                    expandedSize -= literalObject.multiplicity - limit;
                    literalObject.multiplicity = limit; }}
            hasMultiplicities = expandedSize > literals.size();}
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

    /** removes literals which must be true in an ATLEAST-clause.
     *  Example: atleast 4 p^2,q^2,r.<br>
     *  If p is false then the clause becomes atleast 4 q^2,r, which is no longer satisfiable.<br>
     *  Therefore, p must be true, and q as well.<br>
     *  Both can be removed and made true.<br>
     *  The resulting clause may get a limit &lt;= 0. It is true and can be removed.
     *
     * @param removedLiterals for adding the removed literals (for purity check).
     * @return the literals which must be true.
     */
    protected ArrayList<Literal> reduceByTrueLiterals(ArrayList<Literal> removedLiterals) {
        if(!hasMultiplicities) return null;
        auxiliaryLiterals.clear(); // we collect the literals which become true.
        for(Literal literalObject: literals) {
            if(expandedSize-literalObject.multiplicity < limit) {
                auxiliaryLiterals.add(literalObject);}}
        if(auxiliaryLiterals.isEmpty()) return null;

        for(Literal literalObject: auxiliaryLiterals) { // these are the true literals
            removedLiterals.add(literalObject);
            literals.remove(literalObject);
            limit -= literalObject.multiplicity;
            expandedSize -= literalObject.multiplicity;}
        if(limit <= 0) {
            removedLiterals.addAll(literals);
            return auxiliaryLiterals;}
        determineClauseType();

        for(Literal literalObject: literals) {
            if(literalObject.multiplicity > limit) {
                expandedSize -= literalObject.multiplicity - limit;
                literalObject.multiplicity = limit;}}
        if(limit == 1) {
            isDisjunction = true;
            quantifier = Quantifier.OR;
            hasMultiplicities = false;}
        else {hasMultiplicities = expandedSize > literals.size();}

        return auxiliaryLiterals;}


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
        determineClauseType();
        if(multiplicities >= limit) return false; // the remaining literals can satisfy the clause.

        for(Literal literalObject : auxiliaryLiterals) {
            removedLiterals.add(literalObject);
            literals.remove(literalObject);
            literalObject.clause = null;}
        for(Literal literalObject : literals) literalObject.multiplicity = 1;
        limit = 1;
        isDisjunction = true;
        quantifier = Quantifier.OR;
        expandedSize = literals.size();
        hasMultiplicities = false;
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
        hasMultiplicities = false;
        for(Literal literalObject : literals) {
            literalObject.multiplicity = Math.min(limit,literalObject.multiplicity);
            expandedSize += literalObject.multiplicity;
            hasMultiplicities |= literalObject.multiplicity > 1;}}


    /** reduces the clause to a disjunction.
     */
    protected void reduceToDisjunction() {
        if(limit == 1) return;
        limit = 1;
        for(Literal literalObject : literals) literalObject.multiplicity = 1;
        expandedSize = literals.size();
        isDisjunction = true;
        hasMultiplicities = false;
        quantifier = Quantifier.OR;}

    /** creates a resolvent for two clauses.
     *
     * @param id              for generating a new identifier.
     * @param literalObject1  the first parent literal.
     * @param literalObject2  the second parent literal.
     * @param complementaries for counting the removal of complementary literals.
     * @return                null if the resolvent is a tautology, otherwise the new resolvent.
     */
    Clause resolve(IntSupplier id, Literal literalObject1, Literal literalObject2, IntConsumer complementaries) {
        Clause clause2 = literalObject2.clause;
        int newLimit = limit + clause2.limit - Math.max(literalObject1.multiplicity,literalObject2.multiplicity);
        ArrayList<Literal> newLiterals = new ArrayList<>(literals.size() + clause2.literals.size()-2);
        for(Literal literalObject : literals) {
            if(literalObject == literalObject1) continue;
            newLiterals.add(new Literal(literalObject.literal,literalObject.multiplicity));}
        for(Literal literalObject : clause2.literals) {
            if(literalObject == literalObject2) continue;
            boolean found = false;
            for(Literal newLiteralObject : newLiterals) {
                if(newLiteralObject.literal == literalObject.literal) {
                    found = true;
                    newLiteralObject.multiplicity = Math.min(newLimit, newLiteralObject.multiplicity + literalObject.multiplicity);
                    break;}}
            if(found) continue;
            newLiterals.add(new Literal(literalObject.literal,literalObject.multiplicity));}
        Quantifier newQuantifier = (newLimit == 1) ? Quantifier.OR : Quantifier.ATLEAST;
        Clause resolvent = new Clause(id.getAsInt(),newQuantifier,newLimit,newLiterals);
        return resolvent.removeComplementaryLiterals(complementaries,null) ? null : resolvent;}

    /** investigates the distribution of positive and negative literals and determines the ClauseType.
     */
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
        st.append((size == 0) ? id : String.format("%"+size+"s",id)).append(": ");
        if(quantifier == Quantifier.ATLEAST) st.append(">= ").append(limit).append(" ");
        if(literals.size() > 0) {
            int length = literals.size()-1;
            for(int i = 0; i < length; ++i) {
                st.append(literals.get(i).toString(symboltable)).append(quantifier.separator);}
            st.append(literals.get(length).toString(symboltable));}
        return st.toString();}

}
