package Solvers.Simplifier;

import Datastructures.Clauses.Connective;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.function.IntSupplier;

/** A Clause object is essentially a collection of Literal objects.<br>
 *  A clause can only be a disjunction (OR-clause) or an ATLEAST-clause.
 *  Other types are not supported.<br>
 *  Each Clause object may be part of a doubly connected list of clauses. <br>
 *  Clauses can be destructively changed.
 */
public class Clause {
    /** the identifier for the clause. */
    protected int id;

    /** the connective, OR or ATLEAST. */
    protected Connective quantifier;

    /** true if the connective is OR. */
    protected boolean isDisjunction;

    /** the quantifier for ATLEAST clauses. */
    protected int limit = 1;

    /** the sum of all multiplicities of the literals. */
    protected int expandedSize = 0;

    /** the list of all Literal objects in the clause. */
    protected ArrayList<Literal> literals = new ArrayList<>();

    /** flag to indicate that the clause still exists. */
    protected boolean exists = true;

    /** true if there are literals with multiplicities &gt; 1. */
    protected boolean hasMultiplicities = false;

    /** the inference step which caused the derivation of this clause. */
    protected InferenceStep inferenceStep;

    /** a timestamp to be used by various algorithms. */
    protected int timestamp = 0;

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
        quantifier = Connective.getConnective(inputClause[1]);
        assert(quantifier != null && (quantifier == Connective.OR || quantifier == Connective.ATLEAST));
        isDisjunction = quantifier == Connective.OR;
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
        hasMultiplicities = expandedSize > literals.size();}

    /** constructs a new clause from a list of literal,multiplicity pairs.
     *
     * @param id         the identifier.
     * @param quantifier the connective.
     * @param limit the quantifier.
     * @param items      pairs literal,multiplicity.
     */
    public Clause(int id, Connective quantifier, int limit, int... items) {
        this.id = id;
        this.limit = limit;
        this.quantifier = quantifier;
        isDisjunction = quantifier == Connective.OR;
        literals = new ArrayList<>(items.length/2);
        for(int i = 0; i < items.length; i +=2) {
            int literal = items[i];
            int multiplicity = Math.min(items[i+1], limit);
            expandedSize += multiplicity;
            Literal literalObject = new Literal(literal,multiplicity);
            literalObject.clause = this;
            literals.add(literalObject);}
        hasMultiplicities = expandedSize > literals.size();}

    /** This is a constructor for a disjunction.
     *
     * @param id             the identifier.
     * @param literalNumbers the literals of the disjunction.
     */
    public Clause(int id,  int... literalNumbers) {
        this.id = id;
        this.quantifier = Connective.OR;
        expandedSize = literalNumbers.length;
        for(int literal : literalNumbers) {
            Literal literalObject = new Literal(literal,1);
            literalObject.clause = this;
            literals.add(literalObject);}}

    public Clause(int id, int limit, ArrayList<Literal> literals) {
        this.id = id;
        this.limit = limit;
        this.literals = literals;
        expandedSize = 1;
        hasMultiplicities = false;

        for(Literal literalOject: literals) {
            literalOject.clause = this;
            hasMultiplicities |= literalOject.multiplicity > 1;
            expandedSize += literalOject.multiplicity;}

        if(limit == 1) {
            quantifier = Connective.OR;
            isDisjunction = true;}
        else {
            quantifier = Connective.ATLEAST;
            isDisjunction = false;}}

    /** checks if the atleast-clause is true
     *
     * @return true if the atleast-clause is true (limit <= 0)
     */
    protected boolean isTrue() {
        return limit <= 0;}

    /** checks if the atleast-clause is false
     *
     * @return true if the atleast-clause is false (limit > extendedSize)
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

    /** removes a literal object from the clause.<br>
     * All internal data are updated (limit, multiplicity of the literals, connective).<br>
     * If the clause is a disjunction then the literal is just removed.<br>
     * If the clause is an atleast-clause then the literals multiplicities are adjusted to the limit.<br>
     * If the resulting limit is 1 then the clause is turned into a disjunction.<br>
     * If the new limit is &lt;= 0 then the clause may be removed. Therefor only the limit is changed.<br>
     * Other simplifications are not performed.
     *
     * @param literalObject    the literal to be removed.
     * @param reduceQuantifier if true then the quantifier is reduced by the literal's multiplicity.
     * @return true if the clause still exists.
     */
    protected boolean removeLiteral(Literal literalObject, boolean reduceQuantifier) {
        if(reduceQuantifier) {
            limit -= literalObject.multiplicity;
            if(limit <= 0) {exists = false; return false;}}

        literals.remove(literalObject);
        expandedSize -= literalObject.multiplicity;
        literalObject.clause = null;
        if(isDisjunction) return true;
        if(limit == 1) { // clause turns into a disjunction
            isDisjunction = true;
            quantifier = Connective.OR;
            for(Literal literalObject1 : literals) {literalObject1.multiplicity = 1;}
            expandedSize = literals.size();
            hasMultiplicities = true;
            return true;}

        if(reduceQuantifier) { // adjust all multiplicities
            for(Literal literalObject1 : literals) {
                int multiplicity = literalObject1.multiplicity;
                if(multiplicity > limit) {
                    expandedSize -= limit -multiplicity;
                    literalObject1.multiplicity = limit;}}}
        hasMultiplicities = expandedSize > literals.size();
        return true;}

    /** merges multiple literals of disjunctions into one literal.
     */
    public void removeDoubleLiterals() {
        assert(quantifier == Connective.OR);
        for(int i = 0;  i < literals.size()-1; ++i) {
            int literal = literals.get(i).literal;
            for(int j = i+1; j < literals.size(); ++j) {
                if(literal == literals.get(j).literal) literals.remove(j--);}}}

    /** removes complementary pairs from the clause.<br>
     * Example: atleast 4 p^3, -p^2, q, r -> atleast 2 p,q,r. <br>
     * Example: atleast 2 p^2, -p^1,q,r -> atleast 0 q,r -> true.<br>
     * The clause may be turned into a disjunction.
     *
     * @return true if the clause became a true clause.
     */
    protected boolean removeComplementaryLiterals() {
        for(int i = 0; i < literals.size()-1; ++i) {
            Literal literalObject1 = literals.get(i);
            int literal1 = literalObject1.literal;
            int multiplicity1 = literalObject1.multiplicity;
            for(int j = i+1; j < literals.size(); ++j) {
                Literal literalObject2 = literals.get(j);
                if(literalObject2.literal == -literal1) {
                    int multiplicity2 = literalObject2.multiplicity;
                    if(multiplicity1 == multiplicity2) {
                        limit -= multiplicity1;
                        if(limit <= 0) return true;
                        literals.remove(j);
                        literals.remove(i--);
                        expandedSize -= multiplicity1;
                        break;}
                    if(multiplicity1 > multiplicity2) {
                        limit -= multiplicity2;
                        if(limit <= 0) return true;
                        literalObject1.multiplicity -= multiplicity2;
                        literals.remove(j);
                        expandedSize -= multiplicity2;
                        break;}
                    limit -= multiplicity1;
                    if(limit <= 0) return true;
                    literalObject2.multiplicity -= multiplicity1;
                    literals.remove(i--);
                    expandedSize -= multiplicity1;
                    break;}}}
        if(literals.isEmpty()) return true;
        hasMultiplicities = expandedSize > literals.size();
        if(limit == 1) {quantifier = Connective.OR; isDisjunction = true;}
        return false;}

    private final IntArrayList numbers = new IntArrayList();

    /** divides the quantifier and the multiplicities by their greatest common divisor.
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
                    quantifier = Connective.OR; isDisjunction = true;}
            return true;}}
        return false;}

    private final ArrayList<Literal> auxiliaryLiterals = new ArrayList<>(5);

    /** removes literals which must be true in an ATLEAST-clause. <br>
     *  Example: atleast 4 p^2,q^2,r.<br>
     *  If p is false then the clause becomes atleast 4 q^2,r, which is no longer satisfiable.<br>
     *  Therefore, p must be true, and q as well.<br>
     *  Both can be removed and made true.<br>
     *  The resulting clause may get a quantifier &lt;= 0. It is true and can be removed.
     *
     * @param removedLiterals for adding the removed literals (for purity check).
     * @return the literals which must be true.
     */
    protected ArrayList<Literal> reduceByTrueLiterals(IntArrayList removedLiterals) {
        if(!hasMultiplicities) return null;
        auxiliaryLiterals.clear(); // we collect the literals which become true.
        for(Literal literalObject: literals) {
            if(expandedSize-literalObject.multiplicity < limit) {
                auxiliaryLiterals.add(literalObject);}}
        if(auxiliaryLiterals.isEmpty()) return null;

        for(Literal literalObject: auxiliaryLiterals) {
            removedLiterals.add(literalObject.literal);
            literals.remove(literalObject);
            limit -= literalObject.multiplicity;
            expandedSize -= literalObject.multiplicity;
            literalObject.clause = null;}
        if(limit <= 0) {
            for(Literal literalObject : literals) removedLiterals.add(literalObject.literal);
            return auxiliaryLiterals;}

        for(Literal literalObject: literals) {
            if(literalObject.multiplicity > limit) {
                expandedSize -= literalObject.multiplicity - limit;
                literalObject.multiplicity = limit;}}
        if(limit == 1) {
            isDisjunction = true;
            quantifier = Connective.OR;
            hasMultiplicities = false;}
        else {hasMultiplicities = expandedSize > literals.size();}

        return auxiliaryLiterals;}


    /** shrinks the literals to the essential literals, if possible, and turns the clause to a disjunction.<br>
     * Example: atleast 2 p^2,q^2,r.<br>
     * In this case either p or q must be true. If both are false then r is not enough to get 2 true literals.<br>
     * The clause shrinks to the disjunction: p,q.
     *
     * @param removedLiterals for collecting the removed literals (for purity check).
     * @return true if literals have been removed.
     */
    protected boolean reduceToEssentialLiterals(IntArrayList removedLiterals) {
        if(!hasMultiplicities) return false;
        auxiliaryLiterals.clear();
        int multiplicities = 0;
        for(Literal literalObject: literals) {
            if(literalObject.multiplicity != limit) {
                auxiliaryLiterals.add(literalObject);
                multiplicities += literalObject.multiplicity;}}
        if(multiplicities >= limit) return false; // the remaining literals can satisfy the clause.

        for(Literal literalObject : auxiliaryLiterals) {
            removedLiterals.add(literalObject.literal);
            literals.remove(literalObject);
            literalObject.clause = null;}
        for(Literal literalObject : literals) literalObject.multiplicity = 1;
        limit = 1;
        isDisjunction = true;
        quantifier = Connective.OR;
        expandedSize = literals.size();
        hasMultiplicities = false;
        return true;}

    /** performs a merge resolution between this and a longer (or equally long) clause.
     * Example: atleast 2 p,q,r and atleast 4 -p^2 q^2,r,s yields <br>
     * atleast 4 q^3,r^2,s  (the quantifier is 2 + 4 - max(1,2)).<br>
     * The resolvent is not simplified.
     *
     * @param nextId         for getting the next clause identifier.
     * @param longerClause   the longer (or equally long) resolution partner.
     * @param literalShorter the Literal in the shorter clause to be ignored.
     * @param literalLonger  the Literal in the longer clause to be ignored.
     * @return               the new resolvent (not simplified).
     */
    protected Clause mergeResolution(IntSupplier nextId, Clause longerClause, Literal literalShorter, Literal literalLonger) {
        assert(longerClause.size() >= size());
        int newQuantifier = limit + longerClause.limit - Math.max(literalShorter.multiplicity,literalLonger.multiplicity);
        ArrayList<Literal> newLiterals = new ArrayList<>(longerClause.size()-1);
        for(Literal litLonger : longerClause.literals) {
            if(litLonger == literalLonger) continue;
            int literal = litLonger.literal;
            Literal litShorter = findLiteral(literal);
            int newMultiplicity = Math.min(newQuantifier, ((litShorter == null) ? litLonger.multiplicity :
                                                        litLonger.multiplicity + litShorter.multiplicity));
             newLiterals.add(new Literal(literal,newMultiplicity));}
        return new Clause(nextId.getAsInt(), newQuantifier,newLiterals);}

    /** returns the number of Literal objects in the clause.
     *
     * @return the number of Literal objects in the clause.
     */
    public int size() {return literals.size();}

    /** returns the sum of the literal's multiplicities.
     *
     * @return the sum of the literal's multiplicities.
     */
    public int expandedSize() {
        return expandedSize;}

    /** turns the clause into a string
     *
     * @return a string representation of the clause.
     */
    public String toString() {
        return toString(null,0);}

    /** turns the clause into a string
     *
     * @param symboltable null or a symboltable.
     * @param size 0 or the length of the clause number string.
     * @return a string representation of the clause.
     */
    public String toString(Symboltable symboltable, int size) {
        StringBuilder st = new StringBuilder();
        st.append((size == 0) ? id : String.format("%"+size+"s",id)).append(": ");
        if(quantifier == Connective.ATLEAST) st.append(">= ").append(limit).append(" ");
        int length = literals.size()-1;
        for(int i = 0; i < length; ++i) {
            st.append(literals.get(i).toString(symboltable)).append(quantifier.separator);}
        st.append(literals.get(length).toString(symboltable));
        return st.toString();}

}
