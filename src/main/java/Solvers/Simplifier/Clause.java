package Solvers.Simplifier;

import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Arrays;

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
        hasMultiplicities = expandedSize > literals.size();}

    /** This is a constructor for a disjunction.
     *
     * @param id             the identifier.
     * @param literalNumbers the literals of the disjunction.
     */
    public Clause(int id,  int... literalNumbers) {
        this.id = id;
        this.quantifier = Quantifier.OR;
        expandedSize = literalNumbers.length;
        for(int literal : literalNumbers) {
            Literal literalObject = new Literal(literal,1);
            literalObject.clause = this;
            literals.add(literalObject);}}


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
     * @param reduceLimit if true then the limit is reduced by the literal's multiplicity.
     * @return true if the clause still exists.
     */
    protected boolean removeLiteral(Literal literalObject, boolean reduceLimit) {
        if(reduceLimit) {
            limit -= literalObject.multiplicity;
            if(limit <= 0) {exists = false; return false;}}

        literals.remove(literalObject);
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

    /** merges multiple literals of disjunctions into one literal.
     */
    public void removeDoubleLiterals() {
        assert(quantifier == Quantifier.OR);
        for(int i = 0;  i < literals.size()-1; ++i) {
            int literal = literals.get(i).literal;
            for(int j = i+1; j < literals.size(); ++j) {
                if(literal == literals.get(j).literal) literals.remove(j--);}}}

    /** removes complementary pairs from the clause.
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
        if(limit == 1) {quantifier = Quantifier.OR; isDisjunction = true;}
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
            expandedSize -= literalObject.multiplicity;
            literalObject.clause = null;}
        if(limit <= 0) {
            exists = false;
            removedLiterals.addAll(literals);
            return auxiliaryLiterals;}

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


    /** replaces in a binary clause containing the given literal this literal by the representative.
     * The literal is destructively changed.<br>
     * If the two literals merge then the clause is not changed.
     *
     * @param representative the representative of an equivalence class.
     * @param literal        the literal of this representative.
     * @return               true if the two literals merge into one literal.
     */
    protected boolean replaceEquivalenceTwo(int representative, int literal) {
        assert(literals.size() == 2);
        Literal literalObject1 = literals.get(0);
        Literal literalObject2 = literals.get(1);
        if(literalObject1.literal == representative || literalObject2.literal == representative) {
            return true;}  // unit clause derived.
        if(literalObject1.literal == literal) literalObject1.literal = representative;
        else                                  literalObject2.literal = representative;
        return false;}

    /** replaces in a longer clause containing the given literal this literal by the representative.
     * The literal is destructively changed.<br>
     * If the two literals merge, one them is removed.
     *
     * @param representative the representative of an equivalence class.
     * @param literal        the literal of this representative.
     * @return               true if two literals merge into one literal.
     */
    protected boolean replaceEquivalenceMore(int representative, int literal) {
        assert(literals.size() > 2);
        Literal literalObject = findLiteral(literal);
        Literal representativeObject = findLiteral(representative);
        if(representativeObject != null) {
            literals.remove(literalObject);
            int combinedMultiplicity = representativeObject.multiplicity + literalObject.multiplicity;
            if(combinedMultiplicity > limit) expandedSize -= combinedMultiplicity - limit;
            representativeObject.multiplicity = Math.min(limit, combinedMultiplicity);
            if(representativeObject.multiplicity > 1) hasMultiplicities = true;
            return true;}
        literalObject.literal = representative;
        return false;}

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
