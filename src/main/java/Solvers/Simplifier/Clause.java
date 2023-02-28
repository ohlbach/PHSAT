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
    protected Connective connective;

    /** true if the connective is OR. */
    protected boolean isDisjunction;

    /** the quantifier for ATLEAST clauses. */
    protected int quantifier = 1;

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
        connective = Connective.getConnective(inputClause[1]);
        assert(connective != null && (connective == Connective.OR || connective == Connective.ATLEAST));
        isDisjunction = connective == Connective.OR;
        quantifier = isDisjunction ? 1 : inputClause[2];
        inferenceStep = new InfInputClause(id);
        int length = inputClause.length;
        int start = connective.firstLiteralIndex;
        literals = new ArrayList<>(length-start);
        if(isDisjunction)
            for(int i = start; i < length; ++i) {
                Literal literalObject = new Literal(inputClause[i],1);
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
                hasMultiplicities |= multiplicity > 1;
                Literal literalObject = new Literal(literal1,multiplicity);
                literalObject.clause = this;
                literals.add(literalObject);}}}

    /** constructs a new clause from a list of literal,multiplicity pairs.
     *
     * @param id         the identifier.
     * @param connective the connective.
     * @param quantifier the quantifier.
     * @param items      pairs literal,multiplicity.
     */
    public Clause(int id, Connective connective, int quantifier, int... items) {
        this.id = id;
        this.quantifier = quantifier;
        this.connective = connective;
        isDisjunction = connective == Connective.OR;
        literals = new ArrayList<>(items.length/2);
        for(int i = 0; i < items.length; i +=2) {
            int literal = items[i];
            int multiplicity = Math.min(items[i+1],quantifier);
            expandedSize += multiplicity;
            hasMultiplicities |= multiplicity > 1;
            Literal literalObject = new Literal(literal,multiplicity);
            literalObject.clause = this;
            literals.add(literalObject);}}

    /** This is a constructor for a disjunction.
     *
     * @param id             the identifier.
     * @param literalNumbers the literals of the disjunction.
     */
    public Clause(int id,  int... literalNumbers) {
        this.id = id;
        this.connective = Connective.OR;
        expandedSize = literalNumbers.length;
        for(int literal : literalNumbers) {
            Literal literalObject = new Literal(literal,1);
            literalObject.clause = this;
            literals.add(literalObject);}}

    public Clause(int id, int quantifier, ArrayList<Literal> literals) {
        this.id = id;
        this.quantifier = quantifier;
        this.literals = literals;
        expandedSize = 1;
        hasMultiplicities = false;

        for(Literal literalOject: literals) {
            literalOject.clause = this;
            hasMultiplicities |= literalOject.multiplicity > 1;
            expandedSize += literalOject.multiplicity;}

        if(quantifier == 1) {
            connective = Connective.OR;
            isDisjunction = true;}
        else {
            connective = Connective.ATLEAST;
            isDisjunction = false;}}


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
     * All internal data are updated (quantifier, multiplicity of the literals, connective).<br>
     * If the clause is a disjunction then the literal is just removed.<br>
     * If the clause is an atleast-clause then the literals multiplicities are adjusted to the quantifier.<br>
     * If necessary, the remaining multiplicities are divided by their GCD.<br>
     * If the resulting quantifier is 1 then the clause is turned into a disjunction.<br>
     * True literals are not extracted.
     *
     * @param literalObject    the literal to be removed.
     * @param reduceQuantifier if true then the quantifier is reduced by the literal's multiplicity.
     * @return true if the clause has become a tautology (quantifier = 0).
     */
    protected boolean removeLiteral(Literal literalObject, boolean reduceQuantifier) {
        literals.remove(literalObject);
        expandedSize -= literalObject.multiplicity;
        literalObject.clause = null;
        if(isDisjunction) return false;
        if(reduceQuantifier) {
            quantifier -= literalObject.multiplicity;
            if(quantifier <= 0) return true;
            if(quantifier <= 1) {
                isDisjunction = true;
                connective = Connective.OR;
                for(Literal literalObject1 : literals) {literalObject1.multiplicity = 1;}
                expandedSize = literals.size();
                hasMultiplicities = false;
                return false;}}
        boolean gcdUseful = quantifier > 1;
        for(Literal literalObject1 : literals) {
            int multiplicity = literalObject1.multiplicity;
            if(multiplicity == 1) {gcdUseful = false; continue;}
            if(multiplicity > quantifier) {
                expandedSize -= quantifier-multiplicity;
                literalObject1.multiplicity = quantifier;}}
        if(gcdUseful) divideByGCD();
        hasMultiplicities = expandedSize > literals.size();
        return false;}

    /** removes all given literals from the clause and reduces the quantifier.
     * To be used for the literals found by findTrueLiterals.<br>
     * The multiplicities of the remaining literals are adjusted to the quantifier
     * and, if necessary, the numbers are divided by their GCD.
     *
     * @param literalObjects which are to be removed from the clause.
     * @return true if the clause has become true (tautology).
     */
    protected boolean removeLiterals(ArrayList<Literal> literalObjects) {
        for(Literal literalObject : literalObjects) {
            literals.remove(literalObject);
            quantifier -= literalObject.multiplicity;
            expandedSize -= literalObject.multiplicity;}
        if(quantifier <= 0) return true;
        for(Literal literalObject : literals) {
            if(literalObject.multiplicity > quantifier) {
                expandedSize -= literalObject.multiplicity - quantifier;
                literalObject.multiplicity = quantifier;}}
        if(quantifier <= 1) {
            isDisjunction = true;
            connective = Connective.OR;}
        hasMultiplicities = expandedSize > literals.size();
        if(hasMultiplicities && quantifier > 1) divideByGCD();
        return quantifier <= 0;}

    private IntArrayList numbers = new IntArrayList();

    /** divides the quantifier and the multiplicities by their greatest common divisor.
     * @return true if the clause is changed.
     */
    protected boolean divideByGCD() {
        numbers.clear(); numbers.add(quantifier);
        boolean stop = false;
        for(Literal literalObject : literals) {
            int multiplicity = literalObject.multiplicity;
            if(multiplicity == 1) {stop = true; break;}
            numbers.add(multiplicity);}
        if(!stop) {
            int gcd = Utilities.gcd(numbers);
            if(gcd > 1) {
                expandedSize = 0;
                quantifier /= gcd;
                for(Literal literalObject : literals) {
                    literalObject.multiplicity = Math.min(quantifier, literalObject.multiplicity / gcd);
                    expandedSize += literalObject.multiplicity;}
                if(quantifier == 1) {connective = Connective.OR; isDisjunction = true;}
            return true;}}
        return false;}


    /** finds a literal which must be true in an ATLEAST-clause. <br>
     *  Example: atleast 3 p^2,q^2.<br>
     *  If p is false then the clause becomes atleast 3 q^2, which is no longer satisfiable.
     *  Therefore, p must be true (and q as well.)
     *
     * @param trueLiterals a list for collecting the true literals.
     * @return the sum of the multiplicities which can be deleted in ATLEAST-clause.
     */
    protected int findTrueLiterals(ArrayList<Literal> trueLiterals) {
        trueLiterals.clear();
        if(!hasMultiplicities) return 0;
        int multiplicities = 0;
        for(Literal literalObject: literals) {
            if(expandedSize-literalObject.multiplicity < quantifier) {
                multiplicities += literalObject.multiplicity;
                trueLiterals.add(literalObject);}}
        return multiplicities;}

    /** removes literals which must be true in an ATLEAST-clause. <br>
     *  Example: atleast 4 p^2,q^2,r.<br>
     *  If p is false then the clause becomes atleast 4 q^2,r, which is no longer satisfiable.<br>
     *  Therefore, p must be true, and q as well.<br>
     *  Both can be removed and made true.<br>
     *  The resulting clause may get a quantifier &lt;= 0. It is true and can be removed.
     *
     * @return the literals which must be true.
     */
    private final ArrayList<Literal> auxiliaryLiterals = new ArrayList<>(5);
    protected ArrayList<Literal> reduceByTrueLiterals() {
        if(!hasMultiplicities) return null;
        auxiliaryLiterals.clear();
        for(Literal literalObject: literals) {
            if(expandedSize-literalObject.multiplicity < quantifier) {
                auxiliaryLiterals.add(literalObject);}}
        if(auxiliaryLiterals.isEmpty()) return null;

        for(Literal literalObject: auxiliaryLiterals) {
            literals.remove(literalObject);
            quantifier   -= literalObject.multiplicity;
            expandedSize -= literalObject.multiplicity;
            literalObject.clause = null;}
        if(quantifier <= 0) return auxiliaryLiterals;

        for(Literal literalObject: literals) {
            if(literalObject.multiplicity > quantifier) {
                literalObject.multiplicity = quantifier;
                expandedSize -= literalObject.multiplicity - quantifier;}}
        if(quantifier == 1) {
            isDisjunction = true;
            connective = Connective.OR;
            hasMultiplicities = false;}
        else {hasMultiplicities = expandedSize > literals.size();}

        return auxiliaryLiterals;}


    /** shrinks the literals to the essential literals, if possible, and turns the clause to a disjunction.<br>
     * Example: atleast 2 p^2,q^2,r.<br>
     * In this case either p or q must be true. If both are false then r is not enough to get 2 true literals.<br>
     * The clause shrinks to the disjunction: p,q.
     *
     * @return the removed Literals. (They must  be removed from literal indices).
     */
    protected ArrayList<Literal> reduceToEssentialLiterals() {
        if(!hasMultiplicities) return null;
        auxiliaryLiterals.clear();
        int multiplicities = 0;
        for(Literal literalObject: literals) {
            if(literalObject.multiplicity != quantifier) {
                auxiliaryLiterals.add(literalObject);
                multiplicities += literalObject.multiplicity;}}
        if(multiplicities >= quantifier) return null; // the remaining literals can satisfy the clause.

        for(Literal literalObject : auxiliaryLiterals) {
            literals.remove(literalObject);
            literalObject.clause = null;}
        for(Literal literalObject : literals) literalObject.multiplicity = 1;
        quantifier = 1;
        isDisjunction = true;
        connective = Connective.OR;
        expandedSize = literals.size();
        hasMultiplicities = false;
        return auxiliaryLiterals;}

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
        int newQuantifier = quantifier + longerClause.quantifier - Math.max(literalShorter.multiplicity,literalLonger.multiplicity);
        ArrayList<Literal> newLiterals = new ArrayList<>(longerClause.size()-1);
        for(Literal litLonger : longerClause.literals) {
            if(litLonger == literalLonger) continue;
            int literal = litLonger.literal;
            Literal litShorter = findLiteral(literal);
            int newMultiplicity = Math.min(newQuantifier, ((litShorter == null) ? litLonger.multiplicity :
                                                        litLonger.multiplicity + litShorter.multiplicity));
             newLiterals.add(new Literal(literal,newMultiplicity));}
        Clause resolvent = new Clause(nextId.getAsInt(), newQuantifier,newLiterals);
        return resolvent;}

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
        if(connective == Connective.ATLEAST) st.append(">= ").append(quantifier).append(" ");
        int length = literals.size()-1;
        for(int i = 0; i < length; ++i) {
            st.append(literals.get(i).toString(symboltable)).append(connective.separator);}
        st.append(literals.get(length).toString(symboltable));
        return st.toString();}

}
