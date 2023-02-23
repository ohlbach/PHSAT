package Solvers.Simplifier;

import Datastructures.Clauses.Connective;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;

import java.util.ArrayList;
import java.util.Arrays;

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
    protected ArrayList<Literal> literals;

    protected boolean exists = true;

    /** the inference step which caused the derivation of this clause. */
    protected InferenceStep inferenceStep;

    /** a timstamp to be used by various algorithms. */
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
                Literal literalObject = new Literal(literal1,multiplicity);
                literalObject.clause = this;
                literals.add(literalObject);}}}

    /** constructs a new clause from a list of literal,multiplicity pairs.
     *
     * @param id         the identifier
     * @param quantifier the quantifier
     * @param items      pairs literal,multiplicity
     */
    public Clause(int id, int quantifier, int... items) {
        this.id = id;
        this.quantifier = quantifier;
        connective = quantifier == 1 ? Connective.OR : Connective.ATLEAST;
        isDisjunction = connective == Connective.OR;
        literals = new ArrayList<>(items.length/2);
        for(int i = 0; i < items.length; i +=2) {
            int literal = items[i];
            int multiplicity = Math.min(items[i+1],quantifier);
            expandedSize += multiplicity;
            Literal literalObject = new Literal(literal,multiplicity);
            literalObject.clause = this;
            literals.add(literalObject);}}

    /** removes a literal object from the clause.<br>
     * All internal data are updated (quantifier, multiplicity of the literals, connective)
     *
     * @param literalObject    the literal to be removed
     * @param reduceQuantifier if true then the quantifier is reduced by the literal's multiplicity.
     */
    protected void removeLiteral(Literal literalObject, boolean reduceQuantifier) {
        literals.remove(literalObject);
        expandedSize -= literalObject.multiplicity;
        literalObject.clause = null;
        if(!isDisjunction && reduceQuantifier) {
            quantifier -= literalObject.multiplicity;
            if(quantifier == 1) {
                isDisjunction = true;
                connective = Connective.OR;
                for(Literal literalObject1 : literals) {literalObject1.multiplicity = 1;}
                expandedSize = literals.size();
                return;}
            for(Literal literalObject1 : literals) {
                int multiplicity = literalObject1.multiplicity;
                if(multiplicity > quantifier) {
                    expandedSize -= quantifier-multiplicity;
                    literalObject1.multiplicity = quantifier;}}}}

    /** finds a literal which must be true in an ATLEAST-clause. <br>
     *  Example: atleast 3 p^2,q^2.<br>
     *  If p is false then the clause became atleast 3 q^2, which is no longer satisfiable.
     *  Therefore, p must be true (and q as well.)
     *
     * @return the first literal which must be true in an ATLEAST-clause.
     */
    protected int findTrueLiteral() {
        for(Literal literalObject: literals) {
            if(expandedSize-literalObject.multiplicity < quantifier) {
                return literalObject.literal;}}
        return 0;}



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

    public String toString() {
        return toString(null,0);}

    public String toString(Symboltable symboltable, int size) {
        StringBuilder st = new StringBuilder();
        st.append(String.format("%"+size+"s",id));
        st.append(id).append(":");
        int length = literals.size()-1;
        for(int i = 0; i < length; ++i) {
            st.append(literals.get(i).toString(symboltable)).append(connective.separator);}
        st.append(literals.get(length).toString(symboltable));
        return st.toString();}

}
