package Solvers.Simplifier;

import Datastructures.Symboltable;

/** This class is actually an index for the Literal objects.<br>
 * It contains for each predicate two doubly connected lists of Literal objects,
 * one for positive literals and one for negative literals.
 */
public class Literals {

    /** a doubly connected list for each positive literal */
    Literal[] positiveLiterals;

    /** a doubly connected list for each negative literal */
    Literal[] negativeLiterals;

    /** constructs a new index
     *
     * @param predicates the total number of predicates.
     */
    public Literals(int predicates) {
        positiveLiterals = new Literal[predicates+1];
        negativeLiterals = new Literal[predicates+1];}

    /** returns the first Literal object in the Literal chain of the given literal
     *
     * @param literal a literal
     * @return the first Literal object in the Literal chain of the given literal
     */
    public Literal getFirstLiteralObject(int literal) {
        return (literal > 0) ? positiveLiterals[literal] : negativeLiterals[-literal];}


    /** adds a literal object to the Literal index.
     *
     * @param literalObject the literal object to be added.
     */
    public void addLiteral(Literal literalObject) {
        int literal = literalObject.literal;
        int predicate = Math.abs(literal);
        Literal[] literals = (literal > 0) ? positiveLiterals : negativeLiterals;
        Literal firstLiteral = literals[predicate];
        literals[predicate] = literalObject;
        if(firstLiteral != null) {
            literalObject.nextLiteral = firstLiteral;
            firstLiteral.previousLiteral = literalObject;}}

    /** removes a literal object from the Literal index.
     *
     * @param literalObject the literal object to be added.
     * @return true if the chain of literals with the given literal is now empty.
     */
    public boolean removeLiteral(Literal literalObject) {
        int literal = literalObject.literal;
        int predicate = Math.abs(literal);
        Literal[] literals = (literal > 0) ? positiveLiterals : negativeLiterals;
        Literal previousLiteral = literalObject.previousLiteral;
        Literal nextLiteral     = literalObject.nextLiteral;
        if(previousLiteral == null) literals[predicate] = nextLiteral;
        else previousLiteral.nextLiteral = nextLiteral;
        if(nextLiteral != null) nextLiteral.previousLiteral = previousLiteral;
        literalObject.previousLiteral = null;
        literalObject.nextLiteral = null;
        return literals[predicate] == null;}

    /** removes the entire predicate from the Literals index.
     *
     * @param literal a literal to be removed.
     */
    public void removePredicate(int literal) {
        int predicate = Math.abs(literal);
        positiveLiterals[predicate] = null;
        negativeLiterals[predicate] = null;}

    /** checks if the literal chain for the given literal is empty.
     *
     * @param literal a literal
     * @return true if the literal chain for the given literal is empty.
     */
    public boolean isEmpty(int literal) {
        int predicate = Math.abs(literal);
        return literal > 0 ? positiveLiterals[predicate] == null : negativeLiterals[predicate] == null;
    }

    /** counts the number of literal objects for the given literal in the index.
     *
     * @param literal a literal.
     * @return the number of literal objects for the given literal in the index.
     */
    public int size(int literal) {
        int predicate = Math.abs(literal);
        Literal[] literals = (literal > 0) ? positiveLiterals : negativeLiterals;
        int size = 0;
        Literal literalObject = literals[predicate];
        while(literalObject != null) {
            ++size;
            literalObject = literalObject.nextLiteral;}
        return size;}

    /** generates a string representation of the Literal index.
     *
     * @return a string representation of the Literal index.
     */
    public String toString() {
        return toString(null);}

    /** generates a string representation of the Literal index.
     *
     * @param symboltable null or a symboltable.
     * @return a string representation of the Literal index.
     */
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("Positive Literals:\n");
        for(int predicate = 1; predicate <= positiveLiterals.length; ++predicate) {
            int size = size(predicate);
            if(size != 0) st.append(Symboltable.toString(predicate, symboltable)).append(":").append(size).append(",");}
        st.append("\nNegative Literals:\n");
        for(int predicate = 1; predicate <= negativeLiterals.length; ++predicate) {
            int size = size(-predicate);
            if(size != 0) st.append(Symboltable.toString(-predicate, symboltable)).append(":").append(size).append(",");}
        return st.toString();}


}
