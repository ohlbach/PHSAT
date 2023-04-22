package Solvers.Backtracker;

import Datastructures.Symboltable;

/** This class is actually an index for the Literal objects.
 * <br>
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
    Literals() {}

    /** resets the lists such that they can be used for another QuSat problem
     *
     * @param predicates the number of predicates.
     */
    void reset(int predicates) {
        if(positiveLiterals == null || positiveLiterals.length < predicates+1) {
            positiveLiterals = new Literal[predicates+1];
            negativeLiterals = new Literal[predicates+1];
            return;}
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            positiveLiterals[predicate] = null;
            negativeLiterals[predicate] = null;}}

    /** removes all literals from the index.
     * This is mainly for testing purposes.
     */
    public void clear() {
        for(int i = 0; i < positiveLiterals.length; ++i) {
            positiveLiterals[i] = null;
            negativeLiterals[i] = null;}}

    /** returns the first Literal object in the Literal chain of the given literal
     *
     * @param literal a literal
     * @return the first Literal object in the Literal chain of the given literal
     */
    Literal getFirstLiteralObject(int literal) {
        return (literal > 0) ? positiveLiterals[literal] : negativeLiterals[-literal];}



    /** adds a literal object to the Literal index.
     *
     * @param literalObject the literal object to be added.
     */
    void addLiteral(Literal literalObject) {
        int literal = literalObject.literal;
        literalObject.nextLiteral = null;
        int predicate = Math.abs(literal);
        Literal[] literals = (literal > 0) ? positiveLiterals : negativeLiterals;
        Literal firstLiteral = literals[predicate];
        literals[predicate] = literalObject;
        if(firstLiteral != null) {
            literalObject.nextLiteral = firstLiteral;
            firstLiteral.previousLiteral = literalObject;}}

    /** removes a literal object from the Literal index.
     * The removed literal's nextLiteral remains as it is.
     * This way forward iterations with a pointer pointing to the removed literal still work.<br>
     * One has to check literalObject.clause!
     *
     * @param literalObject the literal object to be added.
     * @return true if the chain of literals with the given literal is now empty.
     */
    boolean removeLiteral(Literal literalObject) {
        literalObject.clause = null;
        Literal previousLiteral = literalObject.previousLiteral;
        Literal nextLiteral     = literalObject.nextLiteral;
        literalObject.previousLiteral = null;
        int literal = literalObject.literal;
        int predicate = Math.abs(literal);
        Literal[] literals = (literal > 0) ? positiveLiterals : negativeLiterals;

        if(previousLiteral == null) { // front literal
            literals[predicate] = nextLiteral;
            if(nextLiteral != null) nextLiteral.previousLiteral = null;
            else literalObject.nextLiteral = null;
            return literals[predicate] == null;}

        if(nextLiteral == null) {  // last literal
            previousLiteral.nextLiteral = null;
            literalObject.nextLiteral = null;
            return false;}

        previousLiteral.nextLiteral = nextLiteral;
        nextLiteral.previousLiteral = previousLiteral;
        literalObject.previousLiteral = null;
        return false;}

    /** updates the index position after a literalObject's literal has been changed.
     *
     * @param literalObject the literalObject with a new literal.
     * @param oldLiteral    the previous literal of the literalObject.
     * @return              true if the oldLiteral's position became empty.
     */
    boolean replaceLiteral(Literal literalObject, int oldLiteral) {
        int newLiteral = literalObject.literal;
        literalObject.literal = oldLiteral;
        boolean isEmpty = removeLiteral(literalObject);
        literalObject.literal = newLiteral;
        addLiteral(literalObject);
        return isEmpty;}

    /** removes the entire predicate from the Literals index.
     *
     * @param literal a literal to be removed.
     */
    void removePredicate(int literal) {
        int predicate = Math.abs(literal);
        positiveLiterals[predicate] = null;
        negativeLiterals[predicate] = null;}

    /** checks if the literal chain for the given literal is empty.
     *
     * @param literal a literal
     * @return true if the literal chain for the given literal is empty.
     */
    boolean isEmpty(int literal) {
        int predicate = Math.abs(literal);
        return literal > 0 ? positiveLiterals[predicate] == null : negativeLiterals[predicate] == null;
    }

    /** counts the number of literal objects for the given literal in the index.
     *
     * @param literal a literal.
     * @return the number of literal objects for the given literal in the index.
     */
    int size(int literal) {
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
        for(int predicate = 1; predicate < positiveLiterals.length; ++predicate) {
            int size = size(predicate);
            if(size != 0) st.append(Symboltable.toString(predicate, symboltable)).append(":").append(size).append(",");}
        st.append("\nNegative Literals:\n");
        for(int predicate = 1; predicate < negativeLiterals.length; ++predicate) {
            int size = size(-predicate);
            if(size != 0) st.append(Symboltable.toString(-predicate, symboltable)).append(":").append(size).append(",");}
        return st.toString();}

    /** collects the index entries for a particular literal in a string with entries literal@clause-id.
     *
     * @param literal     a literal
     * @return the index entries for a particular literal in a string with entries literal@clause-id.
     */
    public String toString(int literal) {
        return toString(literal,null);}

    /** collects the index entries for a particular literal in a string with entries literal@clause-id.
     *
     * @param literal     a literal
     * @param symboltable null or a symboltable
     * @return the index entries for a particular literal in a string with entries literal@clause-id.
     */
    public String toString(int literal, Symboltable symboltable) {
        Literal[] literals = (literal > 0) ? positiveLiterals : negativeLiterals;
        Literal literalObject = literals[Math.abs(literal)];
        if(literalObject == null) return "";
        StringBuilder st = new StringBuilder();
        while(literalObject != null) {
            Clause clause = literalObject.clause;
            int multiplicity = literalObject.multiplicity;
            st.append(Symboltable.toString(literalObject.literal,symboltable)).append(multiplicity == 1 ? "":"^"+multiplicity).
                    append("@").append(clause == null ? "0":literalObject.clause.id).append(",");
            literalObject = literalObject.nextLiteral;}
        return st.toString();}


}
