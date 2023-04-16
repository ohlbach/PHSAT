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
     * The removed literal's nextLiteral remains as it is.
     * This way forward iterations with a pointer pointing to the removed literal still work.<br>
     * One has to check literalObject.clause!
     *
     * @param literalObject the literal object to be added.
     * @return true if the chain of literals with the given literal is now empty.
     */
    public boolean removeLiteral(Literal literalObject) {
        literalObject.clause = null;
        Literal previousLiteral = literalObject.previousLiteral;
        Literal nextLiteral     = literalObject.nextLiteral;
        int literal = literalObject.literal;
        int predicate = Math.abs(literal);
        Literal[] literals = (literal > 0) ? positiveLiterals : negativeLiterals;

        if(previousLiteral == null) literals[predicate] = nextLiteral;
        else previousLiteral.nextLiteral = nextLiteral;
        if(nextLiteral != null) nextLiteral.previousLiteral = previousLiteral;
        literalObject.previousLiteral = null;
        return literals[predicate] == null;}

    /** updates the index position after a literalObject's literal has been changed.
     *
     * @param literalObject the literalObject with a new literal.
     * @param oldLiteral    the previous literal of the literalObject.
     * @return              true if the oldLiteral's position became empty.
     */
    public boolean replaceLiteral(Literal literalObject, int oldLiteral) {
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

    /** checks if the literalObject is in the right place of the index.
     *
     * @param literalObject a Literal
     * @return true if the literalObject is in the right place.
     */
    boolean contains(Literal literalObject) {
        Literal[] literals = (literalObject.literal > 0) ? positiveLiterals : negativeLiterals;
        Literal litObject = literals[Math.abs(literalObject.literal)];
        while(litObject != null) {
            if(litObject == literalObject) return true;
            litObject = litObject.nextLiteral;}
        return false;
    }

    /** checks the consistency of the index.
     * - is the literal's clause still defined. <br>
     * - is the literal in the right predicate slot.<br>
     * - if size &gt; 0: has the literal's clause the right size,
     *
     * @param size 0 or the clause size.
     * @param text an error text.
     */
    void checkConsistency(int size, String text) {
        for(int predicate = 1; predicate < positiveLiterals.length; ++predicate) {
            Literal literalObject = positiveLiterals[predicate];
            while(literalObject != null) {
                if(literalObject.clause == null) {
                    System.out.println("Error "+text+" literal " + predicate + " has no clause.");
                    new Exception().printStackTrace();
                    System.exit(1);}
                if(literalObject.literal != predicate) {
                    System.out.println("Error "+text+" literal " + literalObject.literal + " is in wrong predicate " + predicate);
                    new Exception().printStackTrace();
                    System.exit(1);}
                if(size > 0 && literalObject.clause.size() != size) {
                    System.out.println("Error "+text+" literal " + literalObject.literal + " is in the wrong index " + size);
                    new Exception().printStackTrace();
                    System.exit(1);}
                literalObject = literalObject.nextLiteral;}

            literalObject = negativeLiterals[predicate];
            while(literalObject != null) {
                if(literalObject.clause == null) {
                    System.out.println("Error "+text+" literal " + -predicate + " has no clause.");
                    new Exception().printStackTrace();
                    System.exit(1);}
                if(literalObject.literal != -predicate) {
                    System.out.println("Error "+text+" literal " + literalObject.literal + " is in wrong predicate " + -predicate);
                    new Exception().printStackTrace();
                    System.exit(1);}
                if(size > 0 && literalObject.clause.size() != size) {
                    System.out.println("Error "+text+" literal " + literalObject.literal + " is in the wrong index " + size);
                    new Exception().printStackTrace();
                    System.exit(1);}
                literalObject = literalObject.nextLiteral;}
            }}


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


}
