package Solvers.Resolution;

import Datastructures.Symboltable;

/** This class is actually an index for the Literal objects.
 * <br>
 * It contains for each predicate two doubly connected lists of Literal objects,
 * one for positive literals and one for negative literals.
 */
public class Literals {

    /** a name for the index. */
    String name;

    /** the number of predicates for the index. */
    int predicates;

    /** a doubly connected list for each positive literal */
    Literal[] positiveLiterals;

    /** a doubly connected list for each negative literal */
    Literal[] negativeLiterals;

    /** the total number of predicates (positive + negative) with a non-empty index.
     * If entries == 0 then the entire index is empty.*/
    int entries = 0;

    /** constructs a new index
     *
     * @param name the name of the index.
     * @param predicates the total number of predicates.
     */
    public Literals(final String name, final int predicates) {
        this.name = name;
        this.predicates = predicates;
        positiveLiterals = new Literal[predicates+1];
        negativeLiterals = new Literal[predicates+1];}

    /** The arrays can be reused for new problems.
     * <br>
     * If the index is large enough, it is reused and all literals are removed, otherwise a new index is created.
     *
     * @param predicates the new number of predicates for the new version of the index.
     */
     void clear(final int predicates) {
        entries = 0;
        this.predicates = predicates;
        if(positiveLiterals.length <= predicates) {
            positiveLiterals = new Literal[predicates+1];
            negativeLiterals = new Literal[predicates+1];}
        else {
            for(int i = 0; i < positiveLiterals.length; ++i) {
                positiveLiterals[i] = null;
                negativeLiterals[i] = null;}}}

    /** returns the first Literal object in the Literal chain of the given literal.
     *
     * @param literal a literal
     * @return the first Literal object in the Literal chain of the given literal
     */
     Literal getFirstLiteralObject(final int literal) {
        return (literal > 0) ? positiveLiterals[literal] : negativeLiterals[-literal];}


    /** adds a literal object to the Literal index.
     * <br>
     * The new literal is added at the front of the list.
     *
     * @param literalObject the literal object to be added.
     */
     void addLiteral(final Literal literalObject) {
        int literal = literalObject.literal;
        int predicate = Math.abs(literal);
        final Literal[] literals = (literal > 0) ? positiveLiterals : negativeLiterals;
        final Literal firstLiteral = literals[predicate];
        if(firstLiteral == null) ++entries;
        literals[predicate] = literalObject;
        if(firstLiteral != null) {
            literalObject.nextLiteral = firstLiteral;
            firstLiteral.previousLiteral = literalObject;}}

    /** removes a literal object from the Literal index.
     * <br>
     * The removed literal's nextLiteral remains as it is.
     * This way forward iterations with a pointer pointing to the removed literal still work.<br>
     * One has to check literalObject.clause!<br>
     * The literalObject's previousLiteral is set to null.
     *
     * @param literalObject the literal object to be added.
     * @return true if the chain of literals with the given literal is now empty.
     */
     boolean removeLiteral(final Literal literalObject) {
        literalObject.clause = null;
        final Literal previousLiteral = literalObject.previousLiteral;
        final Literal nextLiteral     = literalObject.nextLiteral;
        final int literal = literalObject.literal;
        final int predicate = Math.abs(literal);
        final Literal[] literals = (literal > 0) ? positiveLiterals : negativeLiterals;

        if(previousLiteral == null) literals[predicate] = nextLiteral; // it was the first in the list.
        else previousLiteral.nextLiteral = nextLiteral;
        if(nextLiteral != null) nextLiteral.previousLiteral = previousLiteral;
        literalObject.previousLiteral = null;
        if(literals[predicate] == null) {--entries; return true;}
        return false;}


    /** removes the entire predicate from the Literals index.
     *
     * @param literal a literal to be removed.
     */
     void removePredicate(final int literal) {
        int predicate = Math.abs(literal);
        if(positiveLiterals[predicate] != null) --entries;
        if(negativeLiterals[predicate] != null) --entries;
        positiveLiterals[predicate] = null;
        negativeLiterals[predicate] = null;}

    /** checks if the literal chain for the given literal is empty.
     *
     * @param literal a literal
     * @return true if the literal chain for the given literal is empty.
     */
     boolean isEmpty(final int literal) {
        int predicate = Math.abs(literal);
        return literal > 0 ? positiveLiterals[predicate] == null : negativeLiterals[predicate] == null;
    }

    /** returns the number of non-null entries.
     * <br>
     * size() == 0 means the index is totally empty.
     *
     * @return the number of literals which have entries.
     */
    int size() {return entries;}

    /** counts the number of literal objects for the given literal in the index.
     * <br>
     * There is no counter. Therefor the method counts each time it is called.
     *
     * @param literal a literal.
     * @return the number of literal objects for the given literal in the index.
     */
     int size(final int literal) {
        int predicate = Math.abs(literal);
        Literal[] literals = (literal > 0) ? positiveLiterals : negativeLiterals;
        int size = 0;
        Literal literalObject = literals[predicate];
        while(literalObject != null) {
            ++size;
            literalObject = literalObject.nextLiteral;}
        return size;}

    /** counts the number of literal objects for the given literal in the index up to max.
     * <br>
     * Example: if max = 2, the counting stops when it reached 2 entries.
     *
     * @param literal a literal.
     * @param max the counting of the literals is done only up to this value.
     * @return -1 if the number of literal objects for the given literal in the index &gt; max, otherwise this number.
     */
     int size(final int literal, final int max) {
        int predicate = Math.abs(literal);
        final Literal[] literals = (literal > 0) ? positiveLiterals : negativeLiterals;
        int size = 0;
        Literal literalObject = literals[predicate];
        while(literalObject != null) {
            if(++size > max) return -1 ;
            literalObject = literalObject.nextLiteral;}
        return size;}

    /** checks if the literalObject is in the right place of the index.
     *
     * @param literalObject a Literal
     * @return true if the literalObject is in the right place.
     */
    boolean contains(final Literal literalObject) {
        final Literal[] literals = (literalObject.literal > 0) ? positiveLiterals : negativeLiterals;
        Literal litObject = literals[Math.abs(literalObject.literal)];
        while(litObject != null) {
            if(litObject == literalObject) return true;
            litObject = litObject.nextLiteral;}
        return false;
    }

    /** checks the consistency of the index.
     * <br>
     * - is the literal's clause still defined. <br>
     * - is the literal in the right predicate slot.<br>
     * - if size &gt; 0: has the literal's clause the right size?<br>
     * This can be used for a literal index for clauses with a fixed size.<br>
     * An inconsistency in the index causes a stacktrace to be printed, and the system stops.
     *
     * @param size 0 or the clause's size.
     * @param text an error text.
     */
    void checkConsistency(int size, String text) {
        for(int predicate = 1; predicate < predicates; ++predicate) {
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
    public String toString(final Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("Literals ").append(name).append(":\n");
        st.append("Positive Literals:\n");
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            int size = size(predicate);
            if(size != 0) st.append(Symboltable.toString(predicate, symboltable)).append(":").append(size).append(",");}
        st.append("\nNegative Literals:\n");
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            int size = size(-predicate);
            if(size != 0) st.append(Symboltable.toString(-predicate, symboltable)).append(":").append(size).append(",");}
        return st.toString();}


}
