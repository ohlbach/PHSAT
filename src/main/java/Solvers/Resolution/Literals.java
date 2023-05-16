package Solvers.Resolution;

import Datastructures.Symboltable;

/** This class is actually an index for the Literal objects.<br>
 * It contains for each predicate two doubly connected lists of Literal objects,
 * one for positive literals and one for negative literals.
 */
public class Literals {

    String name;

    int predicates;

    /** a doubly connected list for each positive literal */
    Literal[] positiveLiterals;

    /** a doubly connected list for each negative literal */
    Literal[] negativeLiterals;

    int entries = 0;

    /** constructs a new index
     *
     * @param name the name of the index.
     * @param predicates the total number of predicates.
     */
    public Literals(String name, int predicates) {
        this.name = name;
        this.predicates = predicates;
        positiveLiterals = new Literal[predicates+1];
        negativeLiterals = new Literal[predicates+1];}

    /** If the index is large enough, it is reused and all literals are removed, otherwise a new index is created.
     */
    public void clear(int predicates) {
        entries = 0;
        this.predicates = predicates;
        if(positiveLiterals.length <= predicates) {
            positiveLiterals = new Literal[predicates+1];
            negativeLiterals = new Literal[predicates+1];}
        else {
            for(int i = 0; i < positiveLiterals.length; ++i) {
                positiveLiterals[i] = null;
                negativeLiterals[i] = null;}}}

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
        if(firstLiteral == null) ++entries;
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
        Clause clause = literalObject.clause;
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
        if(literals[predicate] == null) {--entries; return true;}
        return false;}


    /** removes the entire predicate from the Literals index.
     *
     * @param literal a literal to be removed.
     */
    public void removePredicate(int literal) {
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
    public boolean isEmpty(int literal) {
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

    /** counts the number of literal objects for the given literal in the index up to max.
     *
     * @param literal a literal.
     * @return -1 if the number of literal objects for the given literal in the index &gt; max, otherwise this number.
     */
    public int size(int literal, int max) {
        int predicate = Math.abs(literal);
        Literal[] literals = (literal > 0) ? positiveLiterals : negativeLiterals;
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
