package Datastructures;

/**
 * This class represents an index of literals.
 * It stores positive and negative literals separately in linked item lists for efficient retrieval and removal.
 * This class is parameterized with a Literal data structure.
 *
 * @param <Literal> the type of literal stored in the index
 */
public class LiteralIndex<Literal extends Datastructures.Literal> {
    /** maps positive literals to literal occurrences */
    private LinkedItemList[] positiveOccurrences;
    /** maps negative literals to literal occurrences */
    private LinkedItemList[] negativeOccurrences;

    /**
     * Constructs an instance of LiteralIndex with the given number of predicates.
     *
     * @param predicates the number of predicates
     */
    public LiteralIndex(int predicates) {
        positiveOccurrences = new LinkedItemList[predicates];
        negativeOccurrences = new LinkedItemList[predicates];
        for (int i = 0; i < predicates; i++) {
            positiveOccurrences[i] = new LinkedItemList<>("Positive Literals");
            negativeOccurrences[i] = new LinkedItemList<>("Negative Literals");}}

    /**
     * Adds a literal object to the back of the positiveOccurrences or negativeOccurrences list based on the value of the literal.
     *
     * @param literalObject the literal object to be added
     */
    public void addToBack(Literal literalObject) {
        int literal = literalObject.literal;
        if(literal > 0) positiveOccurrences[literal].addToBack(literalObject);
        else            negativeOccurrences[-literal].addToBack(literalObject);}

    /**
     * Adds a literal object to the front of the positiveOccurrences or negativeOccurrences list based on the value of the literal.
     *
     * @param literalObject the literal object to be added
     */
    public void addToFront(Literal literalObject) {
        int literal = literalObject.literal;
        if(literal > 0) positiveOccurrences[literal].addToFront(literalObject);
        else            negativeOccurrences[-literal].addToFront(literalObject);}

    /**
     * Removes a `Literal` object from the `positiveOccurrences` or `negativeOccurrences` list based on the value of the `literal` property.
     *
     * @param literalObject the `Literal` object to be removed
     */
    public void remove(Literal literalObject) {
        int literal = literalObject.literal;
        if(literal > 0) positiveOccurrences[literal].remove(literalObject);
        else            negativeOccurrences[-literal].remove(literalObject);}

    /**
     * Removes all occurrences of a literal from the positiveOccurrences or negativeOccurrences list based on the value of the literal.
     *
     * @param literal the literal to be removed
     */
    public void removeLiteral(int literal) {
        if(literal > 0) positiveOccurrences[literal].clear();
        else            negativeOccurrences[-literal].clear();}

    /**
     * Removes all occurrences of a predicate from the positiveOccurrences and negativeOccurrences list.
     *
     * @param predicate the predicate to be removed
     */
    public void removePtredicate(int predicate) {
        predicate = Math.abs(predicate);
        positiveOccurrences[predicate].clear();
        negativeOccurrences[predicate].clear();}

    /**
     * Retrieves the first occurrence of a literal from the positiveOccurrences or negativeOccurrences list based on the value of the literal.
     *
     * @param literal The value of the literal to retrieve.
     * @return The first occurrence of the literal as a Literal object if it exists, null otherwise.
     */
    public Literal getFirstLiteral(int literal) {
        return (literal > 0) ?
                (Literal)positiveOccurrences[literal].firstLinkedItem:
                (Literal)negativeOccurrences[-literal].firstLinkedItem;}

    /**
     * Retrieves the last occurrence of a literal from the positiveOccurrences or negativeOccurrences list based on the value of the literal.
     *
     * @param literal The value of the literal to retrieve.
     * @return The last occurrence of the literal as a Literal object if it exists, null otherwise.
     */
    public Literal getLastLiteral(int literal) {
        return (literal > 0) ?
                (Literal)positiveOccurrences[literal].lastLinkedItem:
                (Literal)negativeOccurrences[-literal].lastLinkedItem;}

    /**
     * Checks if the list of literals is empty based on the given literal value.
     *
     * @param literal The value of the literal.
     * @return true if the list is empty, false otherwise.
     */
    public boolean isEmpty(int literal) {
        return (literal > 0) ?
                positiveOccurrences[literal].isEmpty() :
                negativeOccurrences[-literal].isEmpty();
    }

    /**
     * Retrieves the size of the list of literals in the LiteralIndex class based on the given literal value.
     *
     * @param literal The value of the literal.
     * @return The size of the list of literals if it exists and greater than 0, 0 otherwise.
     */
    public int size(int literal) {
        return (literal > 0) ?
                positiveOccurrences[literal].size():
                negativeOccurrences[-literal].size();}


}
