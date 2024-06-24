package Datastructures;

/**
 * This class represents an index of predicates.
 * It stores positive and negative predicates separately in linked item lists for efficient retrieval and removal.
 * This class is parameterized with a Literal data structure.
 *
 * @param <Literal> the type of literal stored in the index
 */
public class LiteralIndex<Literal extends Datastructures.Literal> {
    /** maps positive predicates to literal occurrences */
    private LinkedItemList[] positiveOccurrences;
    /** maps negative predicates to literal occurrences */
    private LinkedItemList[] negativeOccurrences;

    private static String positiveTitle = "Positive Literals";
    private static String negativeTitle = "Negative Literals";
    /**
     * Constructs an instance of LiteralIndex with the given number of predicates.
     *
     * @param predicates the number of predicates
     */
    public LiteralIndex(int predicates) {
        positiveOccurrences = new LinkedItemList[predicates+1];
        negativeOccurrences = new LinkedItemList[predicates+1];
        for (int i = 1; i <= predicates; i++) {
            positiveOccurrences[i] = new LinkedItemList<>(positiveTitle);
            negativeOccurrences[i] = new LinkedItemList<>(negativeTitle);}}

    /** increases the size of the LiteralIndex if necessary.
     *
     * @param predicates the number of predicates.
     */
    public void ensureCapacity(int predicates) {
        if(positiveOccurrences.length < predicates) {
            positiveOccurrences = new LinkedItemList[predicates];
            negativeOccurrences = new LinkedItemList[predicates];}
        else {
            for(int predicate = 1; predicate <= predicates; ++predicate) {
                positiveOccurrences[predicate] = null;
                negativeOccurrences[predicate] = null;}}}

    /**
     * Adds a literal object to the back of the positiveOccurrences or negativeOccurrences list based on the value of the literal.
     *
     * @param literalObject the literal object to be added
     */
    public void addToBack(Literal literalObject) {
        int literal = literalObject.literal;
        LinkedItemList<Literal> literals = (literal > 0) ? positiveOccurrences[literal] : negativeOccurrences[-literal];
        if(literals == null) {
            if(literal > 0) {literals = new LinkedItemList<>(positiveTitle); positiveOccurrences[literal] = literals;}
            else            {literals = new LinkedItemList<>(negativeTitle);negativeOccurrences[-literal] = literals;}}
        literals.addToBack(literalObject);}

    /**
     * Adds a literal object to the front of the positiveOccurrences or negativeOccurrences list based on the value of the literal.
     *
     * @param literalObject the literal object to be added
     */
    public void addToFront(Literal literalObject) {
        int literal = literalObject.literal;
        LinkedItemList<Literal> literals = (literal > 0) ? positiveOccurrences[literal] : negativeOccurrences[-literal];
        if(literals == null) {
            if(literal > 0) {literals = new LinkedItemList<>(positiveTitle); positiveOccurrences[literal] = literals;}
            else            {literals = new LinkedItemList<>(negativeTitle);negativeOccurrences[-literal] = literals;}}
        literals.addToFront(literalObject);}

    /**
     * Removes a `Literal` object from the `positiveOccurrences` or `negativeOccurrences` list based on the value of the `literal` property.
     *
     * @param literalObject the `Literal` object to be removed
     */
    public void remove(Literal literalObject) {
        int literal = literalObject.literal;
        LinkedItemList<Literal> literals = (literal > 0) ? positiveOccurrences[literal] : negativeOccurrences[-literal];
        if(literals != null) literals.remove(literalObject);}

    /**
     * Removes all occurrences of a literal from the positiveOccurrences or negativeOccurrences list based on the value of the literal.
     *
     * @param literal the literal to be removed
     */
    public void removeLiteral(int literal) {
        LinkedItemList<Literal> literals = (literal > 0) ? positiveOccurrences[literal] : negativeOccurrences[-literal];
        if(literals != null) literals.clear();}

    /**
     * Removes all occurrences of a predicate from the positiveOccurrences and negativeOccurrences list.
     *
     * @param predicate the predicate to be removed
     */
    public void removePredicate(int predicate) {
        predicate = Math.abs(predicate);
        LinkedItemList<Literal> literals = positiveOccurrences[predicate];
        if(literals != null) literals.clear();
        literals = negativeOccurrences[predicate];
        if(literals != null) literals.clear();}

    /**
     * Retrieves the first occurrence of a literal from the positiveOccurrences or negativeOccurrences list based on the value of the literal.
     *
     * @param literal The value of the literal to retrieve.
     * @return The first occurrence of the literal as a Literal object if it exists, null otherwise.
     */
    public Literal getFirstLiteral(int literal) {
        LinkedItemList<Literal> literals = (literal > 0) ? positiveOccurrences[literal] : negativeOccurrences[-literal];
        return (literals == null) ? null : literals.firstLinkedItem;}

    /**
     * Retrieves the last occurrence of a literal from the positiveOccurrences or negativeOccurrences list based on the value of the literal.
     *
     * @param literal The value of the literal to retrieve.
     * @return The last occurrence of the literal as a Literal object if it exists, null otherwise.
     */
    public Literal getLastLiteral(int literal) {
        LinkedItemList<Literal> literals = (literal > 0) ? positiveOccurrences[literal] : negativeOccurrences[-literal];
        return (literals == null) ? null : literals.lastLinkedItem;}

    /**
     * Checks if the list of literals is empty based on the given literal value.
     *
     * @param literal The value of the literal.
     * @return true if the list is empty, false otherwise.
     */
    public boolean isEmpty(int literal) {
        LinkedItemList<Literal> literals = (literal > 0) ? positiveOccurrences[literal] : negativeOccurrences[-literal];
        return literals == null  || literals.isEmpty();}

    /**
     * Checks if the list of positive and negative occurrences is empty.
     *
     * @param literal The value of the literal.
     * @return true if the lists are empty, false otherwise.
     */
    public boolean isBothEmpty(int literal) {
        int predicate = Math.abs(literal);
        LinkedItemList<Literal> positiveLiterals = positiveOccurrences[predicate];
        LinkedItemList<Literal> negativeLiterals = negativeOccurrences[predicate];
        return (positiveLiterals == null || positiveLiterals.isEmpty()) &&
                (negativeLiterals == null || negativeLiterals.isEmpty());}



    /**
     * Retrieves the size of the list of predicates in the LiteralIndex class based on the given literal value.
     *
     * @param literal The value of the literal.
     * @return The size of the list of predicates if it exists and greater than 0, 0 otherwise.
     */
    public int size(int literal) {
        LinkedItemList<Literal> literals = (literal > 0) ? positiveOccurrences[literal] : negativeOccurrences[-literal];
        return literals == null ? 0 : literals.size();}

    /** collects all the clauses in the indices as a string (for testing purposes)
     *
     * @param symboltable null or a symboltable.
     * @return all the clauses separated according the predicates.
     */
    public String toString(int predicates,Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        LinkedItemList<Literal> literals;
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            for(int sign = 1; sign >= -1; sign -= 2) {
                literals = sign == 1 ?  positiveOccurrences[predicate] : negativeOccurrences[predicate];
                if(literals == null || literals.isEmpty()) continue;
                String pred = Symboltable.toString(sign*predicate,symboltable);
                st.append(pred).append(": ");
                boolean first = true;
                Literal literalObject = literals.firstLinkedItem;
                while(literalObject != null)  {
                    int size = first ? 10-pred.length()-2 : 10; first = false;
                    st.append(literalObject.clause.toString(symboltable,size)).append("\n");
                    literalObject = (Literal)literalObject.nextItem;}}}
        return st.toString();}


}
