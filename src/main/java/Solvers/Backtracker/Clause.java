package Solvers.Backtracker;

import Datastructures.Clauses.Quantifier;

public class Clause extends Datastructures.Clause<Literal>{

    /** this constructor turns a normalizedClause to a clause for the Backtracker solver.
     *
     * @param normalizedClause a normalized and simplified clause from the Normalizer.
     */
    public Clause(Solvers.Normalizer.Clause normalizedClause) {
      super(normalizedClause.id, normalizedClause.version,normalizedClause.quantifier,
              normalizedClause.min, normalizedClause.max, normalizedClause.expandedSize);
      for(int i = 0; i < normalizedClause.literals.size(); i += 2) {
          Literal literal = new Literal(normalizedClause.literals.get(i).literal, normalizedClause.literals.get(i).multiplicity);
          literals.add(literal);}}

    /** generates a clause from an int-array representation of an inputClause (for testing purposes).
     *
     * @param inputClause an int-array representation of an input clause.
     */
    public Clause(int[] inputClause) {
        super(inputClause, false, (literal) -> new Literal(literal,1),null);
        for(Literal literalObject : literals) literalObject.clause = this;
    }

    /**
     * Removes a given Literal object from the clause and performs necessary updates.
     *
     * @param literalObject the Literal object to be removed.
     * @param isTrue        a boolean indicating if the removed Literal is true or false.
     */
    void removeLiteral(Literal literalObject, boolean isTrue) {
        assert literalObject.clause == this;
        super.removeLiteralAtPosition(literals.indexOf(literalObject), isTrue);}

    /** Removes a (false) literal from a disjunction (with atleast two predicates)
     *
     * @return true if the resulting clause is a unit clause.
     */
    boolean removeLiteral(Literal literalObject) {
        assert quantifier == Quantifier.OR;
        assert literals.size() > 1;
        literals.remove(literalObject);
        expandedSize = literals.size();
        max = expandedSize;
        return expandedSize == 1;}


}
