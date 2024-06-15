package Solvers.Backtracker;

public class Clause extends Datastructures.Clause<Literal>{

    private int timestamp = 0;
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




}
