package Solvers.Backtracker;

public class Clause extends Datastructures.Clause<Literal>{

    /** this constructor turns a normalizedClause to a clause for the Backtracker solver.
     *
     * @param normalizedClause a normalized and simplified clause from the Normalizer.
     */
    public Clause(Solvers.Normalizer.Clause normalizedClause) {
      super(normalizedClause.id, normalizedClause.version,normalizedClause.quantifier,
              normalizedClause.min, normalizedClause.max, normalizedClause.expandedSize);
      for(int i = 0; i < normalizedClause.literals.size(); i += 2) {
          Literal literal = new Literal(normalizedClause.literals.getInt(i), normalizedClause.literals.getInt(i + 1));
          literals.add(literal);}}

    /**
     * Removes a given Literal object from the Clause and performs necessary updates.
     *
     * @param literalObject the Literal object to be removed
     * @param isTrue        a boolean indicating if the removed Literal is true or false
     * @return true if the reduced clause is still satisfiable, otherwise false.
     */
    boolean removeLiteral(Literal literalObject, boolean isTrue) {
        literals.remove(literalObject);
        int multiplicity = literalObject.multiplicity;
        expandedSize -= multiplicity;
        if(isTrue) {
            min = Math.max(0,min-multiplicity);
            max -= multiplicity;
            expandedSize = 0;
            for(Literal litObject : literals) {
                litObject.multiplicity = Math.min(min,literalObject.multiplicity);
                expandedSize += literalObject.multiplicity;}}
        else max = Math.min(expandedSize,max);
        if(min > max) return false;
        classifyQuantifier();
        return true;}


}
