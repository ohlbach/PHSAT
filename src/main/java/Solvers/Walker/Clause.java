package Solvers.Walker;

import java.util.ArrayList;

/** A Clause object is essentially a collection of Literal objects.
 * The clauses are represented in interval-normal form [min,max].
 *  Nevertheless, the clauses keep their original connective.
 *  A clause may be part of a doubly quantified list (list of false clauses).
 */
public class Clause extends Datastructures.Clause {

    /** the number of true predicates in the local model. */
    protected int trueLiterals = 0;

    /** flag to indicate that the clause is true in the local model. */
    protected boolean isLocallyTrue;

    /** a timestamp to be used by various algorithms. */
    protected int timestamp = 0;

    public Clause(Datastructures.Clause clause) {
       super(clause.id,clause.version,clause.quantifier,clause.min,clause.max,clause.expandedSize);
        literals = new ArrayList<>(clause.literals.size()/2);
        for(int i = 0; i < clause.literals.size()-1; i +=2) {
            Literal literalObject = new Literal(clause.literals.get(i).literal,clause.literals.get(i).multiplicity);
            literalObject.clause = this;
            literals.add(literalObject);}}






}
