package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Symboltable;
import Solvers.Normalizer.Clause;

public class NMISreduceToEssentialLiterals extends NMInferenceStep {

    private static String title = "reduce to essential Literals";



    public NMISreduceToEssentialLiterals(Clause clause) {
        this.clause = clause;}

    @Override
    public boolean verify(Clause deducedClause, Symboltable symboltable, StringBuilder errors) {
        for(int i = 0; i < deducedClause.literals.size()-1; i += 2) {
            int literal = deducedClause.literals.getInt(i);
            if(!clause.isTrue(literal)) {
                errors.append(title+ ": clauses " + clause.toString(symboltable,0) +
                    " and the version reduced to essential literals " + deducedClause.toString(symboltable,0) +
                    " are not semantically equal. Critical literal: " + Symboltable.toString(literal,symboltable) + "\n");
                return false;}}
        return true;}
}
