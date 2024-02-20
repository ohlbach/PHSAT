package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Symboltable;
import Solvers.Normalizer.Clause;

public class NMISremoveMultiplicities extends NMInferenceStep{

    public NMISremoveMultiplicities(Clause clause) {
        this.clause = clause;}

    /**
     * Verifies if the deduced clause is valid based on the original clause, symbol table, and errors.
     *
     * @param deducedClause The deduced clause to be verified.
     * @param symboltable The symbol table used for evaluation, maybe null.
     * @param errors The StringBuilder to store any error messages encountered during verification.
     * @return Returns true if the deduced clause is valid according to the original clause; otherwise, returns false.
     */
    @Override
    public boolean verify(Clause deducedClause, Symboltable symboltable, StringBuilder errors) {
        int min = clause.min;
        if(min <= 0) return true;
        if(deducedClause.literalsAreEqual(clause,symboltable,"removeMultiplicities",errors)) {
            if(min != deducedClause.min || clause.max != deducedClause.max) {
                errors.append("removeMultiplicities: deduced clause " + deducedClause.toString(symboltable,0) +
                        " and original clause " + clause.toString(symboltable,0) + " have different limits");}}
        else return false;

        boolean isOkay = true;
        for(int i = 0; i < clause.literals.size()-1; i += 2) {
            int literal = clause.literals.getInt(i);
            int multiplicity = clause.literals.getInt(i + 1);
            if (multiplicity > min && !deducedClause.isTrue(literal)) {
                errors.append("removeMultiplicities: making literal " + Symboltable.toString(literal,symboltable) +
                        "true in deduced clause " + deducedClause.toString(symboltable,0) +
                        " does not make the clause true (wrong multiplicity)");
                isOkay = false;}}
        return isOkay;}
}
