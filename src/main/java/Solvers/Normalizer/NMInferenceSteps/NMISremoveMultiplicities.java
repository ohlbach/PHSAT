package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Symboltable;
import Solvers.Normalizer.Clause;

/**
 * NMISremoveMultiplicities class represents an inference step that removes multiplicities &gt; min from a clause.
 * Example: atleast 2 p^3,q -&gt; atleast 2 p^2,q (making p true is in both cases sufficient).
 */
public class NMISremoveMultiplicities extends NMInferenceStep{

    public NMISremoveMultiplicities(String title,Clause clause) {
        super(title,clause);}

    public String title() {
        return "RemoveMultiplicities";}

    /**
     * Verifies if the deduced clause is valid based on the original clause, symbol table, and errors.
     * The min-values must be the same, the literals must be the same<br>
     * and those literals with multiplicities > min must be able to make the clause true.
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
        boolean isOkay = true;
        if(deducedClause.literalsAreEqual(clause,symboltable,"removeMultiplicities",errors)) { // literals must be the same
            if(min != deducedClause.min) {   // min must be the same
                errors.append("removeMultiplicities: deduced clause " + deducedClause.toString(symboltable,0) +
                        " and original clause " + clause.toString(symboltable,0) + " have different limits.\n");
                return false;}}
        else return false;

        for(int i = 0; i < clause.literals.size()-1; i += 2) {
            int literal = clause.literals.getInt(i);
            int multiplicity = clause.literals.getInt(i + 1);
            if (multiplicity >= min && !deducedClause.isTrue(literal)) { // multiplicity > min makes the clause true
                errors.append("removeMultiplicities: making literal " + Symboltable.toString(literal,symboltable) +
                        " true in deduced clause " + deducedClause.toString(symboltable,0) +
                        " does not make the clause true (wrong multiplicity)\n");
                isOkay = false;}}
        return isOkay;}
}
