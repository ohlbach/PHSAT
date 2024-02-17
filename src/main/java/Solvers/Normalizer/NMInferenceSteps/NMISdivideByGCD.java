package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Symboltable;
import Solvers.Normalizer.Clause;

public class NMISdivideByGCD extends NMInferenceStep{

    String title = "Divide-by-GCD";
    int gcd;
    public NMISdivideByGCD(Clause clause, int gcd) {
        this.clause = clause;
        this.gcd = gcd;
    }
    @Override
    public boolean verify(Clause deducedClause, Symboltable symboltable, StringBuilder errors) {
        boolean result = true;
        if(deducedClause.min != clause.min * gcd) {
            errors.append(title + ": min-value of deduced Clause " + deducedClause.toString(symboltable,0) + " != " +
                    clause.min + " * " + gcd + " of original clause "+ clause.toString(symboltable,0) + "\n");
            result = false;}
        if(deducedClause.max != clause.max * gcd) {
            errors.append(title + ": max-value of deduced Clause " + deducedClause.toString(symboltable,0) + " != " +
                    clause.max + " * " + gcd + " of original clause "+ clause.toString(symboltable,0) + "\n");
            result = false;}

        if(clause.literalsAreEqual(deducedClause,symboltable,title, errors)) {
            for(int i = 1; i < clause.literals.size(); i +=2) {
                if(clause.literals.getInt(i) != deducedClause.literals.getInt(i)*gcd) {
                    errors.append(title + ": literal " + clause.literals.getInt(i) + " of deduced Clause " + deducedClause.toString(symboltable, 0) + " != " +
                            clause.literals.getInt(i) + " * " + gcd + " of original clause " + clause.toString(symboltable, 0) + "\n");
                    result = false;}}}
        else result = false;

        return result;}
}
