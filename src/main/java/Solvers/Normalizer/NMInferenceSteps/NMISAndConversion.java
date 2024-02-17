package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import Solvers.Normalizer.Clause;

public class NMISAndConversion extends NMInferenceStep{

    public static final String title = "And-Conversion";

    public NMISAndConversion(Clause clause) {
        this.clause = clause;}
    @Override
    public boolean verify(Clause deducedClause, Symboltable symboltable, StringBuilder errors) {
        boolean result = true;
        if (deducedClause.quantifier != Quantifier.AND) { // check the quantifier
            errors.append(title + ": Deduced clause " + deducedClause.toString(symboltable,0) + " is not a conjunction.\n");
            result = false;}

        if(clause.literals.size() != deducedClause.literals.size()) { // clauses may have different number of literals
            errors.append(title + ": Original clause " + clause.toString(symboltable,0) + " and deduced clause "
            + deducedClause.toString(symboltable,0) +" have different length.\n");
            result = false;}
        else {
            for(int i = 0; i < clause.literals.size()-1; i +=2){ // clauses may have different literals
                if(clause.literals.getInt(i) != deducedClause.literals.getInt(i)) {
                    errors.append(title + ": Original clause " + clause.toString(symboltable, 0) + " and deduced clause "
                            + deducedClause.toString(symboltable, 0) + " have different literals at position " + i + ".\n");
                    result = false;}}
            int size = clause.checkExpandedSize();
            if(size >= 0) {   // expanded size may be wrong
                errors.append(title+ ": Real expanded size " + size + " != expanded size: " + clause.expandedSize+ "\n");
                result = false;}
            else {   // min must be the expanded size.
                if(clause.min != clause.expandedSize) {
                    errors.append(title + ": Original clause " + clause.toString(symboltable,0) + ":" +
                            "min-value " + clause.min + " is not the expandedSize " + clause.expandedSize+"\n");
                    result = false;}}}
        return result;}
}
