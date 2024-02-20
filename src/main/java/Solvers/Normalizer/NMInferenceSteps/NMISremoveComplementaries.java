package Solvers.Normalizer.NMInferenceSteps;

import Datastructures.Symboltable;
import Solvers.Normalizer.Clause;
import it.unimi.dsi.fastutil.ints.IntArrayList;

public class NMISremoveComplementaries extends NMInferenceStep{

    public NMISremoveComplementaries(Clause clause) {
        this.clause = clause;
    }
    @Override
    public boolean verify(Clause deducedClause, Symboltable symboltable, StringBuilder errors) {
        IntArrayList literals = new IntArrayList(clause.literals.size()/2);
        for(int i = 0; i < clause.literals.size()-1; i +=2) {
            int literal = clause.literals.getInt(i);
            if(!literals.contains(-literal)) literals.add(literal);}
        int limit = 1 << literals.size();
        for(int i = 1; i <= limit; ++i) {
            if(clause.isTrue(literal->((i & literals.indexOf(literal)+1) != 0))) {
                if(!deducedClause.isTrue(literal->(i & literals.indexOf(literal)+1) != 0)) {
                    String model = "";
                    for(int j = 0; j < literals.size(); ++j) {
                        if((1 << j & (i+1)) != 0) model += Symboltable.toString(literals.getInt(j),symboltable) + ","; }
                    errors.append("NMISremoveComplementaries: " + clause.toString(symboltable, 0) +
                           " and model " + model + " /=> " +
                            deducedClause.toString(symboltable, 0) + "\n");}}
            return false;}
    return true;}
}
