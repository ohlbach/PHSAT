package Datastructures.Results;

import Datastructures.Symboltable;

public class UnsatClauses extends Unsatisfiable{

    public UnsatClauses(String problemId, String solverId) {
        super(problemId,solverId);}
    @Override
    public String description(Symboltable symboltable) {
        return "All potential models failed.";
    }
}
