package Datastructures.Results;

import Datastructures.Symboltable;

public class UnsatClauses extends Unsatisfiable{

    public UnsatClauses(String problemId, String solverId, long startTime) {
        super(problemId,solverId,startTime);}
    @Override
    public String description(Symboltable symboltable) {
        return "All potential models failed.";
    }
}
