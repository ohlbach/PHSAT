package Datastructures.Results;

import Datastructures.Symboltable;

public class UnsatString extends Unsatisfiable{

    String explanation;
    public UnsatString(String problemId, String solverId, long startTime, String explanation) {
        super(problemId,solverId);
        this.explanation = explanation;}

    @Override
    public String description(Symboltable symboltable) {
        return explanation;}
}
