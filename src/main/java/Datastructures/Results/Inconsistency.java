package Datastructures.Results;

public class Inconsistency extends Result{
    String id;
    String message;
    public Inconsistency(String problemId, String solverId, long startTime, String id,String message) {
        super(problemId,solverId);
        this.id = id;
        this.message = message;}

    public String toString() {
        return "Inconsistency at " + id + ": "+message;}
}
