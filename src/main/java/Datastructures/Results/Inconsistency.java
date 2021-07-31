package Datastructures.Results;

public class Inconsistency extends Result{
    String id;
    String message;
    public Inconsistency(String id,String message) {
        this.id = id;
        this.message = message;}

    public String toString() {
        return "Inconsistency at " + id + ": "+message;}
}
