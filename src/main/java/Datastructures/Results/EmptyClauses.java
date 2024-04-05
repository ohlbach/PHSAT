package Datastructures.Results;

import Datastructures.Theory.Model;

public class EmptyClauses extends Satisfiable{
    public EmptyClauses(String problemId, String solverId, long startTime, Model model) {
        super(problemId,solverId,startTime, model);}

    /** a short description of the result.
     *
     * @return a short description of the result.
     */
    public String toString() {
        return super.toString() + "\nThe clause set became empty.";
    }
}
