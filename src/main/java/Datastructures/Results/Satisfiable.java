package Datastructures.Results;

import Datastructures.Symboltable;
import Datastructures.Theory.Model;

/** The class represents solution of a satisfiable clause set with a model.
 */
public class Satisfiable extends Result {
    /** the model for the clause set. */
    public Model model;

    /** constructs the Satisfiable instance.
     *
     * @param problemId the name of the problem.
     * @param solverId  the solver which found the model.
     * @param model     the model for the clause set.
     */
    public Satisfiable(String problemId, String solverId,Model model) {
        super(problemId,solverId);
        this.model = model;}


    /** a short description of the result.
     *
     * @return a short description of the result.
     */
    public String toString(Symboltable symboltable) {
        return "Problem " + problemId + " is satisfiable with model: "+ model.toString(symboltable) +
                "\n   found by " + solverId + " in " + ((double)getDuration()/1000.0) + " μs.";}

}
