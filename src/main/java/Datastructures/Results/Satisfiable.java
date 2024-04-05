package Datastructures.Results;

import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import Utilities.Utilities;

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
    public Satisfiable(String problemId, String solverId,long startTime, Model model) {
        super(problemId,solverId,startTime);
        this.model = model;}


    /** a short description of the result.
     *
     * @return a short description of the result.
     */
    @Override
    public String toString(Symboltable symboltable) {
         return "For problem " + problemId + ", solver " + solverId + " found a model in "+
                 Utilities.duration(elapsedTime) + ". The model is:\n" +model.toString(symboltable);}

}
