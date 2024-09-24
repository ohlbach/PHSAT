package Datastructures.Results;

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
    public Satisfiable(String problemId, String solverId, Model model) {
        super(problemId,solverId,"satisfiable",model.startTime);
        message = (symboltable) -> "Model: " + model.toString(symboltable);
        this.model = model;
        inferenceSteps = model.inferenceSteps;
    }


}
