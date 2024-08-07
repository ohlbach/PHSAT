package Datastructures.Results;

import Datastructures.Theory.Model;

public class EmptyClauses extends Satisfiable{
    public EmptyClauses(String problemId, String solverId, Model model) {
        super(problemId,solverId, model);
        message = (symboltable -> "Clause set became empty.\n Model: " + model.toString(symboltable));
    }


}
