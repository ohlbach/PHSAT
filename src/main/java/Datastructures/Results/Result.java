package Datastructures.Results;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.ClauseList;
import Datastructures.Theory.Model;

import java.util.ArrayList;

/**
 * Created by ohlbach on 14.09.2018.
 */
public class Result {

    public boolean isOkay() {return true;}

    public static Result makeResult(Model model, BasicClauseList clauses) {
        if(clauses == null) {return null;}
        ArrayList<int[]> falseClauses = clauses.falseClauses(model);
        return (falseClauses == null) ? new Satisfiable(model) : new Erraneous(model,falseClauses,clauses.symboltable);
    }
}
