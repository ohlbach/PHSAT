package Datastructures.Results;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Theory.Model;

import java.util.ArrayList;

/**
 * Created by ohlbach on 14.09.2018.
 */
public class Result extends Exception {

    public boolean isOkay() {return true;}

    public Result() {
        super(); }

    public static Result makeResult(Model model, BasicClauseList clauses) {
        if(clauses == null) {return null;}
        ArrayList<int[]> falseClauses = clauses.notTrueClausesInModel(model);
        return (falseClauses == null) ? new Satisfiable(model) : new Erraneous(model,falseClauses,clauses.symboltable);
    }
}
