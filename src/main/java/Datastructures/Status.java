package Datastructures;

import Datastructures.Clauses.ClauseList;

/**
 * Created by ohlbach on 26.08.2018.
 */
public class Status {
    public String filename;
    public boolean unsatisfiable;
    public boolean toBeExamined;
    public ClauseList clauseList;
    public String falseClause;
    public int seed = -1;

}
