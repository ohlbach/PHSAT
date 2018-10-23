package Solvers.Reolution;

import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;

import java.util.ArrayList;
import java.util.Comparator;

/**
 * Created by ohlbach on 22.10.2018.
 */
public class RClause extends Clause {
    int priority;
    public boolean input;

    public static Comparator<Clause> priorityComparator = Comparator.comparingInt(clause->((RClause)clause).priority);

    public RClause(Clause clause, int priority, boolean input) {
        super(clause.id,clause.size());
        this.priority = priority;
        this.input = input;
        for(CLiteral cLiteral : clause.cliterals) {clause.addCLiteralDirectly(cLiteral.clone());}
    }

    public RClause(String id, ArrayList<CLiteral> literals, int priority, boolean input) {
        super(id,literals.size());
        this.priority = priority;
        this.input = input;
        cliterals = literals;}




}
