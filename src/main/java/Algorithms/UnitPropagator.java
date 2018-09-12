package Algorithms;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Model;

import java.util.LinkedList;

/**
 * Created by Ohlbach on 28.08.2018.
 */
public class UnitPropagator {
    private ClauseList clauses;
    private LinkedList<Integer> unitLiterals = new LinkedList<>();

    public UnitPropagator(ClauseList clauses) {
        this.clauses = clauses;
    }

    public void propagate(CLiteral cliteral) {
        unitLiterals.clear();
        unitLiterals.add(cliteral.literal);
        while(!unitLiterals.isEmpty()) {
            int literal = unitLiterals.removeFirst();
            LiteralIndex index = clauses.literalIndex;
            Model model = null;//clauses.model;
            model.push(literal);
            for(CLiteral clit : index.getLiterals(literal)) {clauses.makeTrue(clit.getClause());}
            for(CLiteral clit : index.getLiterals(-literal)) {
                Clause clause = clit.getClause();
                int size = clauses.makeFalse(clit);
                if(size == 1) {
                    CLiteral cnegliteral = clause.getFirstLiteral(model);
                    clauses.makeTrue(clause);
                    unitLiterals.addFirst(cnegliteral.literal);}}}}
}
