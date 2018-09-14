package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.ImplicationGraph;
import Datastructures.Theory.Model;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;

/**
 * Created by ohlbach on 14.09.2018.
 */
public class SATStructures {
    public int predicates;
    private int number = 0;
    private ClauseList orClauses = null;
    private ClauseList xorClauses = null;
    private ClauseList disjointClauses = null;
    private ClauseList equivalences = null;

    private Model model;
    private ImplicationGraph implicationGraph;
    private LinkedList<Integer> units = new LinkedList<>();


    public SATStructures(int predicates) {
        this.predicates = predicates;
        Model model = new Model(predicates);
        implicationGraph = new ImplicationGraph(predicates);
    }


    public void removeClause(Clause clause) {
        switch(clause.type) {
            case OR:
                orClauses.removeClause(clause); break;
            case XOR:
                xorClauses.removeClause(clause); break;
            case DISJOINT:
                disjointClauses.removeClause(clause); break;
            case EQUIV:
                equivalences.removeClause(clause); break;}}

    public Result addBasicClause(int[] basicClause) {
        if(basicClause[0] != 0) {return addBasicORClause(basicClause);}
        switch(ClauseType.getType(basicClause[1])) {
            case AND:
                units.clear();
                for(int i = 2; i < basicClause.length; ++i) {units.add(basicClause[i]);}
                return unitConsequences(units);
            case XOR:
                return addBasicXORClause(basicClause);
            case DISJOINT:
                return addBasicDISJOINTClause(basicClause);
            case EQUIV:
                return addBasicEQUIVClause(basicClause);}

        return null;
    }

    public Result addBasicXORClause(int[] basicClause) {  // stimmt noch nicht
        for(int i = 2; i < basicClause.length; ++i) {
            if(model.isTrue(basicClause[i])) {
                units.clear();
                for(int j = 2; j < basicClause.length; ++j) {
                    if(j != i) {units.add(-basicClause[j]);}}
                return unitConsequences(units);}}
        Clause clause = new Clause(++number,ClauseType.XOR,basicClause.length-2);
        for(int i = 2; i < basicClause.length; ++i) {
            int literal = basicClause[i];
            if(model.isFalse(literal)) {continue;}
            CLiteral cliteral = new CLiteral(literal);
            if(clause.addCLiteral(cliteral) == -1) {return null;}} // tautology
        units.clear();
        switch(clause.size()) {
            case 0: return new Unsatisfiable(model,basicClause);
            case 1:
                units.add(clause.cliterals.get(0).literal);
                return unitConsequences(units);
            case 2:
                int lit1 = clause.cliterals.get(0).literal;
                int lit2 = clause.cliterals.get(1).literal;
                HashSet<Integer> lits1 = implicationGraph.addClause(lit1,lit2);
                HashSet<Integer> lits2 = implicationGraph.addClause(-lit1,-lit2);
                if(lits1 != null) {units.addAll(lits1);}
                if(lits2 != null) {units.addAll(lits2);}
                if(!units.isEmpty()) {return unitConsequences(units);}
                return null;
            default: xorClauses.addClause(clause);}
        return null;
    }

    public Result addBasicDISJOINTClause(int[] basicClause) {
        return null;
    }

    public Result addBasicEQUIVClause(int[] basicClause) {
        return null;
    }

    public Result addBasicORClause(int[] basicClause) {
        Clause clause = new Clause(++number,ClauseType.OR,basicClause.length);
        for(int literal : basicClause) {
            if(model.isTrue(literal)) {return null;}  // satisfied clause
            if(model.isFalse(literal)) {continue;}
            CLiteral cliteral = new CLiteral(literal);
            if(clause.addCLiteral(cliteral) == -1) {return null;}} // tautology
        units.clear();
        switch(clause.size()) {
            case 0: return new Unsatisfiable(model,basicClause);
            case 1:
                units.add(clause.cliterals.get(0).literal);
                return unitConsequences(units);
            case 2:
                HashSet<Integer> lits = implicationGraph.addClause(clause.cliterals.get(0).literal,clause.cliterals.get(1).literal);
                if(lits != null) {
                    units.addAll(lits);
                    return unitConsequences(units);}
                return null;
            default: orClauses.addClause(clause);}
        return null;}

    public Result unitConsequences(LinkedList<Integer> units) {
        Result result;
        while(!units.isEmpty()) {
            int literal = units.pollFirst();
            switch(model.add(literal)) {
                case 1: continue;
                case -1: return new Unsatisfiable(model,literal);}
            if(orClauses != null) {orConsequences(literal, units);}
            if(xorClauses != null) {xorConsequences(literal,units);}
            if(disjointClauses != null) {disjointConsequences(literal,units);}
            if(equivalences != null) {equaivalenceConsequences(literal,units);}}
     return null;}

    public void orConsequences(int literal, LinkedList<Integer> units) {
        orClauses.removeClausesWithLiteral(literal);
        for(CLiteral cliteral : orClauses.literalIndex.getLiterals(-literal)) {
            Clause clause = cliteral.getClause();
            switch(orClauses.removeLiteral(cliteral)) {
                case 1:
                    units.addLast(clause.cliterals.get(0).literal);
                    orClauses.removeClause(clause);
                break;
                case 2:
                    orClauses.removeClause(clause);
                    HashSet<Integer> lits = implicationGraph.addClause(clause.cliterals.get(0).literal,clause.cliterals.get(1).literal);
                    if(lits != null) {units.addAll(lits);}}}}

    public void xorConsequences(int literal, LinkedList<Integer> units) {
        for(CLiteral cliteral : xorClauses.literalIndex.getLiterals(literal)) {
            Clause clause = cliteral.getClause();
            for(CLiteral lit : clause.cliterals) {
                if(lit.literal != literal) {units.addLast(-lit.literal);}}}
        for(CLiteral cliteral : xorClauses.literalIndex.getLiterals(-literal)) {
            Clause clause = cliteral.getClause();
            xorClauses.removeLiteral(cliteral);
            if(clause.size() == 1) {
                units.add(clause.cliterals.get(0).literal);
                xorClauses.removeClause(clause);}}}

    public void disjointConsequences(int literal, LinkedList<Integer> units) {
        for(CLiteral cliteral : disjointClauses.literalIndex.getLiterals(literal)) {
            Clause clause = cliteral.getClause();
            for(CLiteral lit : clause.cliterals) {
                if(lit.literal != literal) {units.addLast(-lit.literal);}}}
        for(CLiteral cliteral : disjointClauses.literalIndex.getLiterals(-literal)) {
            Clause clause = cliteral.getClause();
            xorClauses.removeLiteral(cliteral);
            if(clause.size() == 1) {xorClauses.removeClause(clause);}}}

    public void  equaivalenceConsequences(int literal, LinkedList<Integer> units) {
        for(CLiteral cliteral : equivalences.literalIndex.getLiterals(literal)) {
            Clause clause = cliteral.getClause();
            for(CLiteral lit : clause.cliterals) {
                if(lit.literal != literal) {units.addLast(lit.literal);}}
            equivalences.removeClause(clause);}
        for(CLiteral cliteral : equivalences.literalIndex.getLiterals(-literal)) {
            Clause clause = cliteral.getClause();
            for(CLiteral lit : clause.cliterals) {
                if(lit.literal != literal) {units.addLast(-lit.literal);}}
            equivalences.removeClause(clause);}}


}
