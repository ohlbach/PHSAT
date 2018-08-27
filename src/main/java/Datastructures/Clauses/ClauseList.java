package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Model;
import Datastructures.Symboltable;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by ohlbach on 26.08.2018.
 */
public class ClauseList {
    public String info;
    private final ArrayList<Clause> clauses;
    private final Model model;
    private final Symboltable symboltable;
    private int timestamp = 0;
    private HashMap<Integer,Clause> number2Clause;
    private LiteralIndex literalIndex;

    public ClauseList(Model model,Symboltable symboltable) {
        clauses = new ArrayList<Clause>();
        this.model = model;
        this.symboltable = symboltable;
        this.number2Clause = new HashMap<>();
        literalIndex = new LiteralIndex(model.predicates);
    }

    public ClauseList(int size, Model model,Symboltable symboltable) {
        clauses = new ArrayList<Clause>(size);
        this.model = model;
        this.symboltable = symboltable;
        this.number2Clause = new HashMap<>();
        literalIndex = new LiteralIndex(model.predicates);}

    public void addClause(Clause clause) {
        clauses.add(clause);
        number2Clause.put(clause.number,clause);
        for(CLiteral literal : clause.cliterals) {literalIndex.addLiteral(literal);}
    }

    public void getClause(int number) {
        number2Clause.get(number);
    }

    public void removeClause(Clause clause) {
        clauses.remove(clause);
        number2Clause.remove(clause.number);
        for(CLiteral literal : clause.cliterals) {literalIndex.removeLiteral(literal);}
    }

    public int size() {return clauses.size();}

    public String toString(){
        StringBuffer st = new StringBuffer();
        if(info != null) {st.append(info).append("\n");}
        if(!model.isEmpty()) {st.append("Partial Model: ").append(model.toString()).append("\n");}
        int numbersize = (""+clauses.size()).length();
        for(Clause clause : clauses) {
            if(!clause.isTrue(model)) {
                st.append(clause.toString(model,numbersize)).append("\n");}}
        return st.toString();
    }





}
