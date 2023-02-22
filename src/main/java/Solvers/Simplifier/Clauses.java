package Solvers.Simplifier;

import Datastructures.Symboltable;

public class Clauses {

    public Clause firstClause;
    public Clause lastClause;

    public int size = 0;

    public void addClause(Clause clause) {
        if(firstClause == null) {firstClause = clause; lastClause = clause; return;}
        clause.previousClause = lastClause;
        lastClause.nextClause = clause;
        lastClause = clause;
        ++size;}

    public void removeClause(Clause clause) {
        if(clause.nextClause == null) {
            if(clause.previousClause == null) return;
            lastClause = clause.previousClause; clause.previousClause = null; return;}
        if(clause.previousClause == null) {
            firstClause = clause.nextClause; clause.nextClause = null; return;}
        Clause previous = clause.previousClause;
        Clause next = clause.nextClause;
        previous.nextClause = next;
        next.previousClause = previous;
        clause.previousClause = null;
        clause.nextClause = null;
        --size;}

    public int size() {return size;}

    public String toString(Symboltable symboltable) {
        if(firstClause == null) return "";
        StringBuilder st = new StringBuilder();
        int maxId = 0;
        Clause clause = firstClause;
        while(clause != null) {
            maxId = Math.max(maxId,clause.id);
            clause = clause.nextClause;}
        int size = Integer.toString(maxId).length();
        clause = firstClause;
        while(clause != null) {
            st.append(clause.toString(symboltable,size)).append("\n");
            clause = clause.nextClause;}
        return st.toString();
    }

}
