package Solvers.Walker;

public class Clauses {
    
    Clause firstClause;
    Clause lastClause;

    public void addToFront(Clause clause) {
        clause.isInList = true;
        clause.previousClause = null; clause.nextClause = null;
        if(firstClause == null) {firstClause = clause; lastClause = clause; return;}
        if(firstClause.nextClause == null) {lastClause = firstClause;}
        clause.nextClause = firstClause;
        firstClause.previousClause = clause;
        firstClause = clause;
    }

    public void addToBack(Clause clause) {
        clause.previousClause = null; clause.nextClause = null;
        if(firstClause == null) {firstClause = clause; lastClause = clause; return;}
        if(firstClause.nextClause == null) {lastClause = firstClause;}
        clause.previousClause = lastClause;
        lastClause.nextClause = clause;
        lastClause = clause;}

    public void remove(Clause clause) {
        if(clause.previousClause == null && clause.nextClause == null) return;
        if(lastClause == firstClause) {lastClause = null; firstClause = null; return;}
        if(clause.previousClause == null) {
            firstClause = clause.nextClause;
            return;}
        if(clause.nextClause == null) {
            lastClause = clause.previousClause;
            return;}
        Clause claus = clause.previousClause;
        claus.nextClause = clause.nextClause;
        clause.nextClause.previousClause = claus;
    }
    
}
