package Solvers.RecursiveSearch;

import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;

import java.util.ArrayList;
import java.util.Formatter;
import java.util.Locale;

public class RSClause {
    public final int id;
    public final short minLimit;
    public final RSLiteral[] rsLiterals;

    public RSClause(int id, short minLimit, RSLiteral[] rsLiterals) {
        this.id = id;
        this.minLimit = minLimit;
        this.rsLiterals = rsLiterals;
    }

    public static void newRSClauses(Clause clause, ArrayList<RSClause> rsClauses) {
        rsClauses.clear();
        RSClause rsClause;
        int size = clause.cliterals.size();
        RSLiteral[] literals = new RSLiteral[size];
        switch(clause.connective) {
            case OR:
            case ATLEAST:
                rsClause = new RSClause(clause.id,clause.minLimit,literals);
                for(int i = 0; i < size; ++i) {
                    CLiteral cLiteral = clause.cliterals.get(i);
                    literals[i] = new RSLiteral(cLiteral.literal,cLiteral.multiplicity,rsClause);}
                 rsClauses.add(rsClause);
                break;
            case ATMOST:
                rsClause = new RSClause(clause.id,(short)(clause.expandedSize()-clause.maxLimit),literals);
                for(int i = 0; i < size; ++i) {
                    CLiteral cLiteral = clause.cliterals.get(i);
                    literals[i] = new RSLiteral(-cLiteral.literal,cLiteral.multiplicity,rsClause);}
                rsClauses.add(rsClause);
                break;
            case INTERVAL:
                rsClause = new RSClause(clause.id,clause.minLimit,literals);
                for(int i = 0; i < size; ++i) {
                    CLiteral cLiteral = clause.cliterals.get(i);
                    literals[i] = new RSLiteral(cLiteral.literal,cLiteral.multiplicity,rsClause);}
                rsClauses.add(rsClause);
                rsClause = new RSClause(clause.id,(short)(clause.expandedSize()-clause.maxLimit),literals);
                for(int i = 0; i < size; ++i) {
                    CLiteral cLiteral = clause.cliterals.get(i);
                    literals[i] = new RSLiteral(-cLiteral.literal,cLiteral.multiplicity,rsClause);}
                rsClauses.add(rsClause);
                break;}}

    public void blockLiteral(int literal, int nodeLiteral, boolean truth) {
        assert contains(literal);
        //if(limit == 1) {literals[0] = nodeLiteral; return;}

    }

    /** checks if the literal is in the clause
     *
     * @param literal a literal
     * @return true if the literal is in the clause
     */
    public boolean contains(int literal) {
        int size = rsLiterals.length;
        for(int i = 0; i < size; i += 3) {
            if(rsLiterals[i+1].literal == literal) return true;}
        return false;}

    /** turns the clause into a string
     *
     * @return             the clause as string
     */
    public String toString(){
        return toString(0,null);}

    /** turns the clause into a string
     *
     * @param width        0 or the width of the id-string
     * @param symboltable  null or a symboltable
     * @return             the clause as string
     */
    public String toString(int width, Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        if(width > 0) {
            Formatter format = new Formatter(st, Locale.GERMANY);
            format.format("%-"+width+"d:",id);}
        else st.append(id+": ");
        if(minLimit != 1) st.append(">= "+ minLimit);
        int size = rsLiterals.length;
        for(int i = 0; i < size; ++i) {
            st.append(rsLiterals[i].toString(symboltable));
            if(i < size-3) st.append(",");}
        return st.toString();}


}
