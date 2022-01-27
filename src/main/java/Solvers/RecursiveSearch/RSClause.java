package Solvers.RecursiveSearch;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.Connective;
import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;

import java.util.ArrayList;
import java.util.Formatter;
import java.util.Locale;

public class RSClause {
    public final int id;
    public final short limit;
    public final int[] literals;

    public RSClause(int id, short limit, int[] literals) {
        this.id = id;
        this.limit = limit;
        this.literals = literals;
    }

    public void newRSClauses(Clause clause, ArrayList<RSClause> rsClauses) {
        rsClauses.clear();
        int size = clause.cliterals.size();
        int[] literals = new int[3*size];
        switch(clause.connective) {
            case OR:
            case ATLEAST:
                for(int i = 0; i < size; ++i) {
                    CLiteral cLiteral = clause.cliterals.get(i);
                    int j = 3*i;
                    literals[j] = 0;
                    literals[j+1] = cLiteral.literal;
                    literals[j+2] = cLiteral.multiplicity;}
                 rsClauses.add(new RSClause(clause.id,clause.minLimit,literals));
                break;
            case ATMOST:
                for(int i = 0; i < size; ++i) {
                    CLiteral cLiteral = clause.cliterals.get(i);
                    int j = 3*i;
                    literals[j] = 0;
                    literals[j+1] = -cLiteral.literal;
                    literals[j+2] = cLiteral.multiplicity;}
                rsClauses.add(new RSClause(clause.id,(short)(clause.expandedSize()-clause.maxLimit),literals));
                break;
            case INTERVAL:
                for(int i = 0; i < size; ++i) {
                    CLiteral cLiteral = clause.cliterals.get(i);
                    int j = 3*i;
                    literals[j] = 0;
                    literals[j+1] = cLiteral.literal;
                    literals[j+2] = cLiteral.multiplicity;}
                rsClauses.add(new RSClause(-clause.id,clause.minLimit,literals));
                for(int i = 0; i < size; ++i) {
                    CLiteral cLiteral = clause.cliterals.get(i);
                    int j = 3*i;
                    literals[j] = 0;
                    literals[j+1] = -cLiteral.literal;
                    literals[j+2] = cLiteral.multiplicity;}
                rsClauses.add(new RSClause(clause.id,(short)(clause.expandedSize()-clause.maxLimit),literals));
                break;}}

    public void blockLiteral(int literal, int nodeLiteral, boolean truth) {
        assert contains(literal);
        if(limit == 1) {literals[0] = nodeLiteral; return;}
        
    }

    /** checks if the literal is in the clause
     *
     * @param literal a literal
     * @return true if the literal is in the clause
     */
    public boolean contains(int literal) {
        int size = literals.length;
        for(int i = 0; i < size; i += 3) {
            if(literals[i+1] == literal) return true;}
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
        if(limit != 1) st.append(">= "+limit);
        int size = literals.length;
        for(int i = 0; i < size; i += 3) {
            int nodeLiteral = literals[i];
            int literal = literals[i+1];
            int multiplicity = literals[i+2];
            if(nodeLiteral != 0) st.append("|"+Symboltable.toString(nodeLiteral,symboltable)+"|");
            st.append(Symboltable.toString(literal,symboltable));
            if(multiplicity > 1) st.append("^"+multiplicity);
            if(i < size-3) st.append(",");}
        return st.toString();}


}
