package Solvers.RecursiveSearch;

import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;

import java.util.ArrayList;
import java.util.Formatter;
import java.util.Locale;

public class RSClause {
    public final int id;
    public short minLimit;
    public final RSLiteral[] rsLiterals;
    protected int timestamp = 0;

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

    /** computes the number of unblocked literals
     *
     * @return the number of unblocked literals
     */
    protected int unblockedSize() {
        int length = rsLiterals.length;
        for(int i = 0; i < length; ++i) {
            if(rsLiterals[i].rsNode != null) return i;}
        return length;}

    /** computes the number of unblocked literals
     *
     * @return the number of unblocked literals
     */
    protected int expandedSize() {
        int size = 0;
        int length = rsLiterals.length;
        for(int i = 0; i < length; ++i) {
            RSLiteral rsLiteral = rsLiterals[i];
            if(rsLiteral.rsNode == null) size += rsLiteral.multiplicity;
            else break;}
        return size;}


    protected void blockClause(RSLiteral rsLiteral, RSNode rsNode) {
        RSLiteral firstRSLiteral = rsLiterals[0];
        if(firstRSLiteral.rsNode != null) return;
        if(rsLiteral.multiplicity <= minLimit) {
            firstRSLiteral.rsNode = rsNode;
            rsNode.addRSLiteral(firstRSLiteral);
            return;}
        rsLiteral.rsNode = rsNode;
        move(rsLiteral);
        minLimit -= rsLiteral.multiplicity;
        rsNode.addRSLiteral(rsLiteral);}

    protected void block(RSNode rsNode) {
        rsLiterals[0].rsNode = rsNode;
        rsNode.addRSLiteral(rsLiterals[0]);}

    protected boolean isBlocked() {
        return rsLiterals[0].rsNode != null;}

    protected void move(RSLiteral rsLiteral) {
        int pos1 = 0;
        for(int i = 0; i < rsLiterals.length; ++i) {
            if(rsLiterals[i] == rsLiteral) pos1 = i;
            if(rsLiterals[i].rsNode != null)  {
                rsLiterals[pos1] = rsLiterals[i-1];
                rsLiterals[i-1] = rsLiteral;
                return;}}}


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
