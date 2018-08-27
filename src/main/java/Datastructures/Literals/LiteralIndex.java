package Datastructures.Literals;

import java.util.LinkedList;

/**
 * Created by Ohlbach on 25.08.2018.
 */
public class LiteralIndex {
    private int predicates;
    private LinkedList<CLiteral>[] posOccurrences;
    private LinkedList<CLiteral>[] negOccurrences;

    public LiteralIndex(int predicates) {
        assert predicates > 0;
        this.predicates = predicates;
        posOccurrences = new LinkedList[predicates+1];
        negOccurrences = new LinkedList[predicates+1];}

    public void addLiteral(CLiteral cliteral) {
        int literal = cliteral.literal;
        int predicate = Math.abs(literal);
        LinkedList<CLiteral>[] list = literal > 0 ? posOccurrences : negOccurrences;
        LinkedList<CLiteral> lits = list[predicate];
        if(lits == null) {
            lits = new LinkedList<CLiteral>();
            list[predicate] = lits;}
        lits.add(cliteral);}

    public void removeLiteral(CLiteral cliteral) {
        int literal = cliteral.literal;
        int predicate = Math.abs(literal);
        if(literal > 0) {posOccurrences[predicate].remove(cliteral);}
        else {negOccurrences[predicate].remove(cliteral);}
    }

    public LinkedList<CLiteral> getLiterals(int literal) {
        assert literal > 0 && Math.abs(literal) <= predicates;
        return literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];}

    public String toString() {
        StringBuffer st = new StringBuffer();
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            LinkedList<CLiteral> pos = posOccurrences[predicate];
            LinkedList<CLiteral> neg = negOccurrences[predicate];
            StringBuffer posString = null;
            StringBuffer negString = null;
            if(pos != null) {
                posString = new StringBuffer();
                for(CLiteral lit : pos) {posString.append(lit.toFullString()).append(",");}}
            if(neg != null) {
                negString = new StringBuffer();
                for(CLiteral lit : neg) {negString.append(lit.toFullString()).append(",");}}
            if(posString != null) {
                st.append(" ").append(Integer.toString(predicate)).append(" ").append(posString).append("\n");}
            if(negString != null)
                st.append(Integer.toString(-predicate)).append(" ").append(negString).append("\n");}
        return st.toString();}

}
