package Datastructures.Literals;

import Datastructures.Clauses.Clause;

/**
 * Created by Ohlbach on 25.08.2018.
 */
public class CLiteral {
    private int literal;
    private Clause clause;
    private int position;

    public CLiteral(int literal) {
        this.literal = literal;}

    public CLiteral(int literal, Clause clause, int position) {
        this.literal = literal;
        this.clause = clause;
        this.position = position;}

    public int getLiteral() {return literal;}

    public Clause getClause() {return clause;}

    public int getPosition() {return position;}

    public void setClause(Clause clause, int position) {
        assert position >= 0;
        this.clause = clause;
        this.position = position;}

    public void removeClause() {
        position = -1;}

    public String toFullString() {
        return Integer.toString(clause.number)+","+Integer.toString(position)+":"+Integer.toString(literal);
    }

    public String toString() {
        return Integer.toString(literal);}

}
