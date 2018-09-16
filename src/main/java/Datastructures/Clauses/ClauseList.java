package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Symboltable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

/**
 * Created by ohlbach on 26.08.2018.
 *
 * This class is for collecting sets of clauses.
 */
public class ClauseList {
    int predicates;
    public final ArrayList<Clause> clauses;    // the list of clauses
    private HashMap<Integer,Clause> number2Clause; // maps clause numbers to clauses
    public LiteralIndex literalIndex;              // maps literals to CLiterals
    public int timestamp = 0;                  // for algorithms


    /** creates a clause list
     *
     */
    public ClauseList(int predicates) {
        this.predicates = predicates;
        clauses = new ArrayList<Clause>();
        this.number2Clause = new HashMap<>();
        literalIndex = new LiteralIndex(predicates);
    }

    /** creates a clause list. The number of clauses should be estimated
     *
     * @param size       the estimated number of clauses
     */
    public ClauseList(int size,int predicates) {
        clauses = new ArrayList<Clause>(size);
        this.number2Clause = new HashMap<>();
        literalIndex = new LiteralIndex(predicates);}

    /** adds a clause to the list and updates the literal index
     *
     * @param clause to be added
     */
    public void addClause(Clause clause) {
        clauses.add(clause);
        number2Clause.put(clause.number,clause);
        for(CLiteral literal : clause.cliterals) {literalIndex.addLiteral(literal);}
    }

    /** returns a clause for the given number
     *
     * @param number the clause number
     */
    public Clause getClause(int number) {
        return number2Clause.get(number);
    }

    /** removes a clause
     *
     * @param clause the clause to be removed
     */
    public void removeClause(Clause clause) {
        clauses.remove(clause);
        number2Clause.remove(clause.number);
        for(CLiteral cliteral : clause.cliterals) {literalIndex.removeLiteral(cliteral);}
    }

    public int removeLiteral(CLiteral cliteral) {
        cliteral.getClause().removeLiteral(cliteral);
        literalIndex.removeLiteral(cliteral);
        return clauses.size();}

    public void removeClausesWithLiteral(int literal) {
        for(CLiteral cLiteral : literalIndex.getLiterals(literal)) {
            removeClause(cLiteral.getClause());  // ????
        }
    }

    /** replaces a literal by its representative in an equivalence class
     *
     * @param cliteral       the literal to be replaced
     * @param representative the new literal
     * @return   true if the literal was replaced, false if it was removed.
     */
    public boolean replaceBy(CLiteral cliteral, int representative) {
        int literal = cliteral.literal;
        boolean replaced = cliteral.getClause().replaceBy(cliteral,representative);
        if(replaced) {
            literalIndex.removeLiteral(literal,cliteral);
            literalIndex.addLiteral(cliteral);}
        else {literalIndex.removeLiteral(cliteral);}
        return replaced;}

    public int getOccurrences(int literal) {
        Collection list = literalIndex.getLiterals(literal);
        return(list == null) ? 0 : list.size();}

    public boolean isPure(int literal) {
        return getOccurrences(-literal) == 0;}

    /** the actual number of clauses
     *
     * @return the number of clauses
     */
    public int size() {return clauses.size();}

    public boolean isEmpty() {return clauses.size() == 0;}

    /** generates a string with clauses
     *
     * @return a string with clauses
     */
    public String toString(){
        return toString(null);}

    /** generates a string with clauses
     *
     * @param symboltable for displaying the literals
     * @return a string with clauses
     */
    public String toString(Symboltable symboltable){
        StringBuffer st = new StringBuffer();
        int numbersize = (""+clauses.size()).length();
        for(Clause clause : clauses) {
            st.append(clause.toString(numbersize,symboltable)).append("\n");}
        return st.toString();
    }





}
