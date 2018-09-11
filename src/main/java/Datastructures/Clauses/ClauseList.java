package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Model;
import Datastructures.Symboltable;
import Datastructures.Theory.ImplicationGraph;
import Datastructures.Theory.Theory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Created by ohlbach on 26.08.2018.
 *
 * This class is for collecting sets of clauses.
 */
public class ClauseList {
    public String info;                        // just for information
    public final ArrayList<Clause> clauses;    // the list of clauses
    public final Symboltable symboltable;      // mapping literal numbers to literal names
    int predicates;
    public int timestamp = 0;                  // for algorithms
    private HashMap<Integer,Clause> number2Clause; // maps clause numbers to clauses
    public LiteralIndex literalIndex;              // maps literals to CLiterals
    public Theory theory;
    public ImplicationGraph implicationGraph = null;

    private boolean destructiveMode = true;      // if true then changes are made destructive

    /** creates a clause list
     *
     * @param symboltable the symbol table (or null)
     */
    public ClauseList(int predicates, Symboltable symboltable) {
        this.predicates = predicates;
        clauses = new ArrayList<Clause>();
        this.symboltable = symboltable;
        this.number2Clause = new HashMap<>();
        literalIndex = new LiteralIndex(predicates);
    }

    /** creates a clause list. The number of clauses should be estimated
     *
     * @param size       the estimated number of clauses
     * @param symboltable the symbol table (or null)
     */
    public ClauseList(int size,int predicates, Symboltable symboltable) {
        clauses = new ArrayList<Clause>(size);
        this.symboltable = symboltable;
        this.number2Clause = new HashMap<>();
        literalIndex = new LiteralIndex(predicates);}

    /** adds a clause to the list and updates the literal index
     *
     * @param clause to be added
     */
    public void addClause(Clause clause) {
        clause.resize();
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

    /** collects the clauses which are false in the given model.
     *
     * @param model a model
     * @return the list of false clauses.
     */
    public ArrayList<Clause> falseClauses(Model model) {
        ArrayList<Clause> falseClauses = new ArrayList<>();
        for(Clause clause : clauses) {
            if(!clause.isTrue(model)) {falseClauses.add(clause);}}
        return falseClauses;}

    public int getOccurrences(int literal) {
        if(implicationGraph == null || implicationGraph.getImplicants(literal) == null) {
            return literalIndex.getLiterals(literal).size();}
        ++timestamp;
        int counter = 0;
        for(CLiteral cliteral : literalIndex.getLiterals(literal)) {
            ++counter;
            cliteral.getClause().timestamp = timestamp;}
        for(Integer lit : implicationGraph.getImplicants(literal)) {
            for(CLiteral cliteral : literalIndex.getLiterals(lit)) {
                if(cliteral.getClause().timestamp != timestamp) {
                    ++counter;
                    cliteral.getClause().timestamp = timestamp;}}}
        return counter;}

    public boolean isPure(int literal) {
        if(!literalIndex.getLiterals(-literal).isEmpty()) {return false;}
        if(implicationGraph == null || implicationGraph.getImplicants(literal) == null) {
            return true;}
        for(Integer lit : implicationGraph.getImplicants(literal)) {
            if(!literalIndex.getLiterals(-lit).isEmpty()) {return false;}}
        return true;}

    public void setDestructiveMode(boolean destructiveMode) {
        this.destructiveMode = destructiveMode;}

    public boolean inDestructiveMode() {return destructiveMode;}

    public void makeTrue(Clause clause) {
        removeClause(clause);
    }

    public int makeFalse(CLiteral literal) {
        return removeLiteral(literal);
    }

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
        return toString(true);}

    /** generates a string with clauses
     *
     * @param withSymboltable if true then the symboltable is used for displaying the literals
     * @return a string with clauses
     */
    public String toString(boolean withSymboltable){
        Symboltable stb = withSymboltable ? symboltable : null;
        StringBuffer st = new StringBuffer();
        if(info != null) {st.append(info).append("\n");}
        int numbersize = (""+clauses.size()).length();
        for(Clause clause : clauses) {
            st.append(clause.toString(numbersize,stb)).append("\n");}
        return st.toString();
    }





}
