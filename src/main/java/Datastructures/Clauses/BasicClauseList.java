
package Datastructures.Clauses;

import Datastructures.Statistics.ProblemStatistics;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;

import java.util.ArrayList;

/**
 * Created by Ohlbach on 03.09.2018.<br>
 *
 * This class contains just the absolutely essential information about clauses.<br>
 * The clauses should be the original clauses from the clause source and must not be changed.<br>
 * They are used to check a candidate model against the original clause set.<br>
 * A clause is an integer-array [clause-number,clause-type,literal1,...]<br>
 * The clause types are: <br>
 * '0': means disjunction:  '0 1 3 5'  means 1 or 3 or 5<br>
 * '1': means and:          '1 3 4 5'  stands for 3 and 4 and 5.<br>
 * '2': means exclusive-or: '2 3 4 5'  means 3 xors 4 xors 5 (exactly one of them must be true).<br>
 * '3': means disjoints   : '3 4 5 -6' means 4,5,-6 are disjoint literals (at most one of them can be true).<br>
 * '4': means equivalences: '4 4 5 -6' means that these three literals are equivalent.
 */
public class BasicClauseList {
    public int predicates;

    /** the original disjunctions */
    public ArrayList<int[]> disjunctions  = new ArrayList<>();
    /** the original conjunctions */
    public ArrayList<int[]> conjunctions  = new ArrayList<>();
    /** the original xors */
    public ArrayList<int[]> xors          = new ArrayList<>();
    /** the original disjoints */
    public ArrayList<int[]> disjoints     = new ArrayList<>();
    /** the original equivalences */
    public ArrayList<int[]> equivalences  = new ArrayList<>();

    /** an info-string about the origin of the clauses */
    public String info = null;

    /** a symboltable, or null */
    public Symboltable symboltable = null;

    /** adds a clause to the corresponding lists
     *
     * @param clause a clause
     */
    public void addClause(int[] clause) {
        switch(ClauseType.getType(clause[1])) {
            case OR:       disjunctions.add(clause); break;
            case AND:      conjunctions.add(clause); break;
            case XOR:      xors.add(clause);         break;
            case DISJOINT: disjoints.add(clause);    break;
            case EQUIV:    equivalences.add(clause); break;}
    }

    /** adds the clauses to the corresponding lists
     *
     * @param clauses a clause
     */
    public void addClauses(int[]... clauses) {
        for(int[] clause : clauses) {addClause(clause);}}


    /** checks if a disjunction is true in a model
     *
     * @param clause a disjunctive clause
     * @param model a model
     * @return true if at least one of the literals is true in the model.
     */
    public static boolean disjunctionIsTrue(int[] clause, Model model) {
        assert ClauseType.getType(clause[1]) == ClauseType.OR;
        for(int i = 2; i < clause.length; ++i) {if(model.isTrue(clause[i])) {return true;}}
        return false;}

    /** checks if a conjunction is true in a model
     *
     * @param clause a conjunctive clause
     * @param model a model
     * @return true if all literals are true in the model.
     */
    public static boolean conjunctionIsTrue(int[] clause, Model model) {
        assert ClauseType.getType(clause[1]) == ClauseType.AND;
        for(int i = 2; i < clause.length; ++i) {if(!model.isTrue(clause[i])) {return false;}}
        return true;}

    /** checks if a xors-clause is true in a model
     *
     * @param clause a xors clause
     * @param model a model
     * @return true if exactly one of the literals is true in the model.
     */
    public static boolean xorIsTrue(int[] clause, Model model) {
        assert ClauseType.getType(clause[1]) == ClauseType.XOR;
        int trueLiteral = 0;
        for(int i = 2; i < clause.length; ++i) {
            if(model.isTrue(clause[i])) {
                if(trueLiteral != 0) {return false;}
                trueLiteral = clause[i];}}
        return trueLiteral != 0;}

    /** checks if a disjoint clause is true in a model
     *
     * @param clause a disjoint clause
     * @param model a model
     * @return true if at most one of the literals is true in the model.
     */
    public static boolean disjointIsTrue(int[] clause, Model model) {
        assert ClauseType.getType(clause[1]) == ClauseType.DISJOINT;
        int trueLiteral = 0;
        for(int i = 2; i < clause.length; ++i) {
            if(model.isTrue(clause[i])) {
                if(trueLiteral != 0) {return false;}
                trueLiteral = clause[i];}}
        return true;}

    /** checks if an equivalence is true in a model
     *
     * @param clause an equivalence clause
     * @param model a model
     * @return true if either all literals are true or all literals are false in the model.
     */
    public static boolean equivalenceIsTrue(int[] clause, Model model) {
        assert ClauseType.getType(clause[1]) == ClauseType.EQUIV;
        int status = model.status(clause[2]);
        if(status == 0) {return false;}
        for(int i = 3; i < clause.length; ++i) {
            if(model.status(clause[i]) != status) {return false;}}
        return true;}




    /** computes a list of clauses which are false in the model
     *
     * @param model a model for the literals of the clause
     * @return null or a list of false clauses.
     */
    public ArrayList<int[]> falseClauses(Model model) {
        ArrayList<int[]> falseClauses = new ArrayList<>();
        for(int[] clause : disjunctions) {if(!disjunctionIsTrue(clause,model)) {falseClauses.add(clause);}}
        for(int[] clause : conjunctions) {if(!conjunctionIsTrue(clause,model)) {falseClauses.add(clause);}}
        for(int[] clause : xors)         {if(!xorIsTrue(clause,model))         {falseClauses.add(clause);}}
        for(int[] clause : disjoints)    {if(!disjointIsTrue(clause,model))    {falseClauses.add(clause);}}
        for(int[] clause : equivalences) {if(!equivalenceIsTrue(clause,model)) {falseClauses.add(clause);}}
        return falseClauses.isEmpty() ? null : falseClauses;}

    /** adds some parameters to the statistics
     *
     * @param statistics the ProblemStatistics
     */
    public void addToStatistics(ProblemStatistics statistics) {
        statistics.disjoints    = disjoints.size();
        statistics.disjunctions = disjunctions.size();
        statistics.conjunctions = conjunctions.size();
        statistics.xors         = xors.size();
        statistics.equivalences = equivalences.size();
    }

    /** turns a clause into a string.
     *
     * @param size       the length for the number string
     * @param clause     the clause
     * @param symboltable a symboltable or null
     * @return the clause as string
     */
    public static String clauseToString(int size, int[] clause, Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append(String.format("%"+size+"d ",clause[0]));
        st.append(ClauseType.getType(clause[1]).abbreviation).append(": ");
        int length = clause.length;
        for(int i = 2; i < length-1; ++i) {
            if(symboltable != null) {st.append(symboltable.getLiteralName(clause[i]));}
            else                    {st.append(Integer.toString(clause[i]));}
            st.append(",");}
        if(symboltable != null) {st.append(symboltable.getLiteralName(clause[length-1]));}
        else                    {st.append(Integer.toString(clause[length-1]));}
        return st.toString();}

    /** generates a string representation of the clause
     *
     * @return  a string representation of the clause.
     */
    public String toString() {
        return toString(true);}

    /** generates a string representation of the clause
     *
     * @param withSymbols if true then a symboltable is used to map the numbers to a string.
     * @return  a string representation of the disjunctions.
     */
    public String toString(boolean withSymbols) {
        Symboltable symboltable = withSymbols ? this.symboltable : null;
        StringBuilder st = new StringBuilder();
        if(info != null) {st.append(info).append("\n");}
        int size = (""+(disjunctions.size() + conjunctions.size() + xors.size() + disjoints.size()+equivalences.size())).length();
        if(!disjunctions.isEmpty()) {
            st.append("Disjunctions:\n");
            for(int[] clause : disjunctions) {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(!conjunctions.isEmpty()) {
            st.append("Conjunctions:\n");
            for(int[] clause : conjunctions) {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(!xors.isEmpty()) {
            st.append("Xor:\n");
            for(int[] clause : xors)          {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(!disjoints.isEmpty()) {
            st.append("Disjoints:\n");
            for(int[] clause : disjoints)    {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(!equivalences.isEmpty()) {
            st.append("Equivalences:\n");
            for(int[] clause : equivalences) {st.append(clauseToString(size,clause,symboltable)).append("\n");}}

        return st.toString();}



}
