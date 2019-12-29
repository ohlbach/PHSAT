
package Datastructures.Clauses;

import Datastructures.Statistics.Statistic;
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
 * <br>
 *  Clauses are not checked for internal double literals or complementary literals.
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

    public int maxClauseLength = 0;

    /** is set in the generators */
    public Symboltable symboltable = null;

    /** adds a clause to the corresponding lists
     *
     * @param clause a clause
     */
    public void addClause(int[] clause) {
        maxClauseLength = Math.max(maxClauseLength,clause.length-2);
        switch(ClauseType.getType(clause[1])) {
            case OR:       disjunctions.add(clause);break;
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
     * It does not check for tautologies.
     *
     * @param clause a disjunctive clause
     * @param model a model
     * @return true if at least one of the literals is true in the model.
     */
    public static boolean disjunctionIsTrue(int[] clause, Model model) {
        assert ClauseType.getType(clause[1]) == ClauseType.OR;
        for(int i = 2; i < clause.length; ++i) {if(model.isTrue(clause[i])) {return true;}}
        for(int i = 2; i < clause.length; ++i) {
            int literal = clause[i];
            if(model.status(literal) == 0) {model.setStatus(literal,1); return true;}}
        return false;}

    /** checks if a disjunction is entirely false in a model
     *
     * @param clause a disjunctive clause
     * @param model a possibly partial model
     * @return true if all literals are false in the model.
     */
    public static boolean disjunctionIsFalse(int[] clause, Model model) {
        assert ClauseType.getType(clause[1]) == ClauseType.OR;
        int size = clause.length;
        for(int i = 2; i < size; ++i) {
            int lit1 = clause[i];
            for(int j = i+1; j < size; ++j) {if(lit1 == -clause[j]) {return false;}}}
        for(int i = 2; i < size; ++i) {if(!model.isFalse(clause[i])) {return false;}}
        return true;}



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

    /** checks if a conjunction is entirely false in a model
     *
     * @param clause a conjunctive clause
     * @param model a possibly partial model
     * @return true if all literals are true in the model.
     */
    public static boolean conjunctionIsFalse(int[] clause, Model model) {
        assert ClauseType.getType(clause[1]) == ClauseType.AND;
        int size = clause.length;
        for(int i = 2; i < size; ++i) {
            int lit1 = clause[i];
            for(int j = i+1; j < size; ++j) {if(lit1 == -clause[j]) {return true;}}}
        for(int i = 2; i < size; ++i) {if(model.isFalse(clause[i])) {return true;}}
        return false;}

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

    /** checks if a xors-clause is false in a model
     *
     * @param clause a xors clause
     * @param model a possibly partial model
     * @return true if not exactly one of the literals is true in the model.
     */
    public static boolean xorIsFalse(int[] clause, Model model) {
        assert ClauseType.getType(clause[1]) == ClauseType.XOR;
        int size = clause.length;
        int trueLiteral = 0;
        for(int i = 2; i < size; ++i) {
            int lit1 = clause[i];
            boolean found = false;
            for(int j = i+1; j < size; ++j) {if(lit1 == -clause[j]) {found = true; break;}}
            if(found){continue;}
            if(model.isTrue(lit1)) {
                if(trueLiteral != 0) {return true;}
                trueLiteral = clause[i];}}
        return false;}

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

    /** checks if a disjoint clause is true in a model
     *
     * @param clause a disjoint clause
     * @param model a model
     * @return true if at most one of the literals is true in the model.
     */
    public static boolean disjointIsFalse(int[] clause, Model model) {
        assert ClauseType.getType(clause[1]) == ClauseType.DISJOINT;
        int size = clause.length;
        int trueLiteral = 0;
        for(int i = 2; i < clause.length; ++i) {
            int lit1 = clause[i];
            boolean found = false;
            for(int j = i+1; j < size; ++j) {if(lit1 == -clause[j]) {found = true; break;}}
            if(found) {continue;}
            if(model.isTrue(clause[i])) {
                if(trueLiteral != 0) {return true;}
                trueLiteral = clause[i];}}
        return false;}

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

    /** checks if an equivalence is false in a model
     *
     * @param clause an equivalence clause
     * @param model a possibly partial model
     * @return true if either all literals are true or all literals are false in the model.
     */
    public static boolean equivalenceIsFalse(int[] clause, Model model) {
        assert ClauseType.getType(clause[1]) == ClauseType.EQUIV;
        int size = clause.length;
        int status = 0;
        for(int i = 2; i < clause.length; ++i) {
            int lit1 = clause[i];
            for(int j = i+1; j < size; ++j) {if(lit1 == -clause[j]) {return true;}}

            int stat = model.status(clause[i]);
            if(stat == 0) {continue;}
            if(status != 0 && status != stat) {return true;}
            status = stat;}
        return false;}




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

    /** computes a list of clauses which are false in the model
     *
     * @param model a model for the literals of the clause
     * @return null or a list of false clauses.
     */
    public ArrayList<int[]> falseClausesInPartial(Model model) {
        ArrayList<int[]> falseClauses = new ArrayList<>();
        for(int[] clause : disjunctions) {if(disjunctionIsFalse(clause,model)) {falseClauses.add(clause);}}
        for(int[] clause : conjunctions) {if(conjunctionIsFalse(clause,model)) {falseClauses.add(clause);}}
        for(int[] clause : xors)         {if(xorIsFalse(clause,model))         {falseClauses.add(clause);}}
        for(int[] clause : disjoints)    {if(disjointIsFalse(clause,model))    {falseClauses.add(clause);}}
        for(int[] clause : equivalences) {if(equivalenceIsFalse(clause,model)) {falseClauses.add(clause);}}
        return falseClauses.isEmpty() ? null : falseClauses;}



    /** adds some parameters to the statistics
     *
     * @param problemId the problem identifier
     * @returns the BasicClauseStatistics
     */
    public Statistic getStatistics(String problemId) {
        BasicClauseStatistics statistics = new BasicClauseStatistics(problemId);
        statistics.disjoints    = disjoints.size();
        statistics.disjunctions = disjunctions.size();
        statistics.conjunctions = conjunctions.size();
        statistics.xors         = xors.size();
        statistics.equivalences = equivalences.size();
        return statistics;}

    /** turns a clause into a string.
     *
     * @param clause     the clause
     * @return the clause as string
     */
    public String clauseToString(int[] clause) {
        return clauseToString((""+clause[0]).length(),clause,null);}


    /** turns a clause into a string.
     *
     * @param size       the length for the number string
     * @param clause     the clause
     * @param symboltable a symboltable or null
     * @return the clause as string
     */
    public static String clauseToString(int size, int[] clause, Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        if(size == 0) {size = Integer.toString(clause[0]).length();}
        st.append(String.format("%"+size+"d ",clause[0]));
        String separator = ",";
        switch(ClauseType.getType(clause[1])) {
            case OR:       separator = " | "; break;
            case AND:      separator = " & "; break;
            case XOR:      separator = " x "; break;
            case EQUIV:    separator = " = "; break;
            case DISJOINT: separator = " /= "; break;}
        st.append(": ");
        int length = clause.length;
        for(int i = 2; i < length-1; ++i) {
            if(symboltable != null) {st.append(symboltable.getLiteralName(clause[i]));}
            else                    {st.append(Integer.toString(clause[i]));}
            st.append(separator);}
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
     * @param withSymboltable if the symboltable is to be used
     * @return  a string representation of the disjunctions.
     */
    public String toString(boolean withSymboltable) {
        Symboltable symboltable1 = withSymboltable ? this.symboltable : null;
        StringBuilder st = new StringBuilder();
        if(info != null) {st.append(info).append("\n");}
        int size = (""+(disjunctions.size() + conjunctions.size() + xors.size() + disjoints.size()+equivalences.size())).length();
        if(disjunctions != null && !disjunctions.isEmpty()) {
            st.append("Disjunctions:\n");
            for(int[] clause : disjunctions) {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(conjunctions != null && !conjunctions.isEmpty()) {
            st.append("Conjunctions:\n");
            for(int[] clause : conjunctions) {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(xors != null && !xors.isEmpty()) {
            st.append("Xor:\n");
            for(int[] clause : xors)          {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(disjoints != null && !disjoints.isEmpty()) {
            st.append("Disjoints:\n");
            for(int[] clause : disjoints)    {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(equivalences != null && !equivalences.isEmpty()) {
            st.append("Equivalences:\n");
            for(int[] clause : equivalences) {st.append(clauseToString(size,clause,symboltable)).append("\n");}}

        return st.toString();}


     public String testString(String var) {
         StringBuilder st = new StringBuilder();
         st.append(info);
         int counter = 0;
         for(int[] clause : disjunctions) {
             st.append(var);
             st.append(".addClause(Utilities.makeClause(\"").append(Integer.toString(++counter)).append("\",\"");
             for(int i = 2; i < clause.length; ++i) {
                 st.append(Integer.toString(clause[i]));
                 if(i < clause.length-1) st.append(",");}
             st.append("\"));\n");}
        return st.toString();}
}
