
package Datastructures.Clauses;

import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;

import java.util.ArrayList;
import java.util.Arrays;

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
 * '2': means exclusive-or: '2 3 4 5'  means 3 xor 4 xor 5 (exactly one of them must be true).<br>
 * '3': means disjoints   : '3 4 5 -6' means 4,5,-6 are disjoint literals (at most one of them can be true).<br>
 * '4': means equivalences: '4 4 5 -6' means that these three literals are equivalent.
 * <br>
 *  Clauses with double literals or complementary literals are considered syntactically wrong,
 *  and not added to the lists..
 */
public class BasicClauseList {
    public int predicates = 0;

    /** the original disjunctions */
    public ArrayList<int[]> disjunctions  = new ArrayList<>();
    /** the original conjunctions */
    public ArrayList<int[]> conjunctions  = new ArrayList<>();
    /** the original equivalences */
    public ArrayList<int[]> equivalences  = new ArrayList<>();
    /** the original atleasts */
    public ArrayList<int[]> atleasts      = new ArrayList<>();
    /** the original atmosts */
    public ArrayList<int[]> atmosts       = new ArrayList<>();
    /** the original exactlys */
    public ArrayList<int[]> exactlys      = new ArrayList<>();


    /** an info-string about the origin of the clauses */
    public String info = null;

    /** the largest clause length */
    public int maxClauseLength = 0;

    /** the largest clause index */
    public int maxIndex = 0;

    /** is set in the generators */
    public Symboltable symboltable = null;

    public StringBuffer syntaxErrors = new StringBuffer();

    public boolean isNumericClause(int[] clause) {
        return ClauseType.isNumericType(clause[1]);}

    /** adds the clauses to the corresponding lists.
     * If a clause is syntactically wrong, error messages are appended to syntaxErrors.
     * Double literals and complementary literals, although logically okay
     * also count as syntactic errors (very likely this was not intended).
     * These clauses are not added to the lists.
     *
     * @param clauses a list of clauses
     */
    public void addClauses(int[]... clauses) {
        for(int[] clause : clauses) {addClause(clause);}}


    /** adds a clause to the corresponding lists.
     * If the clause is syntactically wrong, error messages are appended to syntaxErrors.
     * Double literals and complementary literals, although logically okay
     * also count as syntactic errors (very likely this was not intended).
     * These clauses are not added to the lists.
     *
     * @param clause a clause
     */
    public void addClause(int[] clause) {
        if(!checkSyntax(clause)) {return;}
        maxClauseLength = Math.max(maxClauseLength,clause.length-2);
        maxIndex        = Math.max(clause[0],maxIndex);
        switch(ClauseType.getType(clause[1])) {
            case OR:
                if(clause.length == 3) {
                    clause[1] = ClauseType.AND.ordinal();
                    conjunctions.add(clause);}
                else disjunctions.add(clause);       break;
            case AND:      conjunctions.add(clause); break;
            case EQUIV:    equivalences.add(clause); break;
            case ATLEAST:  atleasts.add(clause);     break;
            case ATMOST:   atmosts.add(clause);      break;
            case EXACTLY:  exactlys.add(clause);     break;}
    }

    /** checks the clause's syntax.
     *  Syntax errors are wrong clause type, wrong literal numbers or double or complementary literals.
     *  These errors are appended to syntaxErrors.
     *
     * @param clause the clause to be checked
     * @return true if there is no syntax error.
     */
    private boolean checkSyntax(int[] clause) {
        int type = clause[1];
        if(type < 0 || type > 5) {
            syntaxErrors.append("Clause '").append(Arrays.toString(clause)).
                    append("' contains illegal type ").append(type).append(".\n");
            return false;}
        int start = 2;
        boolean isNumeric = ClauseType.isNumericType(type);
        int size = clause.length;
        if(isNumeric) {
            start = 3;
            if(clause[2] <= 0 || clause[2] > size-3)
                syntaxErrors.append("Clause '").append(clauseToString(0, clause, symboltable)).
                append("' contains illegal number: ").append(clause[2]);
            return false;}
        for(int i = start; i < size-1; ++i) {
            int literal = clause[i];
            if(literal == 0 || (predicates > 0 && Math.abs(literal) > predicates)) {
                syntaxErrors.append("Clause '").append(clauseToString(0, clause, symboltable)).
                        append("' contains illegal literal ").append(literal).
                        append(".\n");
                return false;}}
        return true; }


    /** computes a list of clauses which are false in a model.
     *  This indicates that something went terribly wrong.
     *  If the model is partial, clauses whose truth value is not determined are not listed.
     *
     * @param model a model for the literals of the clause
     * @return null or a list of false clauses.
     */
    public ArrayList<int[]> falseClausesInModel(Model model) {
        ArrayList<int[]> falseClauses = new ArrayList<>();
        for(int[] clause : disjunctions) {if(disjunctionIsFalse(clause,model)) {falseClauses.add(clause);}}
        for(int[] clause : conjunctions) {if(conjunctionIsFalse(clause,model)) {falseClauses.add(clause);}}
        for(int[] clause : equivalences) {if(equivalenceIsFalse(clause,model)) {falseClauses.add(clause);}}
        for(int[] clause : atleasts)     {if(atleastIsFalse(clause,model))     {falseClauses.add(clause);}}
        for(int[] clause : atmosts)      {if(atmostIsFalse(clause,model))      {falseClauses.add(clause);}}
        for(int[] clause : exactlys)     {if(exactlyIsFalse(clause,model))     {falseClauses.add(clause);}}
        return falseClauses.isEmpty() ? null : falseClauses;}


       /** checks if a disjunction is entirely false in a model.
     *
     * @param clause a disjunctive clause
     * @param model a model
     * @return true if all literals are false in the model and the clause is not a tautology.
     */
    public static boolean disjunctionIsFalse(int[] clause, Model model) {
        assert ClauseType.getType(clause[1]) == ClauseType.OR;
        for(int i = 2; i < clause.length; ++i) {if(!model.isFalse(clause[i])) {return false;}}
        return true;}


    /** checks if a conjunction is entirely false in a model
     *
     * @param clause a conjunctive clause
     * @param model a possibly partial model
     * @return true if all literals are true in the model.
     */
    public static boolean conjunctionIsFalse(int[] clause, Model model) {
        for(int i = 2; i < clause.length; ++i) {if(model.isFalse(clause[i])) {return true;}}
        return false;}


    /** checks if a atleast-clause is false in a model
     *
     * @param clause a atleast clause  [id,type,n,literal1,...]
     * @param model a model
     * @return true if not atleast n literals are true in the model
     */
    public static boolean atleastIsFalse(int[] clause, Model model) {
        assert ClauseType.getType(clause[1]) == ClauseType.ATLEAST;
        int n = clause[2];
        int size = clause.length;
        int trueLiterals = 0;
        for(int i = 3; i < size; ++i) {
            if(model.isTrue(clause[i])) ++trueLiterals;}
        return trueLiterals < n;}

    /** checks if a atmost-clause is false in a model
     *
     * @param clause a atleast clause  [id,type,n,literal1,...]
     * @param model a model
     * @return true if not atleast n literals are true in the model
     */
    public static boolean atmostIsFalse(int[] clause, Model model) {
        assert ClauseType.getType(clause[1]) == ClauseType.ATMOST;
        int n = clause[2];
        int size = clause.length;
        int trueLiterals = 0;
        for(int i = 3; i < size; ++i) {
            if(model.isTrue(clause[i])) ++trueLiterals;}
        return trueLiterals > n;}

    /** checks if an exactly-clause is false in a model
     *
     * @param clause a atleast clause  [id,type,n,literal1,...]
     * @param model a model
     * @return true if not atleast n literals are true in the model
     */
    public static boolean exactlyIsFalse(int[] clause, Model model) {
        assert ClauseType.getType(clause[1]) == ClauseType.EXACTLY;
        int n = clause[2];
        int size = clause.length;
        int trueLiterals = 0;
        for(int i = 3; i < size; ++i) {
            if(model.isTrue(clause[i])) ++trueLiterals;}
        return trueLiterals != n;}

    /** checks if an equivalence is false in a model
     *
     * @param clause an equivalence clause
     * @param model a model
     * @return true if not either all literals are true or all literals are false in the model.
     */
    public static boolean equivalenceIsFalse(int[] clause, Model model) {
        assert ClauseType.getType(clause[1]) == ClauseType.EQUIV;
        int size = clause.length;
        int trueLiterals = 0;
        int falseLiterals = 0;
        for(int i = 2; i < size; ++i) {
            switch(model.status(clause[i])) {
                case +1: ++trueLiterals; break;
                case -1: ++falseLiterals;}}
        size -= 2;
        return (trueLiterals != size || falseLiterals != size);}


    /** adds some parameters to the statistics
     *
     * @param problemId the problem identifier
     * @return the BasicClauseStatistics
     */
    public Statistic getStatistics(String problemId) {
        BasicClauseStatistics statistics = new BasicClauseStatistics(problemId);
        statistics.disjunctions = disjunctions.size();
        statistics.conjunctions = conjunctions.size();
        statistics.equivalences = equivalences.size();
        statistics.atleasts     = atleasts.size();
        statistics.atmosts      = atmosts.size();
        statistics.exactlys     = exactlys.size();
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
        st.append(String.format("%"+size+"d ",clause[0])).append(": ");
        int typeNumber = clause[1];
        ClauseType type = ClauseType.getType(typeNumber);
        int start = 2;
        if(ClauseType.isNumericType(typeNumber)) {
            start = 3;
            st.append(type.toString() + " " + clause[2] + " ");}
        String separator = type.separator;
        int length = clause.length;
        for(int i = start; i < length-1; ++i) {
            st.append(Symboltable.toString(clause[i],symboltable));
            st.append(separator);}
        st.append(Symboltable.toString(clause[length-1],symboltable));
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
        Symboltable symboltable = withSymboltable ? this.symboltable : null;
        StringBuilder st = new StringBuilder();
        if(info != null) {st.append(info).append("\n");}
        int size = (""+(disjunctions.size() + conjunctions.size()  +equivalences.size()) +
                atleasts.size() + atmosts.size() + exactlys.size()).length();
        if(!disjunctions.isEmpty()) {
            st.append("Disjunctions:\n");
            for(int[] clause : disjunctions) {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(!conjunctions.isEmpty()) {
            st.append("Conjunctions:\n");
            for(int[] clause : conjunctions) {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(!equivalences.isEmpty()) {
            st.append("Equivalences:\n");
            for(int[] clause : equivalences) {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(!atleasts.isEmpty()) {
            st.append("Atleast:\n");
            for(int[] clause : atleasts)     {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(!atmosts.isEmpty()) {
            st.append("Atmost:\n");
            for(int[] clause : atmosts)      {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(!exactlys.isEmpty()) {
            st.append("Exactly:\n");
            for(int[] clause : exactlys)     {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        return st.toString();}


}
