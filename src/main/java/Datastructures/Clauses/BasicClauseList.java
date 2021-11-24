
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
 * A clause is an integer-array [clause-number,clause-type,[quantifier],literal1,...]<br>
 * The clause types are: <br>
 * '0': means disjunction:  '0 1 3 5'    means 1 or 3 or 5<br>
 * '1': means and:          '1 3 4 5'    stands for 3 and 4 and 5.<br>
 * '2': means equivalences: '2 4 5 -6'   means that these three literals are equivalent. <br>
 * '3': means atleast:      '3 2 4 5 6'  means atleast 2 of 4,5,6 are true <br>
 * '4': means atmost:       '4 2 4 5 6'  means atmost 2 of 4,5,6 are true <br>
 * '5': means exactly:      '5 2 4 5 6'  means exactly 2 of 4,5,6 are true
 */
public class BasicClauseList {
    /** the maximum number of predicates */
    public int predicates = 0;
    /** the largest clause length */
    public int maxClauseLength = 0;
    /** null or a symboltable */
    public Symboltable symboltable = null;
    /** the original disjunctions */
    public ArrayList<int[]> disjunctions  = new ArrayList<>();
    /** the original conjunctions */
    public ArrayList<int[]> conjunctions  = new ArrayList<>();
    /** the original equivalences */
    public ArrayList<int[]> equivalences  = new ArrayList<>();
    /** the original quantifieds atleast, atmost, exactly */
    public ArrayList<int[]> quantifieds   = new ArrayList<>();
    /** the original intervals */
    public ArrayList<int[]> intervals      = new ArrayList<>();

    /** an info-string about the origin of the clauses */
    public String info = null;

    /** adds the clauses to the corresponding lists.
     *
     * @param clauses a list of clauses
     * @return null or a string if clauses are syntactically wrong or contain double literals.
     */
    public String addClauses(int[]... clauses) {
        StringBuilder errors   = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        for (int[] clause : clauses) addClause(clause,"",errors,warnings);
        if(errors.length() == 0  && warnings.length() == 0) return null;
        return errors.toString() + warnings;}


    /** adds a clause to the corresponding lists.
     *  Erroneous clauses are not added to the lists
     *
     * @param clause a clause
     */
    public void addClause(int[] clause, String errorPrefix, StringBuilder errors, StringBuilder warnings) {
        clause = checkSyntax(clause,errorPrefix, errors, warnings);
        if(clause == null) return;
        Connective connective = Connective.getType(clause[1]);
        if(connective == null) {
            errors.append(errorPrefix).append("Clause " + Arrays.toString(clause) + " has unknown clause type "
            + clause[1]);
            return;}
        int length = clause.length-2;
        if(connective.isQuantifier()) length -= 1;
        else {if(connective == Connective.INTERVAL) length -= 2;}
        maxClauseLength = Math.max(maxClauseLength,length);
        switch(connective) {
            case OR:       disjunctions.add(clause); break;
            case AND:      conjunctions.add(clause); break;
            case EQUIV:    equivalences.add(clause); break;
            case ATLEAST:
            case ATMOST:
            case EXACTLY:  quantifieds.add(clause);  break;
            default:       intervals.add(clause);    break;}
    }

    /** checks the clause's syntax.
     *  Syntax errors are wrong clause type or wrong literal numbers
     *  These errors are appended to syntaxErrors.
     *  Double literals are removed and a warning is added to warnings.
     *
     * @param clause the clause to be checked
     * @param errorPrefix  a prefix for the warnings
     * @param errors       for adding error messages
     * @param warnings     for adding a warning
     * @return null or the shortened clause
     */
    protected int[] checkSyntax(int[] clause, String errorPrefix, StringBuilder errors, StringBuilder warnings) {
        int type = clause[1];
        if(type < 0 || type > 5) {errors.append(errorPrefix).append("Clause type :"+type + " is not between 0 and 5\n"); return null;}
        int start = 2;
        boolean isNumeric = Connective.isQuantifier(type);
        int size = clause.length;
        if(isNumeric) {
            start = 3;
            if(clause[2] <= 0) {
                {errors.append(errorPrefix).append("Quantifier: " + clause[2] + " < 1\n"); return null;}}
            if(clause[2] > size-3) {
                {errors.append(errorPrefix).append("Quantifier: " + clause[2] + " is larger than the clause\n"); return null;}}}
        for(int i = start; i < size; ++i) {
            int literal = clause[i];
            if(literal == 0 || (predicates > 0 && Math.abs(literal) > predicates)) {
                errors.append(errorPrefix).
                        append("Literal " + literal + " is not within the predicate boundaries: " + predicates + "\n");
                return null;}}
        return clause;}



    /** computes a list of clauses which are false in a model.
     *  This indicates that something went terribly wrong.
     *  If the model is partial, clauses whose truth value is not determined are not listed.
     *
     * @param model a model for the literals of the clause
     * @return null or a list of false clauses.
     */
    public ArrayList<int[]> falseClausesInModel(Model model) {
        ArrayList<int[]> falseClauses = new ArrayList<>();
        for(int[] clause : disjunctions) {if(!disjunctionIsTrue(clause,model)) {falseClauses.add(clause);}}
        for(int[] clause : conjunctions) {if(!conjunctionIsTrue(clause,model)) {falseClauses.add(clause);}}
        for(int[] clause : equivalences) {if(!equivalenceIsTrue(clause,model)) {falseClauses.add(clause);}}
        for(int[] clause : quantifieds)  {if(!quantifiedIsTrue(clause,model))  {falseClauses.add(clause);}}
        for(int[] clause : intervals)    {if(!intervalIsTrue(clause,model))    {falseClauses.add(clause);}}
        return falseClauses.isEmpty() ? null : falseClauses;}


       /** checks if a disjunction is entirely false in a model.
     *
     * @param clause a disjunctive clause
     * @param model a model
     * @return true if all literals are false in the model and the clause is not a tautology.
     */
    public static boolean disjunctionIsTrue(int[] clause, Model model) {
        assert Connective.getType(clause[1]) == Connective.OR;
        for(int i = 2; i < clause.length; ++i) {if(model.isTrue(clause[i])) {return true;}}
        return false;}


    /** checks if a conjunction is entirely false in a model
     *
     * @param clause a conjunctive clause
     * @param model a possibly partial model
     * @return true if all literals are true in the model.
     */
    public static boolean conjunctionIsTrue(int[] clause, Model model) {
        for(int i = 2; i < clause.length; ++i) {if(model.isFalse(clause[i])) {return false;}}
        return true;}
    /** checks if an equivalence is false in a model
     *
     * @param clause an equivalence clause
     * @param model a model
     * @return true if not either all literals are true or all literals are false in the model.
     */
    public static boolean equivalenceIsTrue(int[] clause, Model model) {
        assert Connective.getType(clause[1]) == Connective.EQUIV;
        int size = clause.length;
        int trueLiterals = 0;
        int falseLiterals = 0;
        for(int i = 2; i < size; ++i) {
            switch(model.status(clause[i])) {
                case +1: ++trueLiterals; break;
                case -1: ++falseLiterals;}}
        size -= 2;
        return (trueLiterals == size || falseLiterals == size);}

    /** checks if a atleast-clause is false in a model
     *
     * @param clause a atleast clause  [id,type,n,literal1,...]
     * @param model a model
     * @return true if not atleast n literals are true in the model
     */
    public static boolean quantifiedIsTrue(int[] clause, Model model) {
        int n = clause[2];
        int size = clause.length;
        int trueLiterals = 0;
        for(int i = 3; i < size; ++i) {
            if(model.isTrue(clause[i])) ++trueLiterals;}
        switch(Connective.getType(clause[1])) {
            case ATLEAST: return trueLiterals >= n;
            case ATMOST:  return trueLiterals <= n;
            case EXACTLY: return trueLiterals == n;}
        return false;}

    /** checks if a atmost-clause is false in a model
     *
     * @param clause a atleast clause  [id,type,n,literal1,...]
     * @param model a model
     * @return true if not atleast n literals are true in the model
     */
    public static boolean intervalIsTrue(int[] clause, Model model) {
        assert Connective.getType(clause[1]) == Connective.INTERVAL;
        int min = clause[2];
        int max = clause[3];
        int size = clause.length;
        int trueLiterals = 0;
        for(int i = 3; i < size; ++i) {
            if(model.isTrue(clause[i])) ++trueLiterals;}
        return min <= trueLiterals && trueLiterals <= max;}


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
        statistics.intervals    = intervals.size();
        for(int[] clause : quantifieds) {
            switch(Connective.getType(clause[1])) {
                case ATLEAST: ++statistics.atleasts; break;
                case ATMOST:  ++statistics.atmosts;  break;
                case EXACTLY: ++statistics.exactlys; break;}}
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
        Connective connective = Connective.getType(typeNumber);
        int start = 2;
        if(Connective.isQuantifier(typeNumber)) {
            start = 3;
            st.append(connective.prefix + " " + clause[2] + " ");}
        else{if(connective == Connective.INTERVAL) {
            start = 4;
            st.append(connective.prefix + " ");
            if(clause[2] == clause[3]) st.append(clause[2] + ": ");
            else st.append(clause[2] + "-" + clause[3]+": ");}}
        String separator = connective.separator;
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
        return toString(symboltable);}

    /** generates a string representation of the clause
     *
     * @param symboltable null or a symboltable
     * @return  a string representation of the disjunctions.
     */
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        if(info != null) {st.append(info).append("\n");}
        int size = (""+(disjunctions.size() + conjunctions.size()  +equivalences.size()) +
                quantifieds.size() + intervals.size()).length();
        if(!disjunctions.isEmpty()) {
            st.append("Disjunctions:\n");
            for(int[] clause : disjunctions) {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(!conjunctions.isEmpty()) {
            st.append("Conjunctions:\n");
            for(int[] clause : conjunctions) {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(!equivalences.isEmpty()) {
            st.append("Equivalences:\n");
            for(int[] clause : equivalences) {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(!quantifieds.isEmpty()) {
            st.append("Quantifieds:\n");
            for(int[] clause : quantifieds)     {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(!intervals.isEmpty()) {
            st.append("Intervals:\n");
            for(int[] clause : intervals)      {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        return st.toString();}


}
