
package Datastructures.Clauses;

import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.concurrent.ScheduledExecutorService;
import java.util.function.IntSupplier;

/**
 * Created by Ohlbach on 03.09.2018.<br>
 *
 * This class contains just the absolutely essential information about clauses.<br>
 * The clauses should be the original clauses from the clause source and must not be changed.<br>
 * They are in particular used to check a candidate model against the original clause set.<br>
 * A clause is an integer-array [clause-number,connective,[min,max],literal1,...].<br>
 * The connectives are: <br>
 * '0': means disjunction:  '10 0 3 5'      means 1 or 3 or 5.<br>
 * '1': means and:          '11 1 4 5'      means 3 and 4 and 5.<br>
 * '2': means equivalences: '12 2 5 -6'     means that these three literals are equivalent. <br>
 * '4': means atleast:      '13 3 4 5 6'    means atleast 2 of 4,5,6 are true. <br>
 * '5': means atmost:       '14 4 4 5 6'    means atmost 2 of 4,5,6 are true. <br>
 * '6': means exactly:      '15 5 4 5 6'    means exactly 2 of 4,5,6 are true.<br>
 * '3': means interval      '16 6 2 3 5 6 7 8 means between 2 and 3 literals among 5,6,7,8 are true.<br>
 */
public class InputClauses {
    private static final int cOr = Connective.OR.ordinal();
    private static final int cAtleast = Connective.ATLEAST.ordinal();
    private static final int cAtmost = Connective.ATMOST.ordinal();
    private static final int cExactly = Connective.EXACTLY.ordinal();
    private static final int cInterval = Connective.INTERVAL.ordinal();

    /** the name of the problem */
    public String problemName = null;

    /** the maximum number of predicates. */
    public int predicates;

    /** null or a symboltable. */
    public Symboltable symboltable;

    /** an info-string about the origin of the clauses. */
    public String info = null;

    /** the next free number to be used as identifier for a clause. */
    public int nextId = 0;

    /** the number of literals in the largest clause.*/
    public int maxClauseLength;

    /** the original disjunctions. */
    public final ArrayList<int[]> disjunctions  = new ArrayList<>();

    /** the original conjunctions. */
    public final ArrayList<int[]> conjunctions  = new ArrayList<>();

    /** the original equivalences. */
    public final ArrayList<int[]> equivalences  = new ArrayList<>();

    /** the original atleast clauses. */
    public final ArrayList<int[]> atleasts      = new ArrayList<>();

    /** the original atmost clauses. */
    public final ArrayList<int[]> atmosts       = new ArrayList<>();

    /** the original exactly clauses. */
    public final ArrayList<int[]> exacltys      = new ArrayList<>();

    /** the original interval clauses. */
    public final ArrayList<int[]> intervals     = new ArrayList<>();


    /** constructs a new input clause list.
     *
     * @param problemName the name of the problem.
     * @param predicates  the number of predicates which are allowed in the clauses.
     * @param symboltable null or a symboltable.
     * @param info        some information about the origin of the clause set.
     */
    public InputClauses(String problemName, int predicates, Symboltable symboltable, String info) {
        this.problemName = problemName;
        this.predicates = predicates;
        this.symboltable = symboltable;
        this.info = info;}

    /** constructs a new input clause list without any extra information.
     */
    public InputClauses() {}

    /** adds a clause to the corresponding lists.
     *  Erroneous clauses are not added to the lists.
     *
     * @param clauses      some clauses.
     */
    public void addClause(int[]... clauses) {
        for(int[] clause : clauses) {
            Connective connective = Connective.getConnective(clause[1]);
            assert connective != null;
            int length = clause.length-2;
            switch(connective) {
                case OR:       disjunctions.add(clause); maxClauseLength = Math.max(maxClauseLength,length);   break;
                case AND:      conjunctions.add(clause); maxClauseLength = Math.max(maxClauseLength,length);   break;
                case EQUIV:    equivalences.add(clause); maxClauseLength = Math.max(maxClauseLength,length);   break;
                case INTERVAL: intervals.add(clause);    maxClauseLength = Math.max(maxClauseLength,length-2); break;
                case ATLEAST:  atleasts.add(clause);     maxClauseLength = Math.max(maxClauseLength,length-1);  break;
                case ATMOST:   atmosts.add(clause);      maxClauseLength = Math.max(maxClauseLength,length-1);  break;
                case EXACTLY:  exacltys.add(clause);     maxClauseLength = Math.max(maxClauseLength,length-1);  break;}}
    }

    /** checks if the clause has double literals
     *
     * @param clause a clause
     * @param start the start index of the literal section
     * @return true if there are double literals in the clause.
     */
    public static boolean hadDoubles(int[] clause, int start) {
        int length = clause.length;
        for(int i = start; i < length; ++i) {
            int literal = clause[i];
            for(int j = i+1; j < length; ++j) {
                if(clause[j] == literal) return true;}}
        return false;}

    /** checks the clause's syntax.
     *  Syntax errors are wrong clause type or wrong literal numbers.
     *  These errors are appended to syntaxErrors.
     *  Double literals are removed and a warning is added to warnings.
     *
     * @param clause the   clause to be checked.
     * @param predicates   the number of predicates in the clause set.
     * @param errorPrefix  a prefix for the warnings.
     * @param errors       for adding error messages.
     * @return null or the original clause.
     */
    public static int[] checkSyntax(int[] clause, int predicates, String errorPrefix, StringBuilder errors) {
        errorPrefix += "Clause " + Arrays.toString(clause) + ": ";
        int type = clause[1];
        Connective connective = Connective.getConnective(type);
        if(connective == null) {
            errors.append(errorPrefix).append("Connective number '").append(type).append("' is not between 0 and ").
                    append(Connective.size() - 1).append("\n");
            return null;}
        int start = connective.firstLiteralIndex;
        boolean erraneous = false;
        for(int i = start; i < clause.length; ++i) {
            int predicate = Math.abs(clause[i]);
            if(predicate == 0 || predicate > predicates) {
                errors.append(errorPrefix).append("Literal ").append(clause[i]).append(": predicate ").
                        append("is not within the boundaries [1,").append(predicates).append("]\n");
                erraneous = true;}}
        return erraneous ? null : clause;}

    /** turns an atmost input clause into an atleast clause<br>
     * Example: atmost 3 1,2,3,4 -> atleast 1 -1,-2,-3,-4 -> or -1,-2,-3,-4
     *
     * @param atmostClause
     * @return the new atleastClause.
     */
    public static int[] atmostToAtleast(int[] atmostClause) {
        assert(atmostClause[1] == cAtmost);
        int newLimit = atmostClause.length-3-atmostClause[2];
        if(newLimit == 1) {
            int[] atleastClause = Arrays.copyOf(atmostClause,atmostClause.length-1);
            atleastClause[1] = cOr;
            for(int i = 3; i < atmostClause.length; ++i) atleastClause[i-1] = -atmostClause[i];
            return atleastClause;}
        int[] atleastClause = Arrays.copyOf(atmostClause,atmostClause.length);
        atleastClause[1] = cAtleast;
        atleastClause[2] = newLimit;
        for(int i = 3; i < atmostClause.length; ++i) atleastClause[i] *= -1;
        return atleastClause;}

    /** turns an exactly input clause into two atleast clauses<br>
     * Example: exactly 3 1,2,3,4 -> atleast 3 1,2,3,4 and <br>
     *                               atleast 1 -1,-2,-3,-4 -> or -1,-2,-3,-4
     *
     * @param exactlyClause
     * @return the two new atleastClauses.
     */
    public static int[][] exactlyToAtleast(int[] exactlyClause, IntSupplier newId) {
        assert(exactlyClause[1] == cExactly);
        int[] atleastClause1;
        int limit = exactlyClause[2];
        if(limit == 1) {
            atleastClause1 = new int[exactlyClause.length-1];
            atleastClause1[0] = newId.getAsInt();
            atleastClause1[1] = cOr;
            System.arraycopy(exactlyClause,3,atleastClause1,2,exactlyClause.length-3);}
        else {
            atleastClause1 = Arrays.copyOf(exactlyClause,exactlyClause.length);
            atleastClause1[0] = newId.getAsInt();
            atleastClause1[1] = cAtleast;}

        int[] atleastClause2;
        int newLimit = exactlyClause.length-3-limit;
        if(newLimit == 1) {
            atleastClause2 = new int[exactlyClause.length-1];
            atleastClause2[0] = newId.getAsInt();
            atleastClause2[1] = cOr;
            for(int i = 3; i < exactlyClause.length; ++i) atleastClause2[i-1] = -exactlyClause[i];}
        else {
            atleastClause2 = Arrays.copyOf(exactlyClause,exactlyClause.length);
            atleastClause2[0] = newId.getAsInt();
            atleastClause2[1] = cAtleast;
            atleastClause2[2] = newLimit;
            for(int i = 3; i < atleastClause2.length; ++i) atleastClause2[i] *= -1;}
        int[][] atleastClauses = new int[2][];
        atleastClauses[0] = atleastClause1; atleastClauses[1] = atleastClause2;
        return atleastClauses;}

    /** turns an exactly input clause into two atleast clauses<br>
     * Example: [2,3] 1,2,3,4 -> atleast 2 1,2,3,4 and <br>
     *                           atleast 1 -1,-2,-3,-4 -> or -1,-2,-3,-4
     *
     * @param intervalClause
     * @return the two new atleastClauses.
     */
    public static int[][] intervalToAtleast(int[] intervalClause, IntSupplier newId) {
        assert(intervalClause[1] == cInterval);
        int min = intervalClause[2];
        int max = intervalClause[3];
        int[] atleastClause1;
        if(min == 1) {
            atleastClause1 = new int[intervalClause.length-2];
            atleastClause1[0] = newId.getAsInt();
            atleastClause1[1] = cOr;
            System.arraycopy(intervalClause,4,atleastClause1,2,intervalClause.length-4);}
        else {
            atleastClause1 = new int[intervalClause.length-1];
            atleastClause1[0] = newId.getAsInt();
            atleastClause1[1] = cAtleast;
            atleastClause1[2] = min;
            System.arraycopy(intervalClause,4,atleastClause1,3,intervalClause.length-4);
        }

        int[] atleastClause2;
        int newLimit = intervalClause.length-4-max;
        if(newLimit == 1) {
            atleastClause2 = new int[intervalClause.length-2];
            atleastClause2[0] = newId.getAsInt();
            atleastClause2[1] = cOr;
            for(int i = 4; i < intervalClause.length; ++i) atleastClause2[i-2] = -intervalClause[i];}
        else {
            atleastClause2 = new int[intervalClause.length-1];
            atleastClause2[0] = newId.getAsInt();
            atleastClause2[1] = cAtleast;
            atleastClause2[2] = newLimit;
            for(int i = 4; i < intervalClause.length; ++i) atleastClause2[i-1] = -intervalClause[i];}
        int[][] atleastClauses = new int[2][];
        atleastClauses[0] = atleastClause1; atleastClauses[1] = atleastClause2;
        return atleastClauses;}


    /** computes a list of clauses which are false in a model.
     *  This indicates that something went terribly wrong.
     *  If the model is partial, clauses whose truth value is not determined are not listed.
     *
     * @param model a model for the literals of the clause.
     * @return null or a list of false clauses.
     */
    public ArrayList<int[]> falseClausesInModel(Model model) {
        ArrayList<int[]> falseClauses = new ArrayList<>();
        for(int[] clause : disjunctions) {if(disjunctionIsFalse(clause,model)) {falseClauses.add(clause);}}
        for(int[] clause : conjunctions) {if(conjunctionIsFalse(clause,model)) {falseClauses.add(clause);}}
        for(int[] clause : equivalences) {if(equivalenceIsFalse(clause,model)) {falseClauses.add(clause);}}
        for(int[] clause : atleasts)  {if(quantifiedIsFalse(clause,model))  {falseClauses.add(clause);}}
        for(int[] clause : atmosts)  {if(quantifiedIsFalse(clause,model))  {falseClauses.add(clause);}}
        for(int[] clause : exacltys)  {if(quantifiedIsFalse(clause,model))  {falseClauses.add(clause);}}
        for(int[] clause : intervals)    {if(intervalIsFalse(clause,model))    {falseClauses.add(clause);}}
        return falseClauses.isEmpty() ? null : falseClauses;}


    /** checks if the clause contains literals p,-p.
     *
     * @param clause an and-, or-, equiv-clause.
     * @return true if the clause does not contain literals p,-p.
     */
    public static boolean containsComplementaryLiterals(int[] clause) {
        assert clause[1] <= 2;  // and-, or-, equiv- clauses
        int length = clause.length;
        for(int i = 2; i < length; ++i){
            int literal = clause[i];
            for(int j = i; j < length; ++j) {
                if(literal == -clause[j]) return true;}}
        return false;}

    /** counts the number of complementary pairs p,-p, which have no truth value.
     *
     * @param clause a quantified clause.
     * @param model a model.
     * @return the number of complementary literal pairs p,-p.
     */
    public static int numberOfComplementaryPairs(int[] clause, Model model) {
        assert clause[1] > 2; // quantified clauses
        int shift = Integer.MAX_VALUE/2;
        int length = clause.length;
        int start = clause[1] == Connective.INTERVAL.ordinal() ? 4 : 3;
        int pairs = 0;
        for(int i = start; i < length; ++i){
            int literal1 = clause[i];
            if(Math.abs(literal1) > shift || model.status(literal1) != 0 ) continue;
            for(int j = i; j < length; ++j) {
                int literal2 = clause[j];
                if(literal2 > shift) continue;
                if(literal2 == -literal1) {
                    ++pairs;
                    if(literal2 > 0) clause[j] += shift; else {clause[j] -= shift;}
                    break;}}}
        if(pairs > 0) { // undo the shifts
            for(int i = start; i < length; ++i){
                int literal = clause[i];
                if(Math.abs(literal) < shift) continue;
                if(literal > 0) clause[i] -= shift; else clause[i] += shift;}}
        return pairs;}


    /** checks if a disjunction is entirely false in a model.
     *
     * @param clause a disjunctive clause.
     * @param model a model.
     * @return true if all literals are either false or undefined in the model and the clause is not a tautology.
     */
    public static boolean disjunctionIsFalse(int[] clause, Model model) {
        assert Connective.getConnective(clause[1]) == Connective.OR;
        for(int i = 2; i < clause.length; ++i) {if(model.isTrue(clause[i])) {return false;}}
        if(model.isComplete()) return true; // there can't be complementary literals
        return !containsComplementaryLiterals(clause);} // p or -p is always true

    /** checks if a conjunction is entirely false in a model.
     *
     * @param clause a conjunctive clause.
     * @param model a possibly partial model.
     * @return true if one literal is false or undefined in the model.
     */
    public static boolean conjunctionIsFalse(int[] clause, Model model) {
        assert Connective.getConnective(clause[1]) == Connective.AND;
        for(int i = 2; i < clause.length; ++i) {if(!model.isTrue(clause[i])) {return true;}}
        return false;}

    /** checks if an equivalence is false in a model.
     *
     * @param clause an equivalence clause.
     * @param model a model.
     * @return true if not either all literals are true or all literals are false in the model.
     */
    public static boolean equivalenceIsFalse(int[] clause, Model model) {
        assert Connective.getConnective(clause[1]) == Connective.EQUIV;
        int size = clause.length;
        int trueLiterals = 0;
        int falseLiterals = 0;
        for(int i = 2; i < size; ++i) {
            switch(model.status(clause[i])) {
                case 0: return true;
                case +1: ++trueLiterals; break;
                case -1: ++falseLiterals;}}
        size -= 2;
        return !(trueLiterals == size || falseLiterals == size);}

    /** checks if an atleast-clause is false in a model.
     *
     * @param clause an atleast clause  [id,type,n,literal1,...].
     * @param model a model.
     * @return true if not atleast n literals are true in the model.
     */
    public static boolean quantifiedIsFalse(int[] clause, Model model) {
        int n = clause[2];
        int size = clause.length;
        int trueLiterals = 0;
        for(int i = 3; i < size; ++i) {if(model.isTrue(clause[i])) ++trueLiterals;}
        trueLiterals += model.isComplete() ? 0 : numberOfComplementaryPairs(clause,model);
        Connective connective = Connective.getConnective(clause[1]);
        assert connective != null;
        switch(connective) {
            case ATLEAST: return trueLiterals < n;
            case ATMOST:  return trueLiterals > n;
            case EXACTLY: return trueLiterals != n;}
        return false;}

    /** checks if an interval-clause is false in a model.
     *
     * @param clause an interval clause  [id,type,min,max,literal1,...].
     * @param model a model.
     * @return true if not between min and max literals are true in the model.
     */
    public static boolean intervalIsFalse(int[] clause, Model model) {
        assert Connective.getConnective(clause[1]) == Connective.INTERVAL;
        int min = clause[2];
        int max = clause[3];
        int size = clause.length;
        int trueLiterals = 0;
        for(int i = 4; i < size; ++i) {
            if(model.isTrue(clause[i])) ++trueLiterals;}
        trueLiterals += model.isComplete() ? 0 : numberOfComplementaryPairs(clause,model);
        return !(min <= trueLiterals && trueLiterals <= max);}


    /** adds some parameters to the statistics.
     *
     * @param problemId the problem identifier.
     * @return the InputClauseStatistics.
     */
    public Statistic getStatistics(String problemId) {
        InputClauseStatistics statistics = new InputClauseStatistics(problemId);
        statistics.disjunctions = disjunctions.size();
        statistics.conjunctions = conjunctions.size();
        statistics.equivalences = equivalences.size();
        statistics.intervals    = intervals.size();
        statistics.atleasts     = atleasts.size();
        statistics.atmosts      = atmosts.size();
        statistics.exactlys     = exacltys.size();
        return statistics;}

    /** turns a clause into a string, without using a symboltable.
     *
     * @param clause     the clause.
     * @return the clause as string.
     */
    public static String toString(int[] clause) {
        return toString((""+clause[0]).length()+2,clause,null);}

    /** collects the list of clauses in a string.
     *
     * @param clauses     a list of clauses.
     * @param symboltable null or a symboltable.
     * @return the list of clauses as a string.
     */
    public static String toString(ArrayList<int[]> clauses, Symboltable symboltable) {
        int size = 0;
        for(int[] clause : clauses) {size = Math.max(size, Integer.toString(clause[0]).length());}
        size += 3;
        StringBuilder st = new StringBuilder();
        for(int i = 0; i < clauses.size()-1; ++i) {
            st.append(toString(size,clauses.get(i),symboltable)).append("\n");}
        st.append(toString(size,clauses.get(clauses.size()-1),symboltable));
        return st.toString();
    }


    /** turns a clause into a string.
     *
     * @param size       the length for the identifier string.
     * @param clause     the clause.
     * @param symboltable a symboltable or null.
     * @return the clause as string.
     */
    public static String toString(int size, int[] clause, Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        int connectiveNumber = clause[1];
        Connective connective = Connective.getConnective(connectiveNumber);
        assert connective != null;
        if(size == 0) {size = Integer.toString(clause[0]).length();}
        st.append(String.format("%"+size+"s",clause[0])).append(": ");
        int start = 2;
        if(Connective.isQuantifier(connectiveNumber) && connective != Connective.INTERVAL) {
            start = 3;
            st.append(connective.abbreviation).append(" ");
            st.append(clause[2]).append(" ");}
        else{if(connective == Connective.INTERVAL) {
            start = 4;
            st.append(clause[2]).append("-").append(clause[3]).append(": ");}}
        String separator = connective.separator;
        int length = clause.length;
        for(int i = start; i < length-1; ++i) {
            st.append(Symboltable.toString(clause[i],symboltable));
            st.append(separator);}
        st.append(Symboltable.toString(clause[length-1],symboltable));
        return st.toString();}

    /** generates a string representation of the clause.
     *
     * @return  a string representation of the clause.
     */
    public String toString() {
        return toString(symboltable);}

    /** generates a string representation of the clauses.
     *
     * @param symboltable null or a symboltable.
     * @return  a string representation of the clauses.
     */
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        if(problemName != null) st.append("Problem ").append(problemName).append("\n");
        if(info != null) st.append(info).append("\n");
        int size = (""+(disjunctions.size() + conjunctions.size()  +equivalences.size()) +
                atleasts.size() + atmosts.size() +exacltys.size() +intervals.size()).length()+2;
        if(!disjunctions.isEmpty()) {
            st.append("Disjunctions:\n");
            for(int[] clause : disjunctions) {st.append(toString(size,clause,symboltable)).append("\n");}}
        if(!conjunctions.isEmpty()) {
            st.append("Conjunctions:\n");
            for(int[] clause : conjunctions) {st.append(toString(size,clause,symboltable)).append("\n");}}
        if(!equivalences.isEmpty()) {
            st.append("Equivalences:\n");
            for(int[] clause : equivalences) {st.append(toString(size,clause,symboltable)).append("\n");}}
        if(!atleasts.isEmpty()) {
            st.append("Atleasts:\n");
            for(int[] clause : atleasts)  {st.append(toString(size,clause,symboltable)).append("\n");}}
        if(!atmosts.isEmpty()) {
            st.append("Atmosts:\n");
            for(int[] clause : atmosts)  {st.append(toString(size,clause,symboltable)).append("\n");}}
        if(!exacltys.isEmpty()) {
            st.append("Exactlys:\n");
            for(int[] clause : exacltys)  {st.append(toString(size,clause,symboltable)).append("\n");}}
        if(!intervals.isEmpty()) {
            st.append("Intervals:\n");
            for(int[] clause : intervals)    {st.append(toString(size,clause,symboltable)).append("\n");}}
        return st.toString();}


}
