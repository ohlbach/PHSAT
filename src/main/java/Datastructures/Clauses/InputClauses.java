
package Datastructures.Clauses;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import Solvers.Simplifier.Literal;
import Solvers.Simplifier.UnsatClause;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
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
    private static final int cOr       = Quantifier.OR.ordinal();
    private static final int cEquiv    = Quantifier.EQUIV.ordinal();
    private static final int cAtleast  = Quantifier.ATLEAST.ordinal();
    private static final int cAtmost   = Quantifier.ATMOST.ordinal();
    private static final int cExactly  = Quantifier.EXACTLY.ordinal();
    private static final int cInterval = Quantifier.INTERVAL.ordinal();

    /** the name of the problem */
    public String problemId = null;

    /** the maximum number of predicates. */
    public int predicates;

    /** null or a symboltable. */
    public Symboltable symboltable = null;

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
    public final ArrayList<int[]> exactlys      = new ArrayList<>();

    /** the original interval clauses. */
    public final ArrayList<int[]> intervals     = new ArrayList<>();


    public final ArrayList<IntArrayList> simplifiedClauses = new ArrayList<>();
    public final IntArrayList trueLiterals = new IntArrayList();
    public final ArrayList<IntArrayList> equivalenceClasses = new ArrayList();

    public final ArrayList<Unsatisfiable> contradictions = new ArrayList<>();

    /** constructs a new input clause list.
     *
     * @param problemId the name of the problem.
     * @param predicates  the number of predicates which are allowed in the clauses.
     * @param symboltable null or a symboltable.
     * @param info        some information about the origin of the clause set.
     */
    public InputClauses(String problemId, int predicates, Symboltable symboltable, String info) {
        this.problemId = problemId;
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
            Quantifier quantifier = Quantifier.getQuantifier(clause[1]);
            assert quantifier != null;
            int length = clause.length-2;
            switch(quantifier) {
                case OR:       disjunctions.add(clause); maxClauseLength = Math.max(maxClauseLength,length);   break;
                case AND:      conjunctions.add(clause); maxClauseLength = Math.max(maxClauseLength,length);   break;
                case EQUIV:    equivalences.add(clause); maxClauseLength = Math.max(maxClauseLength,length);   break;
                case INTERVAL: intervals.add(clause);    maxClauseLength = Math.max(maxClauseLength,length-2); break;
                case ATLEAST:  atleasts.add(clause);     maxClauseLength = Math.max(maxClauseLength,length-1);  break;
                case ATMOST:   atmosts.add(clause);      maxClauseLength = Math.max(maxClauseLength,length-1);  break;
                case EXACTLY:  exactlys.add(clause);     maxClauseLength = Math.max(maxClauseLength,length-1);  break;}}
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
        Quantifier quantifier = Quantifier.getQuantifier(type);
        if(quantifier == null) {
            errors.append(errorPrefix).append("Connective number '").append(type).append("' is not between 0 and ").
                    append(Quantifier.size() - 1).append("\n");
            return null;}
        int start = quantifier.firstLiteralIndex;
        boolean erraneous = false;
        for(int i = start; i < clause.length; ++i) {
            int predicate = Math.abs(clause[i]);
            if(predicate == 0 || predicate > predicates) {
                errors.append(errorPrefix).append("Literal ").append(clause[i]).append(": predicate ").
                        append("is not within the boundaries [1,").append(predicates).append("]\n");
                erraneous = true;}}
        return erraneous ? null : clause;}

    /** finds the clause with the given identifier
     *
     * @param id an identifier for a clause
     * @return null or the clause with this identifier.
     */
    public int[] findClause(int id) {
        for(int[] clause: disjunctions) if(clause[0] == id) return clause;
        for(int[] clause: atleasts) if(clause[0] == id) return clause;
        for(int[] clause: atmosts) if(clause[0] == id) return clause;
        for(int[] clause: exactlys) if(clause[0] == id) return clause;
        for(int[] clause: intervals) if(clause[0] == id) return clause;
        for(int[] clause: equivalences) if(clause[0] == id) return clause;
        for(int[] clause: conjunctions) if(clause[0] == id) return clause;
        return null;}

    IntArrayList simplifyClause(int[] inputClause) {
        IntArrayList clause = new IntArrayList(inputClause.length);
        clause.add(inputClause[0]); // id
        clause.add(inputClause[1]); // quantifier;
        clause.add(0); clause.add(0); // min,max
        Quantifier quantifier = Quantifier.getQuantifier(inputClause[1]);
        switch(quantifier) {
            case OR:      return simplifyOr(inputClause,clause);
            case AND:     for(int i = 2; i < inputClause.length; ++i) addTrueLiteral(inputClause[i],inputClause); return null;
            case EQUIV:      return simplifyEquiv(inputClause,clause);
            case ATLEAST:    return simplifyAtleast(inputClause,clause);
            case ATMOST:     break;
            case EXACTLY:    break;
            case INTERVAL:   break;}
        return null;}

    IntArrayList simplifyOr(int[] inputClause, IntArrayList clause) {
        clause.set(2,1); // min
        int size = 0;
        for(int i = 2; i < inputClause.length; ++i) {
            int literal1 = inputClause[i];
            boolean multiple = false;
            for(int j = 2; j < i; ++j) {
                int literal2 = inputClause[j];
                if(literal2 == -literal1) {return null;} // tautology
                if(literal2 == literal1)  {multiple = true; break;}}
            if(multiple) continue;
            ++size;
            clause.add(literal1); clause.add(1);}
        if(size == 1) {addTrueLiteral(clause.getInt(4),inputClause);  return null;}
        clause.set(3,size);
        simplifiedClauses.add(clause);
        return clause;}


    IntArrayList simplifyAtleast(int[] inputClause, IntArrayList clause) {
        int length = inputClause.length;
        int[] inputClauseCopy= Arrays.copyOf(inputClause,length);
        int size = 0;
        int complementaryLiterals = 0;
        int expandedSize = 0;
        for(int i = 3; i < length; ++i) {
            int literal1 = inputClauseCopy[i];
            if(literal1 == 0) continue;
            int multiplicity = 1;
            for(int j = i+1; j < length; ++j) {
                int literal2 = inputClauseCopy[j];
                if(literal2 == literal1)  {++multiplicity; inputClauseCopy[j] = 0; continue;}
                if(literal2 == -literal1) {--multiplicity; ++complementaryLiterals; inputClauseCopy[j] = 0;}}
            if(multiplicity <= 0) continue;
            expandedSize += multiplicity;
            ++size;
            clause.add(literal1); clause.add(multiplicity);}
        int min = inputClauseCopy[2] - complementaryLiterals;  int max = expandedSize;
        if(min <= 0) {return null;} // tautology
        if(min > expandedSize)  {contradictions.add(new UnsatClause(problemId,"InputClauses",inputClause)); return null;}
        if(min == expandedSize) {
            for(int i = 4; i < clause.size(); i += 2) addTrueLiteral(clause.getInt(i),inputClause);
            return null;}
        for(int i = 5; i < clause.size(); i += 2) {
            int multiplicity = clause.getInt(i);
            if(multiplicity > min) {
                expandedSize -= multiplicity - min;
                clause.set(i,min);}}
        if(size == 1) {addTrueLiteral(clause.getInt(4),inputClause); return null;}
        clause.set(2,min); clause.set(3,max); // max
        divideByGCD(clause);
        if(min == 1) clause.set(1,Quantifier.OR.ordinal());
        simplifiedClauses.add(clause);
        return clause;}

    IntArrayList simplifyEquiv(int[] inputClause, IntArrayList clause) {
        int length = inputClause.length;
        int sign = 1;
        IntArrayList eqClass = null;
        for(int i = 2; i < length; ++i) {
            int literal1 = inputClause[i];
            for(IntArrayList equivalenceClass : equivalenceClasses) {
                for(int j = 4; j < equivalenceClass.size(); ++j) {
                    int literal2 = equivalenceClass.getInt(j);
                    if(literal2 == literal1) {
                        sign = 1;
                        eqClass = equivalenceClass;
                        break;}
                    if(literal2 == -literal1) {
                        sign = 1;
                        eqClass = equivalenceClass;
                        break;}}
                if(eqClass != null) break;}
            if(eqClass != null) break;}
        boolean newClass = eqClass == null;
        if(!newClass) clause = eqClass;
        for(int i = 2; i < length; ++i) {
            int literal1 = sign*inputClause[i];
            boolean found = false;
            for(int j = 4; j < clause.size(); ++j) {
                int literal2 = clause.getInt(j);
                if(literal1 == literal2) {found = true; break;}
                if(literal1 == -literal2) {
                    contradictions.add(new UnsatClause(problemId,"InputClauses", inputClause));
                    return null;}}
            if(!found) clause.add(literal1);}
        if(newClass && clause.size() > 5) equivalenceClasses.add(clause);
        return (clause.size() > 5) ? clause : null;}

    /** to be used by divideByGCD. */
    private final IntArrayList numbers = new IntArrayList();

    /** divides the limit and the multiplicities by their greatest common divisor.
     *
     * @return true if the clause is changed.
     */
    void divideByGCD(IntArrayList clause) {
        numbers.clear(); numbers.add(clause.get(2)); // min
        boolean stop = false;
        for(int i = 5; i < clause.size(); i += 2) {
            int multiplicity = clause.getInt(i);
            if(multiplicity == 1) {return;}
            numbers.add(multiplicity);}
        int gcd = Utilities.gcd(numbers);
        if(gcd == 1) return;
        int min = clause.get(2)/gcd;
        clause.set(2,min);
        int max = 0;
        for(int i = 5; i < clause.size(); i += 2) {
            int multiplicity = clause.getInt(i)/gcd;
            clause.set(i,multiplicity);
            max += multiplicity;}
        clause.set(3,max);}

    void addTrueLiteral(int trueLiteral, int[] inputClause) {
        for(int literal : trueLiterals) {
            if(literal == trueLiteral) return;
            if(literal == -trueLiteral) {
                contradictions.add(new UnsatClause(problemId,"InputClauses", inputClause));
                return;}}
        trueLiterals.add(trueLiteral);}

    public String toString(IntArrayList clause) {
        StringBuilder st = new StringBuilder();
        st.append(clause.getInt(0)).append(": ");
        Quantifier quantifier = Quantifier.getQuantifier(clause.getInt(1));
        switch(quantifier) {
            case OR:         toStringOr(clause, quantifier, st);    break;
            case EQUIV:      toStringEquiv(clause,quantifier,st); break;
            case ATLEAST:    toStringAtleast(clause,quantifier,st);     break;
            case ATMOST:     break;
            case EXACTLY:    break;
            case INTERVAL:   break;}
        return st.toString();}

    void toStringOr(IntArrayList clause, Quantifier quantifier, StringBuilder st) {
        String separator = quantifier.separator;
        st.append(Symboltable.toString(clause.getInt(4),symboltable));
        for(int i = 6; i < clause.size(); i += 2) {
            st.append(separator).append(Symboltable.toString(clause.getInt(i),symboltable));}}

    void toStringAtleast(IntArrayList clause, Quantifier quantifier, StringBuilder st) {
        String separator = quantifier.separator;
        st.append(quantifier.abbreviation).append(clause.getInt(2)).append(" ");
        st.append(Symboltable.toString(clause.getInt(4),symboltable));
        if(clause.getInt(5) > 1) st.append("^").append(clause.getInt(5));
        for(int i = 6; i < clause.size(); i += 2) {
            st.append(separator).append(Symboltable.toString(clause.getInt(i),symboltable));
            int multiplicity = clause.getInt(i+1);
            if(multiplicity > 1) st.append("^").append(multiplicity);
        }}

    void toStringEquiv(IntArrayList clause, Quantifier quantifier, StringBuilder st) {
        String separator = quantifier.separator;
        st.append(Symboltable.toString(clause.getInt(4),symboltable));
        for(int i = 5; i < clause.size(); ++i)
            st.append(separator).append(Symboltable.toString(clause.getInt(i),symboltable));}


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
        for(int[] clause : exactlys)  {if(quantifiedIsFalse(clause,model))  {falseClauses.add(clause);}}
        for(int[] clause : intervals)    {if(intervalIsFalse(clause,model))    {falseClauses.add(clause);}}
        return falseClauses;}


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
        int start = clause[1] == Quantifier.INTERVAL.ordinal() ? 4 : 3;
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
        assert Quantifier.getQuantifier(clause[1]) == Quantifier.OR;
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
        assert Quantifier.getQuantifier(clause[1]) == Quantifier.AND;
        for(int i = 2; i < clause.length; ++i) {if(!model.isTrue(clause[i])) {return true;}}
        return false;}

    /** checks if an equivalence is false in a model.
     *
     * @param clause an equivalence clause.
     * @param model a model.
     * @return true if not either all literals are true or all literals are false in the model.
     */
    public static boolean equivalenceIsFalse(int[] clause, Model model) {
        assert Quantifier.getQuantifier(clause[1]) == Quantifier.EQUIV;
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
        Quantifier quantifier = Quantifier.getQuantifier(clause[1]);
        assert quantifier != null;
        switch(quantifier) {
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
        assert Quantifier.getQuantifier(clause[1]) == Quantifier.INTERVAL;
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
        statistics.exactlys     = exactlys.size();
        return statistics;}

    /** removes all clauses.
     * This is mainly for testing purposes.*/
    public void clear() {
        disjunctions.clear();
        conjunctions.clear();
        equivalences.clear();
        atleasts.clear();
        atmosts.clear();
        exactlys.clear();
        intervals.clear();
        maxClauseLength = 0;}

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
        Quantifier quantifier = Quantifier.getQuantifier(connectiveNumber);
        assert quantifier != null;
        if(size == 0) {size = Integer.toString(clause[0]).length();}
        st.append(String.format("%"+size+"s",clause[0])).append(": ");
        int start = 2;
        if(Quantifier.isQuantifier(connectiveNumber) && quantifier != Quantifier.INTERVAL) {
            start = 3;
            st.append(quantifier.abbreviation).append(" ");
            st.append(clause[2]).append(" ");}
        else{if(quantifier == Quantifier.INTERVAL) {
            start = 4;
            st.append(clause[2]).append("-").append(clause[3]).append(": ");}}
        String separator = quantifier.separator;
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
        return toString(symboltable,false);}

    /** generates a string representation of the clauses.
     *
     * @param symboltable null or a symboltable.
     * @return  a string representation of the clauses.
     */
    public String toString(Symboltable symboltable, boolean infoOnly) {
        StringBuilder st = new StringBuilder();
        if(problemId != null) st.append("Problem ").append(problemId).append("\n");
        if(info != null) st.append(info).append("\n");
        int size = (disjunctions.size() + conjunctions.size()  +equivalences.size()) +
                atleasts.size() + atmosts.size() + exactlys.size() +intervals.size();
        if(infoOnly) {st.append("Clauses " + size); return st.toString();}
        size = (""+size).length()+2;
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
        if(!exactlys.isEmpty()) {
            st.append("Exactlys:\n");
            for(int[] clause : exactlys)  {st.append(toString(size,clause,symboltable)).append("\n");}}
        if(!intervals.isEmpty()) {
            st.append("Intervals:\n");
            for(int[] clause : intervals)    {st.append(toString(size,clause,symboltable)).append("\n");}}
        return st.toString();}

    /** prints the clauses as a cnf-file into the jobdirectory.
     *
     * @param jobdirectory           where the files of the job are to be printed.
     * @param cnfFile               'symboltable' or 'numbers'.
     * @throws FileNotFoundException if the file cannot be opnened.
     */
    public void makeCNFFile(Path jobdirectory, String cnfFile) throws FileNotFoundException {
        Symboltable symboltable1 = cnfFile.equals("symboltable") ? symboltable : null;
        PrintStream stream = new PrintStream(Paths.get(jobdirectory.toString(), problemId +".cnf").toFile());
        stream.println("#problem " + problemId);
        for(String info : info.split("\\n")) {
            stream.println("#"+info);}
        stream.println("p " + predicates + " c " + (nextId-1));
        if(!disjunctions.isEmpty()) {
            for(int[] clause : disjunctions) {printCNF(clause,stream,symboltable1);}}
        if(!conjunctions.isEmpty()) {
            for(int[] clause : conjunctions) {printCNF(clause,stream,symboltable1);}}
        if(!equivalences.isEmpty()) {
            for(int[] clause : equivalences) {printCNF(clause,stream,symboltable1);}}
        if(!atleasts.isEmpty()) {
            for(int[] clause : atleasts)     {printCNF(clause,stream,symboltable1);}}
        if(!atmosts.isEmpty()) {
            for(int[] clause : atmosts)      {printCNF(clause,stream,symboltable1);}}
        if(!exactlys.isEmpty()) {
            for(int[] clause : exactlys)     {printCNF(clause,stream,symboltable1);}}
        if(!intervals.isEmpty()) {
            for(int[] clause : intervals)    {printCNF(clause,stream,symboltable1);}}
        stream.close();
    }

    /** prints a single clause in cnf-form to the stream.
     *
     * @param clause      a clause to be printed.
     * @param stream      a PrintStream.
     * @param symboltable null or a symboltable.
     */
    private void printCNF(int[] clause, PrintStream stream, Symboltable symboltable) {
        Quantifier quantifier = Quantifier.getQuantifier(clause[1]);
        int start = quantifier.firstLiteralIndex;
        int end = clause.length;
        String abbreviation = quantifier.abbreviation;
        if(quantifier == Quantifier.OR || quantifier.isInterval()) abbreviation = "";
        stream.print(abbreviation);
        if(quantifier.isQuantifier() && !quantifier.isInterval()) stream.print(" "+clause[2]);
        if(quantifier.isInterval()) stream.print("["+clause[2]+","+clause[3]+"]");
        for(int i = start; i < end; ++i) {
            stream.print(" "+ Symboltable.toString(clause[i],symboltable));}
        stream.println(" 0");}

}
