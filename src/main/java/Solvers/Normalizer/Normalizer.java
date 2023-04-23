package Solvers.Normalizer;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Result;
import Datastructures.Results.UnsatString;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import Management.ProblemSupervisor;
import Solvers.Simplifier.UnsatClause;
import Solvers.Solver;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.HashMap;

/** The Normalizer transforms the InputClauses into Interval-Normalform and removes obvious redundancies.
 *  <br>
 *  A clause is an IntArrayList with the following entries:<br>
 *  0: identifier<br>
 *  1: quantifier ordinal <br>
 *  2: min <br>
 *  3: max <br>
 *  4: extendedSize<br>
 *  5... literal1,multiplicity1,...<br>
 * <br>
 * - True clauses are removed (e.g. tautologies)<br>
 * - complementary literals are removed from clauses <br>
 * - and-clauses and other derived true literals are put into a model<br>
 * - equivalences are joined into equivalence classes<br>
 * <br>
 * The following contradictions are found:<br>
 * - complementary true literals, <br>
 * - equivalence classes with complementary literals,<br>
 * - clauses which don't fit their interval boundaries<br>
 * <br>
 * - All occurrences of true literals are removed from the clauses.<br>
 * - Equivalent literals re replaced by their representatives.<br>
 * <br>
 * If the input clauses are not redundant, there will be no changes in the clause set.
 */
public class Normalizer extends Solver {

    /** returns the clause's identifier.
     *
     * @param clause a clause as IntArrayList.
     * @return the clause's identifier.
     */
    public static int getIdentifier(IntArrayList clause) {
        return clause.getInt(0);}

    /** returns the clause's quantifier.
     *
     * @param clause a clause as IntArrayList.
     * @return the clause's quantifier.
     */
    public static Quantifier getQuantifier(IntArrayList clause) {
        return Quantifier.getQuantifier(clause.getInt(1));}

    /** sets the clause's quantifier.
     *
     * @param quantifier a quantifier
     * @param clause a clause as IntArrayList.
     */
    public static void setQuantifier(IntArrayList clause, Quantifier quantifier) {
        clause.set(1,quantifier.ordinal());}

    /** sets the clause's ordinal.
     *
     * @param ordinal the quantifier's ordinal
     * @param clause a clause as IntArrayList.
     */
    public static void setQuantifier(IntArrayList clause, int ordinal) {
        clause.set(1,ordinal);}

    /** returns the clause's min-limit.
     *
     * @param clause a clause as IntArrayList.
     * @return the clause's min-limit.
     */
    public static int getMin(IntArrayList clause) {
        return clause.getInt(2);}

    /** sets the clause's min-limit.
     *
     * @param clause a clause as IntArrayList.
     */
    public static void setMin(IntArrayList clause, int min) {
        clause.set(2,Math.max(0,min));}

    /** returns the clause's max-limit.
     *
     * @param clause a clause as IntArrayList.
     * @return the clause's max-limit.
     */
    public static int getMax(IntArrayList clause) {
        return clause.getInt(3);}

    /** sets the clause's max-limit.
     *
     * @param clause a clause as IntArrayList.
     * @param max    the clause's max-limit.
     */
    public static void setMax(IntArrayList clause, int max) {
        clause.set(3,max);}

    /** returns the clause's extendedSize
     *
     * @param clause a clause as IntArrayList.
     * @return the clause's extendedSize.
     */
    public static int getExtendedSize(IntArrayList clause) {
        return clause.getInt(4);}

    /** sets the clause's extendedSize.
     *
     * @param clause a clause as IntArrayList.
     * @param extendedSize the clause's extendedSize.
     */
    public static void setExtendedSize(IntArrayList clause, int extendedSize) {
        clause.set(4,extendedSize);}


    /** the start index for the literals section in a clause.*/
    public static int literalsStart = 5;

    /** the global model. */
    public Model model;

    /** the list of transformed clauses. */
    public ArrayList<IntArrayList> clauses = new ArrayList<>();

    /** the list of transformed equivalence classes. */
    public ArrayList<IntArrayList> equivalences = new ArrayList<>();

    /** constructs a Normalizer and initiates various variables.
     *
     * @param problemSupervisor the problem supervisor.
     */
    public Normalizer(ProblemSupervisor problemSupervisor) {
        super(1,null);
        solverParameters = new HashMap<>();
        solverParameters.put("name","Normalizer");
        initialize(Thread.currentThread(),problemSupervisor);
        model = new Model(predicates);}

    public Normalizer(int predicates) {
        super(1,null);
        problemId = "Test";
        solverParameters = new HashMap<>();
        solverParameters.put("name","Normalizer");
        this.predicates = predicates;
        model = new Model(predicates);
       }

    @Override
    public Result solveProblem(ProblemSupervisor problemSupervisor) {
        try{
            for(int[] inputClause : inputClauses.conjunctions) normalizeConjunction(inputClause);
            for(int[] inputClause : inputClauses.equivalences) normalizeEquivalence(inputClause);
            if(equivalences.size() > 1) joinEquivalences();
            for(int[] inputClause : inputClauses.disjunctions) normalizeDisjunction(inputClause);
        }
        catch(Result result) {return result;}
        return null;}

    /** puts all conjuncts into the model.
     *
     * @param inputClause   a conjunction.
     * @throws Unsatisfiable if there are complementary 'true' literals.
     */
    void normalizeConjunction(int[] inputClause) throws Unsatisfiable {
        for(int i = Quantifier.AND.firstLiteralIndex; i < inputClause.length; ++i) {
            model.add(myThread, inputClause[i]);}}

    /** normalizes a disjunction and adds it to 'clauses'.
     * - multiple literals are removed.<br>
     * - tautologies are ignored.<br>
     * - unit clauses are added to the model.
     *
     * @param inputClause the input clause.
     * @return the normalized clause.
     * @throws Unsatisfiable if the model discovers a contradiction.
     */
    IntArrayList normalizeDisjunction(int[] inputClause) throws Unsatisfiable{
        int length = inputClause.length;
        IntArrayList clause = new IntArrayList(length+2);
        clause.add(inputClause[0]); // id
        clause.add(inputClause[1]); // quantifier ordinal
        clause.add(1); // min
        clause.add(0); // max
        clause.add(0); // extendedSize;
        int start = Quantifier.OR.firstLiteralIndex;
        int size = 0;
        for(int i = start; i < inputClause.length; ++i) {
            int literal1 = inputClause[i];
            boolean multiple = false;
            for(int j = start; j < i; ++j) {
                int literal2 = inputClause[j];
                if(literal2 == -literal1) {return null;} // tautology
                if(literal2 == literal1)  {multiple = true; break;}}
            if(multiple) continue;
            ++size;
            clause.add(literal1); clause.add(1);}
        if(size == 1) {model.add(myThread, clause.getInt(literalsStart));  return null;}
        setMax(clause,size);
        setExtendedSize(clause,size);
        clauses.add(clause);
        return clause;}

    /** transforms the inputClause into an IntArrayList of literals and puts it into the equivalences list.
     * - Double literals are removed.<br>
     * - If the remains is a singleton, it is ignored.
     * - Complementary literals cause and Unsatisfiable to be thrown.
     *
     * @param inputClause     an inputClause.
     * @throws Unsatisfiable if the clause contains complementary literals.
     */
    void normalizeEquivalence(int[] inputClause) throws Unsatisfiable {
        IntArrayList clause = new IntArrayList(inputClause.length-2);
        for(int i = Quantifier.EQUIV.firstLiteralIndex; i < inputClause.length; ++i) {
            int literal = inputClause[0];
            if(clause.contains(literal)) continue;
            if(clause.contains(-literal)) throw new UnsatClause(problemId,solverId,inputClause);
            clause.add(literal);}
        if(clause.size() > 1) equivalences.add(clause);}

    /** joins overlapping equivalence classes into one of the class.
     *
     * @throws Unsatisfiable if the overlapping classes contain complementary literals.
     */
    void joinEquivalences() throws Unsatisfiable {
        for(int i = 0; i < equivalences.size(); ++i) {
            IntArrayList eq1 = equivalences.get(i);
            for(int j = i+1; j < equivalences.size(); ++j) {
                IntArrayList eq2 = equivalences.get(j);
                int sign = overlap(eq1,eq2);
                if(sign == 0) continue;
                for(int literal : eq2) {
                    if(eq1.contains(literal)) continue;
                    if(eq1.contains(-literal))
                        throw new UnsatString(problemId,solverId,
                                "The equivalence classes contain complementary literals: " +
                                        Symboltable.toString(literal,symboltable));
                    eq1.add(literal);}
                equivalences.remove(j);}}}

    /** checks if the two lists overlap.
     *
     * @param list1 an IntArrayList
     * @param list2 an IntArrayList
     * @return +1 if the lists contain the same literal, -1 if they contain complementary literals, 0 otherwise.
     */
    private static int overlap(IntArrayList list1, IntArrayList list2) {
        for(int literal1 : list1) {
            for(int literal2 : list2) {
                if(literal1 ==  literal2) return +1;
                if(literal1 == -literal2) return -1;}}
        return 0;}

    /** lists the model, the equivalence classes and the clauses as a string.
     *
     * @return the model, the equivalence classes and the clauses as a string.
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        if(!model.isEmpty()) {st.append("Model:\n  ").append(model.toString(symboltable));}
        if(!equivalences.isEmpty()) {
            st.append("Equivalences:\n");
            toStringEquiv(st,"  ");}
        if(!clauses.isEmpty()) {
            st.append("Clauses:\n");
            toString(clauses.get(0),"  ",st);
            for(int i = 1; i < clauses.size(); ++i) {
                st.append("\n");
                toString(clauses.get(i),"  ",st);}}
        return st.toString();}

    /** lists the clauses as a string.
     *
     * @return the clauses as a string.
     */
    public String toStringClauses() {
        if(clauses.isEmpty()) return "";
        StringBuilder st = new StringBuilder();
        toString(clauses.get(0),"",st);
        for(int i = 1; i < clauses.size(); ++i) {
            st.append("\n");
            toString(clauses.get(i),"",st);}
        return st.toString();}

    /** turns the clause into a string.
     *
     * @param clause a clause
     * @return the clause as a string.
     */
    public String toString(IntArrayList clause) {
        StringBuilder st = new StringBuilder();
        toString(clause,"",st);
        return st.toString();}

    /** appends the clause at the StringBuilder
     *
     * @param clause a clause.
     * @param prefix a prefix for the clause.
     * @param st a StringBuilder.
     */
    private void toString(IntArrayList clause, String prefix, StringBuilder st) {
        st.append(prefix).append(getIdentifier(clause)).append(": ");
        Quantifier quantifier = getQuantifier(clause);
        switch(quantifier) {
            case OR:         toStringOr(clause, quantifier, st);                  break;
            case ATLEAST:    toStringLimit(clause,quantifier,getMin(clause), st); break;
            case ATMOST:     toStringLimit(clause,quantifier,getMax(clause), st); break;
            case EXACTLY:    toStringLimit(clause,quantifier,getMin(clause), st); break;
            case INTERVAL:   toStringInterval(clause,quantifier,st);              break;}}

    /** appends the or-clause at the StringBuilder
     *
     * @param clause a clause.
     * @param quantifier the or-quantifier.
     * @param st a StringBuilder.
     */
    private void toStringOr(IntArrayList clause, Quantifier quantifier, StringBuilder st) {
        String separator = quantifier.separator;
        st.append(Symboltable.toString(clause.get(literalsStart),symboltable));
        for(int i = literalsStart+2; i < clause.size(); i += 2) {
            st.append(separator).append(Symboltable.toString(clause.getInt(i),symboltable));}}

    /** appends the atleast-, atmost-, and exactly-clause at the StringBuilder
     *
     * @param clause     a clause.
     * @param quantifier the quantifier.
     * @param limit      the min- or max-limit.
     * @param st         a StringBuilder.
     */
    private void toStringLimit(IntArrayList clause, Quantifier quantifier, int limit, StringBuilder st) {
        String separator = quantifier.separator;
        st.append(quantifier.abbreviation).append(limit).append(" ");
        st.append(Symboltable.toString(clause.get(literalsStart),symboltable));
        if(clause.getInt(literalsStart+1) > 1) st.append("^").append(clause.getInt(literalsStart+1));
        for(int i = literalsStart+2; i < clause.size(); i += 2) {
            st.append(separator).append(Symboltable.toString(clause.getInt(i),symboltable));
            int multiplicity = clause.getInt(i+1);
            if(multiplicity > 1) st.append("^").append(multiplicity);}}

    /** appends the interval-clause at the StringBuilder
     *
     * @param clause     a clause.
     * @param quantifier the quantifier.
     * @param st         a StringBuilder.
     */
    private void toStringInterval(IntArrayList clause, Quantifier quantifier, StringBuilder st) {
        String separator = quantifier.separator;
        st.append("[").append(getMin(clause)).append(",").append(getMax(clause)).append("] ");
        st.append(Symboltable.toString(clause.get(literalsStart),symboltable));
        if(clause.getInt(literalsStart+1) > 1) st.append("^").append(clause.getInt(literalsStart+1));
        for(int i = literalsStart+2; i < clause.size(); i += 2) {
            st.append(separator).append(Symboltable.toString(clause.getInt(i),symboltable));
            int multiplicity = clause.getInt(i+1);
            if(multiplicity > 1) st.append("^").append(multiplicity);
        }}


    /** lists all the equivalence classes as a string.
     *
     * @param st a StringBuilder for appending the equivalence classes.
     * @param prefix a prefix for the clauses.
     */
    public void toStringEquiv(StringBuilder st, String prefix) {
        if(equivalences.isEmpty()) return;
        st.append(prefix);
        toStringEquiv(equivalences.get(0),st);
        for(int i = 1; i < equivalences.size(); ++i) {
            st.append("\n").append(prefix);
            toStringEquiv(equivalences.get(i),st);}}

    /** appends a single equivalence class at the StringBuilder.
     *
     * @param clause an equivalence class.
     * @param st a StringBuilder.
     */
    private void toStringEquiv(IntArrayList clause, StringBuilder st) {
        String separator = Quantifier.EQUIV.separator;
        st.append(Symboltable.toString(clause.getInt(0),symboltable));
        for(int i = 1; i < clause.size(); ++i)
            st.append(separator).append(Symboltable.toString(clause.getInt(i),symboltable));}


    @Override
    public Statistic getStatistics() {
        return null;
    }
}
