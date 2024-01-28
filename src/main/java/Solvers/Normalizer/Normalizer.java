package Solvers.Normalizer;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Result;
import Datastructures.Results.Satisfiable;
import Datastructures.Results.UnsatString;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import Management.Monitor.MonitorLife;
import Management.ProblemSupervisor;
import Datastructures.Results.UnsatClause;
import Solvers.Solver;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Arrays;
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

    /** the Or-ordinal */
    private static final int cOr = Quantifier.OR.ordinal();

    private static final int cAtleast     = Quantifier.ATLEAST.ordinal();

    private static final int cAtmost = Quantifier.ATMOST.ordinal();

    private static final int cExactly = Quantifier.EXACTLY.ordinal();

    private static final int cInterval = Quantifier.INTERVAL.ordinal();

    /** the normalizer statistics */
    NormalizerStatistics statistics;

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
     * @param min the min-limit for the clause.
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

    /** returns the clause's extendedSize.
     *
     * @param clause a clause as IntArrayList.
     * @return the clause's extendedSize.
     */
    public static int getExpandedSize(IntArrayList clause) {
        return clause.getInt(4);}

    /** sets the clause's expandedSize.
     *
     * @param clause a clause as IntArrayList.
     * @param expandedSize the clause's expandedSize.
     */
    public static void setExpandedSize(IntArrayList clause, int expandedSize) {
        clause.set(4,expandedSize);}

    /** computes the number of literals in the clause.
     *
     * @param clause a clause.
     * @return the number of literals in the clause.
     */
    public static int size(IntArrayList clause) {
        return (clause.size()-literalsStart) / 2;}

    /** checks if the clause has multiplicities &gt; 1.
     *
     * @param clause a clause
     * @return true if the clause has multiplicities &gt; 1.
     */
    public static boolean hasMultiplicities(IntArrayList clause) {
        return getExpandedSize(clause) - size(clause) > 0;}


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
        model = problemSupervisor.model;
        statistics = new NormalizerStatistics(this);}

    /** construct a normalizer for testing purposes.
     *
     * @param predicates the number of predicates.
     * @param monitoring if true then the monitor is activated.
     */
    public Normalizer(int predicates, boolean monitoring) {
        super(1,null);
        this.monitoring = monitoring;
        if(monitoring) {
            monitorId = "Normalizer";
            monitor = new MonitorLife();}
        problemId = "Test";
        solverParameters = new HashMap<>();
        solverParameters.put("name","Normalizer");
        this.predicates = predicates;
        model = new Model(predicates);
        statistics = new NormalizerStatistics(this);
       }

    /** indicates that there new true/false literals have been derived.*/
    private boolean newTrueLiterals = false;

    /** adds a literal to the model.
     *
     * @param literal       a derived true literal.
     * @param inferenceStep null or an inference step.
     * @throws Unsatisfiable if the model finds complementary literals.
     */
    private void addTrueLiteral(int literal, InferenceStep inferenceStep) throws Unsatisfiable {
        model.add(myThread,literal,inferenceStep);
        newTrueLiterals = true;
        ++statistics.derivedTrueLiterals;}

    /** transforms the input clauses into IntArrayList form and performs various simplifications.
     * <br>
     *  All true/false literals are put into the model and removed from the clauses.<br>
     *  Equivalence classes are processed by replacing all literals by the representatives in the equivalence classes.<br>
     *  In the further processing the equivalence classes can be ignored.
     *
     * @return null or an Unsatisfiable or Satisfiable exception.
     */
    @Override
    public Result solveProblem() {
        InputClauses inputClauses = problemSupervisor.inputClauses;
        try{
            for(int[] inputClause : inputClauses.conjunctions) normalizeConjunction(inputClause);
            for(int[] inputClause : inputClauses.equivalences) normalizeEquivalence(inputClause);
            if(equivalences.size() > 1) joinEquivalences();
            for(int[] inputClause : inputClauses.disjunctions) normalizeDisjunction(inputClause);
            for(int[] inputClause : inputClauses.atleasts)     transformAndSimplify(inputClause);
            for(int[] inputClause : inputClauses.atmosts)      transformAndSimplify(inputClause);
            for(int[] inputClause : inputClauses.exactlys)     transformAndSimplify(inputClause);
            for(int[] inputClause : inputClauses.intervals)    transformAndSimplify(inputClause);
            // from now on the equivalence classes are no longer needed.
            while(newTrueLiterals) { // fixpoint iteration until no new true literals are derived.
                newTrueLiterals = false;
                for(int i = clauses.size()-1; i >= 0; --i) replaceTrueLiterals(i);}} // clauses[i] may be removed.
        catch(Result result) {return result;}
        statistics.survivedClauses = clauses.size();
        return clauses.isEmpty() ? new Satisfiable(problemId,solverId,model) : null;}

    /** puts all conjuncts into the model.
     *
     * @param inputClause   a conjunction.
     * @throws Unsatisfiable if there are complementary 'true' literals.
     */
    void normalizeConjunction(int[] inputClause) throws Unsatisfiable {
        for(int i = Quantifier.AND.firstLiteralIndex; i < inputClause.length; ++i) {
            int literal = inputClause[i];
            if(monitoring) monitor.println(monitorId, "adding literal " +
                    Symboltable.toString(literal,symboltable) + " to the model.");
            ++statistics.initialTrueLiterals;
            model.add(myThread, literal, trackReasoning ? new InfInputClause(inputClause[0]) : null);}}

    /** transforms the inputClause into an IntArrayList of literals and puts it into the equivalences list.
     * <br>
     * - Double literals are removed.<br>
     * - If the remains is a singleton, it is ignored.<br>
     * - Complementary literals cause an UnsatClause to be thrown.<br>
     * - true/false literals cause all equivalent literals to become true/false.
     *
     * @param inputClause     an inputClause.
     * @throws Unsatisfiable if the clause or the model contains complementary literals.
     */
    void normalizeEquivalence(int[] inputClause) throws Unsatisfiable {
        IntArrayList equivalence = new IntArrayList(inputClause.length-2);
        int start = Quantifier.EQUIV.firstLiteralIndex;
        for(int i = start; i < inputClause.length; ++i) {
            int literal = inputClause[i];
            if(equivalence.contains(literal)) continue;
            if(equivalence.contains(-literal)) throw new UnsatClause(problemId,solverId,inputClause);
            int status = model.status(literal);
            if(status == 0) {equivalence.add(literal); continue;}
                // all literals get a truth value.
            for(int j = start; j < inputClause.length; ++j) {
                int literal1 = inputClause[j];
                if(literal1 != literal)
                    addTrueLiteral(status*literal1,trackReasoning ? new InfInputClause(inputClause[0]) : null);}
            return;}
        if(equivalence.size() > 1) equivalences.add(equivalence);}

    /** joins overlapping equivalence classes into one of the class.
     * <br>
     * The smallest predicate will be moved to the front.<br>
     * It can be used as representative for the equivalence class.<br>
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
                equivalences.remove(j--);}}

        for(IntArrayList eqv : equivalences) {
            int minLiteral = eqv.getInt(0);
            int minPredicate = Math.abs(minLiteral);
            int firstLiteral = minLiteral;
            for(int i = 1; i < eqv.size(); ++i)
                if(minPredicate > Math.abs(eqv.getInt(i))) {
                    minLiteral = eqv.getInt(i);
                    minPredicate = Math.abs(minLiteral);}
            int sign = (minLiteral > 0) ? +1: -1;
            eqv.set(0,sign*minLiteral);
            for(int i = 1; i < eqv.size(); ++i)
                if(eqv.getInt(i) == minLiteral) eqv.set(i,sign*firstLiteral);
                else eqv.set(i, sign*eqv.getInt(i));}

        if(monitoring) {
            StringBuilder st = new StringBuilder();
            toStringEquiv(st,"    ");
            monitor.println(monitorId,"Equivalence Classes:\n"+ st);}
        statistics.equivalenceClasses = equivalences.size();
    }

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

    /** searches for the representative of the literal in one of the equivalence classes.
     *
     * @param literal a literal.
     * @return either the literal itself or the representative of the literal in the equivalence classes.
     */
    int getRepresentative(int literal) {
        for(IntArrayList equivalence : equivalences) {
            int representative = equivalence.getInt(0);
            for(int i = 1; i < equivalence.size(); ++i) {
                int lit = equivalence.getInt(i);
                if(lit ==  literal) return  representative;
                if(lit == -literal) return -representative;}}
        return literal;}

    /** generates and IntArrayList version from an InputClause, just for testing purposes.
     *
     * @param inputClause an inputClause
     * @return an IntArrayList version of the clause.
     */
    public static IntArrayList makeClause(int[] inputClause){
        Quantifier quantifier = Quantifier.getQuantifier(inputClause[1]);
        assert quantifier != null && quantifier != Quantifier.AND && quantifier != Quantifier.EQUIV;
        int length = inputClause.length;
        IntArrayList clause = new IntArrayList(4 + 2 * (inputClause.length - 3));
        clause.add(inputClause[0]); // id
        clause.add(inputClause[1]); // quantifier
        int min = 0, max = 0;
        int expandedSize = length - quantifier.firstLiteralIndex;
        switch(quantifier) {
            case OR:       min = 1; max = expandedSize; expandedSize = max; break;
            case ATLEAST:  min = inputClause[2]; max = expandedSize;        break;
            case ATMOST:   min = 0; max = inputClause[2];                   break;
            case EXACTLY:  min = inputClause[2] ; max = min;                break;
            case INTERVAL: min = inputClause[2] ; max = inputClause[3];     break;}
        clause.add(min);
        clause.add(max);
        clause.add(expandedSize);
        int[] inputClauseCopy = Arrays.copyOf(inputClause, length); // the original clause must not be changed.
        int complementaryLiterals = 0;
        expandedSize = 0;
        for (int i = quantifier.firstLiteralIndex; i < length; ++i) {
            int literal1 = inputClauseCopy[i];
            if (literal1 == 0) continue;
            int multiplicity = 1;
              for (int j = i + 1; j < length; ++j) {
                int literal2 = inputClauseCopy[j];
                if(literal2 == 0) continue;
                if (literal2 == literal1) {++multiplicity; inputClauseCopy[j] = 0; continue;}
                if (literal2 == -literal1) {--multiplicity; ++complementaryLiterals; inputClauseCopy[j] = 0; break;}}
            if(multiplicity <= 0) continue;
            expandedSize += multiplicity;
            clause.add(literal1);
            clause.add(multiplicity);}
        setMin(clause,min - complementaryLiterals);
        setMax(clause,max - complementaryLiterals);
        setExpandedSize(clause,expandedSize);
        return clause;}

    /** normalizes a disjunction and adds it to 'clauses'.
     * <br>
     * An input clause has the structure: [id,quantifier,literal1,...]<br>
     * - multiple literals are removed.<br>
     * - tautologies are ignored.<br>
     * - unit clauses are added to the model.<br>
     * - true literals make the clause true. The clause is ignored.<br>
     * - false literals are ignored.<br>
     * - literals are replaced by their representative in the equivalence classes.
     *
     * @param inputClause the input clause.
     * @return the normalized clause.
     * @throws Unsatisfiable if the model discovers a contradiction.
     */
    IntArrayList normalizeDisjunction(int[] inputClause) throws Unsatisfiable{
        int quantifier = inputClause[1];
        assert quantifier == cOr;
        int length = inputClause.length;
        IntArrayList clause = new IntArrayList(length+2);
        clause.add(inputClause[0]); // id
        clause.add(quantifier);
        clause.add(1); // min
        clause.add(0); // max
        clause.add(0); // extendedSize;
        int start = Quantifier.OR.firstLiteralIndex;
        int size = 0;
        boolean changed = false;
        for(int i = start; i < inputClause.length; ++i) {
            int literal1 = inputClause[i];
            if(model.isTrue(literal1))  {++statistics.removedClauses; return null;}  // true clause
            if(model.isFalse(literal1)) {changed = true; continue;}    // can be ignored
            boolean multiple = false;
            for(int j = start; j < i; ++j) {
                int literal2 = inputClause[j];
                if(literal2 == -literal1) {++statistics.removedClauses; return null;} // tautology
                if(literal2 == literal1)  {multiple = true; break;}}
            if(multiple) continue;
            int representative = getRepresentative(literal1);
            if(representative != literal1) changed = true;
            ++size;
            clause.add(representative); clause.add(1);}

        if(changed) {
            ++statistics.simplifiedClauses;
            if(monitoring)
                monitor.println(monitorId,"InputClause " + InputClauses.toString(inputClause) +
                        " changed to " + toString(clause));}

        if(size == 0) throw new UnsatClause(problemId,solverId,inputClause); // empty disjunction is false.
        if(size == 1) {addTrueLiteral(clause.getInt(literalsStart),trackReasoning ? new InfInputClause(inputClause[0]) : null);
                        return null;}
        setMax(clause,size);
        setExpandedSize(clause,size);
        clauses.add(clause);
        return clause;}


    /** transforms an input clause into the IntArrayList form and performs various simplifications.
     * <br>
     * Only the following clause types are transformed: atleat-, atmost-, exactly- and interval-clauses.
     *
     * @param inputClause an inputClause.
     * @return null or the transformed clause.
     * @throws Unsatisfiable if the clause becomes empty or the model discovers complementary literals.
     */
    IntArrayList transformAndSimplify(int[] inputClause) throws Unsatisfiable {
        IntArrayList clause = transformInputClause(inputClause);
        if(analyseMultiplicities(clause,inputClause) == null) return null;
        divideByGCD(clause);
        optimizeQuantifier(clause,inputClause);
        clauses.add(clause);
        return clause;}

    /** transforms the inputClause into the IntArrayList form.
     * <br>
     *  true/false literals are removed.<br>
     *  equivalent literals are replaced by their representative.
     *
     * @param inputClause an input clause
     * @return the transformed clause.
     */
    IntArrayList transformInputClause(int[] inputClause) {
        Quantifier quantifier = Quantifier.getQuantifier(inputClause[1]);
        assert quantifier != null && quantifier != Quantifier.AND && quantifier != Quantifier.EQUIV;
        int length = inputClause.length;
        IntArrayList clause = new IntArrayList(4 + 2 * (inputClause.length - 3));
        clause.add(inputClause[0]); // id
        clause.add(inputClause[1]); // quantifier
        int min = 0, max = 0;
        int expandedSize = length - quantifier.firstLiteralIndex;
        switch(quantifier) {
            case OR:       min = 1; max = expandedSize; expandedSize = max; break;
            case ATLEAST:  min = inputClause[2]; max = expandedSize;        break;
            case ATMOST:   min = 0; max = inputClause[2];                   break;
            case EXACTLY:  min = inputClause[2] ; max = min;                break;
            case INTERVAL: min = inputClause[2] ; max = inputClause[3];     break;}
        clause.add(min);
        clause.add(max);
        clause.add(expandedSize);
        int[] inputClauseCopy = Arrays.copyOf(inputClause, length); // the original clause must not be changed.
        int complementaryLiterals = 0;
        expandedSize = 0;
        boolean changed = false;
        for (int i = quantifier.firstLiteralIndex; i < length; ++i) {
            int literal1 = inputClauseCopy[i];
            if (literal1 == 0) continue;
            int representative = getRepresentative(literal1);
            if(representative != literal1) {changed = true; literal1 = representative;}
            int multiplicity = 1;
            if(model.isTrue(literal1)) {--multiplicity; --max; --min; changed = true; continue;}
            if(model.isFalse(literal1)){--multiplicity; changed = true; continue;}
            for (int j = i + 1; j < length; ++j) {
                int literal2 = inputClauseCopy[j];
                if(literal2 == 0) continue;

                literal2 = getRepresentative(literal2);
                if (literal2 == literal1) {++multiplicity; inputClauseCopy[j] = 0; continue;}
                if (literal2 == -literal1) {--multiplicity; ++complementaryLiterals; inputClauseCopy[j] = 0; break;}}
            if(multiplicity <= 0) continue;
            expandedSize += multiplicity;
            clause.add(literal1);
            clause.add(multiplicity);}
        setMin(clause,min - complementaryLiterals);
        setMax(clause,max - complementaryLiterals);
        setExpandedSize(clause,expandedSize);
        if(changed) {
            ++statistics.simplifiedClauses;
            if(monitoring) {
                monitor.println(monitorId,"Input Clause " + InputClauses.toString(inputClause) +
                " changed to " + toString(clause));}}
        return clause;}

    /** analyses the multiplicities to find true/false literals and to reduce the multiplicities.
     * <br>
     *  Examples: <br>
     *  - atmost 2 p^3 ..., p must be false<br>
     *  - atleast 4 p^2,q^2,r.<br>
     *        If p is false then the clause becomes atleast 4 q^2,r, which is no longer satisfiable.<br>
     *        Therefore, p must be true, and q as well.<br>
     *  - atleast 2 p^3...   -&gt; atleast 2 p^2 ...<br>
     *  - atmost 3 p^3,q^2 -> atmost 2 p^2,q^2 (-&gt; atmost 1 p,q)
     *
     * @param clause the clause to be reduced.
     * @param inputClause the original input clause.
     * @return null or the simplified clause.
     * @throws Unsatisfiable if the model discovers complementary literals.
     */
     IntArrayList analyseMultiplicities(IntArrayList clause, int[] inputClause) throws Unsatisfiable{
        int id = clause.getInt(0);
        int expandedSize = getExpandedSize(clause);
        int min = getMin(clause);
        int max = getMax(clause);
        if(max > expandedSize) {max = expandedSize; setMax(clause,max);}
        if(min > max) {throw new UnsatClause(problemId,solverId,inputClause);}
        if(!hasMultiplicities(clause)) return clause;
        boolean changed = true;
        boolean anyChanges = false;
        while(changed) {
            changed = false;
            for(int i = clause.size()-2; i >= literalsStart; i -= 2) {
                int multiplicity = clause.getInt(i+1);
                if(multiplicity > max) { // example: atmost 2 p^3 ... p must be false
                    int literal = -clause.getInt(i);
                    addTrueLiteral(literal, trackReasoning ? new InfInputClause(id) : null);
                    if(monitoring) monitor.println(monitorId,"Clause " + InputClauses.toString(inputClause) +
                            " -> true(" + Symboltable.toString(literal,symboltable) + ")");
                    clause.removeInt(i+1); clause.removeInt(i);
                    expandedSize -= multiplicity;
                    changed = true;
                    continue;}
                int remainings = expandedSize-multiplicity;
                if(remainings < min) { // example: atleast 4 p^2,q^2,r   p and q must be true
                    int literal = clause.getInt(i);
                    addTrueLiteral(literal, trackReasoning ? new InfInputClause(id) : null);
                    if(monitoring) monitor.println(monitorId,"Clause " + InputClauses.toString(inputClause) +
                            " -> true(" + Symboltable.toString(literal,symboltable) + ")");
                    clause.removeInt(i+1); clause.removeInt(i);
                    min -= multiplicity; max -= multiplicity;
                    expandedSize -= multiplicity;
                    changed = true;
                    continue;}
                if(min > 0 && multiplicity > min) {  // example atleast 2 p^3...   -> atleast 2 p^2 ...
                    remainings = multiplicity - min;
                    clause.set(i+1,min);
                    expandedSize -= remainings;
                    changed = true;
                    continue;}
                                // example atmost 3 p^3,q^2 -> atmost 2 p^2,q^2 (-> atmost 1 p,q)
                int negMax = expandedSize - max;            // negMax = 2, expandedSize = 5
                if(negMax > 0 && multiplicity > negMax) {                 // multiplicity = 2
                    int difference = multiplicity - negMax; // difference = 1
                    clause.set(i+1,negMax);                 // new multiplicity = 2
                    expandedSize -= difference;             // expanded size = 4
                    max = expandedSize - negMax;            // new min = 2
                    changed = true;}}

                anyChanges |= changed;}
        setMin(clause,min); setMax(clause,max);
        setExpandedSize(clause,expandedSize);

        if(anyChanges) {
            ++statistics.simplifiedClauses;
            if(monitoring) monitor.println(monitorId,"Clause " + InputClauses.toString(inputClause) +
                    " simplified to " + toString(clause));}
        return (expandedSize == 0) ? checkEmptyClause(clause,inputClause) : clause;}

    /** checks the empty clause. It can be ignored (min &lt;= 0) or is unsatisfiable (min &gt; 0).
     *
     * @param clause      an empty clause
     * @param inputClause the original inputClause
     * @return            null
     * @throws Unsatisfiable if the empty clause represents an unsatisfiability.
     */
    IntArrayList checkEmptyClause(IntArrayList clause, int[] inputClause) throws Unsatisfiable{
        if(getMin(clause) <= 0) {++statistics.removedClauses; return null;}
        throw new UnsatClause(problemId,solverId,inputClause);
    }

    /** to be used by divideByGCD. */
    private final IntArrayList numbers = new IntArrayList();


    /** divides the limits and the multiplicities by their greatest common divisor.
     *
     * @param clause the clause.
     */
    void divideByGCD(IntArrayList clause) {
        numbers.clear();
        int min = getMin(clause);
        int max = getMax(clause);
        if(min == 0) {if(max == 1) return; numbers.add(max);}
        if(max == 0) {if(min == 1) return; numbers.add(min);}
        if(min > 1 && max > 1) {numbers.add(min); numbers.add(max);}
        else return;
        for(int i = literalsStart+1; i < clause.size(); i += 2) {
            int multiplicity = clause.getInt(i);
            if(multiplicity == 1) {return;}
            numbers.add(multiplicity);}
        int gcd = Utilities.gcd(numbers);
        if(gcd == 1) return;
        ++statistics.gcdReductions;
        setMin(clause,min/gcd);
        setMax(clause,max/gcd);
        int expandedSize = 0;
        for(int i = literalsStart+1; i < clause.size(); i += 2) {
            int multiplicity = clause.getInt(i)/gcd;
            clause.set(i,multiplicity);
            expandedSize += multiplicity;}
        setExpandedSize(clause,expandedSize);
        if(monitoring) monitor.println(monitorId,"Clause " + toString(clause) + " was reduced by gcd: " + gcd);}

    /** modifies the quantifier to the most specific quantifier.
     * <br>
     * Example: atleast 1 -&gt; or<br>
     * In addition the limits are checked. Some combinations of min,max allows one to derive new facts.
     *
     * @param clause      a clause
     * @param inputClause the original input clause
     * @return            the same clause with a possibly changed quantifier.
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    IntArrayList optimizeQuantifier(IntArrayList clause, int[] inputClause) throws Unsatisfiable {
        int min = getMin(clause);
        int max = getMax(clause);
        int expandedSize = getExpandedSize(clause);
        if(min <= 0 && max >= expandedSize) {++statistics.removedClauses; return null;} // tautology
        if(max < min || min > expandedSize) throw new UnsatClause(problemId,solverId,inputClause);

        int size = (clause.size() - literalsStart)/2;
        if(min == 0) { // atmost
            if(max == expandedSize-1) { // atleast one of the literals must be false.
                for(int i = literalsStart; i < clause.size(); i += 2) {
                    clause.set(i,-clause.getInt(i));
                    clause.set(i+1,1);}
                setQuantifier(clause, Quantifier.OR);
                setMin(clause,1);
                setMax(clause,size);
                setExpandedSize(clause,size);}
            else setQuantifier(clause, Quantifier.ATMOST);
            return clause;}

        if(max == expandedSize) { // atleast
            if(min == 1) { // or
                for(int i = literalsStart+1; i < clause.size(); i += 2) {clause.set(i,1); ++size;}
                setQuantifier(clause, Quantifier.OR);
                setMax(clause,size);
                setExpandedSize(clause,size);}
            return clause;}

        if(min == max) { // exactly
            if(size == 0) throw new UnsatClause(problemId,solverId,inputClause);
            if(size == 1) {
                int multiplicity = clause.getInt(literalsStart+1);
                if(multiplicity == min) addTrueLiteral(clause.getInt(literalsStart),
                        trackReasoning ? new InfInputClause(inputClause[0]) : null);
                else throw new UnsatClause(problemId,solverId,inputClause);}
            setQuantifier(clause, Quantifier.EXACTLY);}

        return clause;}


    /** replaces true/false literals in the clause.
     *
     * @param index         the clause's index in the clauses list.
     * @throws Unsatisfiable if the clause becomes empty or the model discovers complementary literals.
     */
    void replaceTrueLiterals(int index) throws Unsatisfiable {
         IntArrayList clause = clauses.get(index);
         boolean isDisjunction = clause.getInt(1) == cOr;
         int min = getMin(clause);
         int max = getMax(clause);
         int expandedSize = getExpandedSize(clause);
         boolean changed = false;
         for(int i = clause.size()-2; i >= literalsStart; i -= 2) {
             int literal = clause.getInt(i);
             int status = model.status(literal);
             if(status != 0) {
                 if(isDisjunction && status == 1) {++statistics.removedClauses; clauses.remove(index); return;} // true clause
                 changed = true;
                 expandedSize -= clause.getInt(i+1);
             clause.removeInt(i+1); clause.removeInt(i);
             if(status == 1) {--max; --min;}}}

         if(changed) {
             int[] inputClause = problemSupervisor.inputClauses.findClause(clause.getInt(0));
             if(expandedSize == 0) {checkEmptyClause(clause, inputClause); clauses.remove(index); return;}
             setMin(clause,min); setMax(clause,max); setExpandedSize(clause,expandedSize);
             if(analyseMultiplicities(clause, inputClause) == null) {clauses.remove(index); return;}
             divideByGCD(clause);
             if(optimizeQuantifier(clause,inputClause) == null){clauses.remove(index);}}
    }

    /** turns a normalized atmost-clause into an atleast clause.
     *
     * @param atmostClause a normalized atmost-clause
     * @return the transformed atleast-clause.
     */
    public static IntArrayList atmost2Atleast(IntArrayList atmostClause) {
        assert atmostClause.getInt(1) == cAtmost;
        IntArrayList atleastClause = new IntArrayList(atmostClause.size());
        atleastClause.add(atmostClause.getInt(0)); // Id
        atleastClause.add(cAtleast);
        int expandedSize = getExpandedSize(atmostClause);
        atleastClause.add(expandedSize - getMax(atmostClause)); // min;
        atleastClause.add(expandedSize);
        atleastClause.add(expandedSize);
        for(int i = literalsStart; i < atmostClause.size(); i += 2) {
            atleastClause.add(-atmostClause.getInt(i));
            atleastClause.add(atmostClause.getInt(i+1));}
        return atleastClause;}

    /** turns a normalized atleast-clause into an atmost clause.
     *
     * @param atleastClause a normalized atmost-clause
     * @return the transformed atmost-clause.
     */
    public static IntArrayList atleast2Atmost(IntArrayList atleastClause) {
        assert atleastClause.getInt(1) == cAtleast;
        IntArrayList atmostClause = new IntArrayList(atleastClause.size());
        atmostClause.add(atleastClause.getInt(0)); // Id
        atmostClause.add(cAtmost);
        int expandedSize = getExpandedSize(atleastClause);
        atmostClause.add(0); // min;
        atmostClause.add(expandedSize - getMin(atleastClause));
        atmostClause.add(expandedSize);
        for(int i = literalsStart; i < atleastClause.size(); i += 2) {
            atmostClause.add(-atleastClause.getInt(i));
            atmostClause.add(atleastClause.getInt(i+1));}
        return atmostClause;}

    /** turns a normalized exactly-clause into two atleast clauses.
     *
     * @param exactlyClause a normalized atmost-clause
     * @return the transformed atleast-clauses.
     */
    public static IntArrayList[] exactlyToAtleast(IntArrayList exactlyClause) {
        assert (exactlyClause.getInt(1) == cExactly);
        int id = getIdentifier(exactlyClause);
        int size = exactlyClause.size();
        int expandedSize = getExpandedSize(exactlyClause);
        IntArrayList atleastClause1 = new IntArrayList(size);
        IntArrayList atleastClause2 = new IntArrayList(size);
        atleastClause1.add(id);
        atleastClause2.add(id);
        atleastClause1.add(cAtleast);
        atleastClause2.add(cAtleast);
        atleastClause1.add(getMin(exactlyClause));
        atleastClause1.add(expandedSize);
        atleastClause2.add(expandedSize-getMax(exactlyClause));
        atleastClause2.add(0);
        atleastClause1.add(expandedSize);
        atleastClause2.add(expandedSize);
        for(int i = literalsStart; i < exactlyClause.size(); i += 2) {
            atleastClause1.add(exactlyClause.getInt(i));
            atleastClause1.add(exactlyClause.getInt(i+1));
            atleastClause2.add(-exactlyClause.getInt(i));
            atleastClause2.add(exactlyClause.getInt(i+1));}
         return new IntArrayList[]{atleastClause1,atleastClause2};
    }

    /** turns a normalized exactly-clause into two atleast clauses.
     *
     * @param intervalClause a normalized atmost-clause
     * @return the transformed atleast-clauses.
     */
    public static IntArrayList[] intervalToAtleast(IntArrayList intervalClause) {
        assert (intervalClause.getInt(1) == cInterval);
        int id = getIdentifier(intervalClause);
        int size = intervalClause.size();
        int expandedSize = getExpandedSize(intervalClause);
        IntArrayList atleastClause1 = new IntArrayList(size);
        IntArrayList atleastClause2 = new IntArrayList(size);
        atleastClause1.add(id);
        atleastClause2.add(id);
        atleastClause1.add(cAtleast);
        atleastClause2.add(cAtleast);
        atleastClause1.add(getMin(intervalClause));
        atleastClause1.add(expandedSize);
        atleastClause2.add(expandedSize-getMax(intervalClause));
        atleastClause2.add(0);
        atleastClause1.add(expandedSize);
        atleastClause2.add(expandedSize);
        for(int i = literalsStart; i < intervalClause.size(); i += 2) {
            atleastClause1.add(intervalClause.getInt(i));
            atleastClause1.add(intervalClause.getInt(i+1));
            atleastClause2.add(-intervalClause.getInt(i));
            atleastClause2.add(intervalClause.getInt(i+1));}
        return new IntArrayList[]{atleastClause1,atleastClause2};
    }

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
        if(clause == null) return "";
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
        st.append(Symboltable.toString(clause.getInt(literalsStart),symboltable));
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
        st.append(Symboltable.toString(clause.getInt(literalsStart),symboltable));
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
        st.append(Symboltable.toString(clause.getInt(literalsStart),symboltable));
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


    /** returns the Normalizer statistics.
     *
     * @return the Normalizer statistics.
     */
    @Override
    public Statistic getStatistics() {
        return statistics;
    }
}
