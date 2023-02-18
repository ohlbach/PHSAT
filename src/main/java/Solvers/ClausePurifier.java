package Solvers;

import Datastructures.Clauses.Connective;
import Datastructures.Clauses.InputClauses;
import Datastructures.Results.Result;
import Datastructures.Results.UnsatInputClause;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Statistics.Statistic;
import Datastructures.Theory.Model;
import InferenceSteps.InfInputClause;
import Management.ErrorReporter;
import Management.ProblemSupervisor;

import java.util.ArrayList;
import java.util.Arrays;

public class ClausePurifier extends Solver {

    private static final int cOr       = Connective.OR.ordinal();
    private static final int cEquiv    = Connective.EQUIV.ordinal();
    private static final int cAtleast  = Connective.ATLEAST.ordinal();
    private static final int cAtmost   = Connective.ATMOST.ordinal();
    private static final int cExactly  = Connective.EXACTLY.ordinal();
    private static final int cInterval = Connective.INTERVAL.ordinal();

    private InputClauses inputClauses;
    private final Model model;

    public final ClausePurifierStatistics statistics;

    public ArrayList<int[]> newEquivalences;

    private ProblemSupervisor problemSupervisor = null;

    public ClausePurifier(Model model) {
        this.model        = model;
        statistics = new ClausePurifierStatistics(this);
    }

    public ClausePurifier(ProblemSupervisor problemSupervisor, Model model) {
        this.problemSupervisor = problemSupervisor;
        this.inputClauses = problemSupervisor.inputClauses;
        this.model        = model;
        statistics = new ClausePurifierStatistics(this);
    }

    public void run() {
        try{purifyClauses();}
        catch(Unsatisfiable unsatisfiable) {
            unsatisfiable.problemId = inputClauses.problemName;
            unsatisfiable.solverClass = ClausePurifier.class;
            unsatisfiable.statistic = statistics;
            if(problemSupervisor != null)
                problemSupervisor.announceResult(unsatisfiable,"ClausePurifier");}}

    protected void purifyClauses() throws Unsatisfiable {
        ArrayList<int[]> purifiedDisjunctions = new ArrayList<>();
        int changedDisjunctions = 0;
        ArrayList<int[]>  newDisjunctions = new ArrayList<>();
        if(!inputClauses.disjunctions.isEmpty())  {
            for(int[] clause : inputClauses.disjunctions) {
                int[] purifiedClause = purifyDisjunction(clause,true);
                if(purifiedClause == clause) {purifiedDisjunctions.add(purifiedClause); continue;}
                ++changedDisjunctions; ++statistics.changedClauses;
                if(purifiedClause == null)  continue;
                purifiedDisjunctions.add(purifiedClause);}}

        ArrayList<int[]> purifiedAtleasts = new ArrayList<>();
        int changedAtleasts = 0;
        if(!inputClauses.atleasts.isEmpty()) {
            for(int[] clause : inputClauses.atleasts) {
                int[] purifiedClause = purifyClause(clause);
                if(purifiedClause    == clause) {purifiedAtleasts.add(purifiedClause); continue;}
                ++changedAtleasts; ++statistics.changedClauses;
                if(purifiedClause    == null)   continue;
                if(purifiedClause[1] == cOr)    {newDisjunctions.add(purifiedClause);  continue;}
                purifiedAtleasts.add(purifiedClause);}}

        ArrayList<int[]> purifiedAtmosts = new ArrayList<>();
        int changedAtmosts = 0;
        if(!inputClauses.atmosts.isEmpty()) {
            for(int[] clause : inputClauses.atmosts) {
                int[] purifiedClause = purifyClause(clause);
                if(purifiedClause    == clause) {purifiedAtmosts.add(purifiedClause); continue;}
                ++changedAtmosts; ++statistics.changedClauses;
                if(purifiedClause    == null)   {continue;}
                if(purifiedClause[1] == cOr)    {newDisjunctions.add(purifiedClause); continue;}
                purifiedAtmosts.add(purifiedClause);}}

        ArrayList<int[]> purifiedExactlys = new ArrayList<>();
        newEquivalences = new ArrayList<>();
        int changedExactlys = 0;
        if(!inputClauses.exacltys.isEmpty()) {
            for(int[] clause : inputClauses.exacltys) {
                int[] purifiedClause = purifyClause(clause);
                if(purifiedClause    == clause) {purifiedExactlys.add(purifiedClause); continue;}
                ++changedExactlys; ++statistics.changedClauses;
                if(purifiedClause    == null)   {continue;}
                if(purifiedClause[1] == cEquiv) {newEquivalences.add(purifiedClause);  continue;}
                purifiedExactlys.add(purifiedClause);}}

        ArrayList<int[]> purifiedIntervals = new ArrayList<>();
        ArrayList<int[]> newAtleasts       = new ArrayList<>();
        ArrayList<int[]> newAtmosts        = new ArrayList<>();
        ArrayList<int[]> newExactlys       = new ArrayList<>();
        int changedIntervals = 0;
        if(!inputClauses.intervals.isEmpty()) {
            for(int[] clause : inputClauses.intervals) {
                int[] purifiedClause = purifyClause(clause);
                if(purifiedClause    == clause) {purifiedIntervals.add(purifiedClause); continue;}
                ++changedIntervals; ++statistics.changedClauses;
                if(purifiedClause    == null)   {continue;}
                Connective connective = Connective.getConnective(purifiedClause[1]);
                assert(connective != null);
                switch(connective) {
                    case OR:      newDisjunctions.add(purifiedClause); continue;
                    case EQUIV:   newEquivalences.add(purifiedClause); continue;
                    case ATLEAST: newAtleasts.add(purifiedClause);     continue;
                    case ATMOST:  newAtmosts.add(purifiedClause);      continue;
                    case EXACTLY: newExactlys.add(purifiedClause);     continue;
                    case INTERVAL: purifiedIntervals.add(purifiedClause);
                    default:
                        ErrorReporter.reportError("ClausePurifier: interval clause turned into wrong type " + connective);}}}

        if(changedDisjunctions != 0 || !newDisjunctions.isEmpty()) {
            inputClauses.purifiedDisjunctions = purifiedDisjunctions;
            if(!newDisjunctions.isEmpty()) inputClauses.purifiedDisjunctions.addAll(newDisjunctions);}
        if(changedAtleasts != 0 || !newAtleasts.isEmpty()) {
            inputClauses.purifiedAtleasts = purifiedAtleasts;
            if(!newAtleasts.isEmpty()) inputClauses.purifiedAtleasts.addAll(newAtleasts);}
        if(changedAtmosts != 0 || !newAtmosts.isEmpty()) {
            inputClauses.purifiedAtmosts = purifiedAtmosts;
            if(!newAtmosts.isEmpty()) inputClauses.purifiedAtmosts.addAll(newAtmosts);}
        if(changedExactlys != 0|| !newExactlys.isEmpty()) {
            inputClauses.purifiedExactlys = purifiedExactlys;
            if(!newExactlys.isEmpty()) inputClauses.purifiedExactlys.addAll(newExactlys);}
        if(changedIntervals != 0) {
            inputClauses.purifiedIntervals = purifiedIntervals;}}

    /** removes redundancies from disjunctions.
     * - multiple occurrences of literals are removed <br>
     * - complementary literals are detected. The clause can be ignored.<br>
     * - if the purified clause becomes a unit clause, the literal is put into the model.<br>
     * The original clause remains always unchanged.<br>
     * All zeros (0) are removed from the literals.
     *
     * @param clause   a disjunction
     * @param original if true then the clause must not be changed.
     * @return         null (tautology or unit clause) or the unchanged clause or the purified shortened clause.
     * @throws Unsatisfiable if the clause becomes a unit clause and inserting it into the model causes a contradiction.
     */
    protected int[] purifyDisjunction(int[] clause, boolean original) throws Unsatisfiable {
        int length = clause.length;
        int nonZeroLiteral = 0;
        int zeros = 0;
        for(int i = 2; i < length; ++i) {
            int literal1 = clause[i];
            if(literal1 == 0) {++zeros; continue;}
            nonZeroLiteral = literal1;
            for(int j = i+1; j < length; ++j) {
                int literal2 = clause[j];
                if(literal1 == -literal2) return null; // tautology
                if(literal1 ==  literal2) {
                    if(original) {clause = Arrays.copyOf(clause,length); original = false;} // the original clause remains unchanged.
                    clause[j] = 0;}}}

        int newLength = length-zeros;
        if(newLength == 3) {model.add(nonZeroLiteral,new InfInputClause(clause[0])); return null;}
        return (zeros == 0) ? clause : compactify(clause,zeros);}

    /** removes all redundancies from the clause.
     *
     * @param clause a clause which is not a disjunction.
     * @return null or the original clause or a purified clause.
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    protected int[] purifyClause(int[] clause) throws Unsatisfiable {
        int[] newClause = transform(clause,0); // the quantifier may cause the type of the clause to be changed.
        if(newClause == null) return null;
        boolean original = newClause == clause;
        if(newClause[1] == cOr) return purifyDisjunction(newClause,original);
        clause = newClause;
        newClause = eliminateComplementaryPairs(clause,original);
        if(newClause == null) return null;
        if(newClause[1] == Connective.OR.ordinal()) return purifyDisjunction(newClause,original);
        original &= newClause == clause;
        clause = newClause;
        newClause = reduceMultiplicities(clause,original);
        if(newClause == null) return null;
        original &= newClause == clause;
        newClause = trueLiteralsFromMultiplicities(clause,original);
        if(newClause == null) return null;
        original &= newClause == clause;
        return original ? clause : compactify(clause,countZeros(clause));
    }

    /** eliminates complementary pairs from the clause.
     * Each complementary pair p,-p represents a true fact. <br>
     * Therefore, each complementary pair reduces the quantifier by 1.<br>
     * Several phenomena may occur:<br>
     * disjunctions:<br>
     * - or 0 -&gt; unsatisfiability (empty clause)
     * - or p -&gt; true(p)<br>
     * <br>
     * atleast clauses: <br>
     *  - atleast 0 ... is always true: return null. <br>
     *  - atleast 1 ... is turned into a disjunction (OR-clause). <br>
     *  - atleast n l1,..., ln: all literals must be true. (Example: atleast 3 p,-p,q,r -&gt; atleast 2 q,r).<br>
     *  <br>
     *  atmost clauses: <br>
     *   - atmost -n ... yields an Unsatisfiability to be thrown (Example: atmost 1 p,-p,q,-q,r -&gt; atmost -1 r).<br>
     *   - atmost 0 ... all literals must be false. <br>
     *   - atmost n-1 l_1,...,ln -> -l1 or ... or -ln (Example: atmost 3 p,-p,q,r,s -&gt; atmost 2 q,r,s -&gt; -q or -r or -s).<br>
     *   <br>
     *   exactly clause: <br>
     *   - exactly -n ... yields an Unsatisfiability to be thrown (Example: exactly 1 p,-p,q,-q,r -&gt; exactly -1 r).<br>
     *   - exactly 0 ... all literals must be false (Example: exactly 1 p,-p,q,r -&gt; exactly 0 q,r -&gt; -q,-r).<br>
     *   - exactly n l1,...,ln all literals must be true (Example: exactly 2 p,-p,q -&gt; exactly 1 q -&gt; q).<br>
     *   <br>
     *   interval clauses: <br>
     *   - [..,-n]  yields an Unsatisfiability to be thrown (Example: [0,1] p,-p,q,-q,r -&gt; [-2,-1] r).<br>
     *   - [...,0] all literals must be false (Example: [1,2] p,-p,q,-q,r,s -> [-1,0] r,s -&gt; -r,-s). <br>
     *   - [0,n] ... yields atmost n ...<br>
     *   - [n,n] ... yields exaclty n ...<br>
     *   <br>
     *
     * @param clause a clause.
     * @param zeros  the number of zeros (0) in the clause.
     * @return null or the unchanged clause or a new clause.
     * @throws Unsatisfiable if a contradiction is encountered.
     */
    protected int[] transform(int[] clause, int zeros) throws Unsatisfiable {
        int length = clause.length;
        int literals, quantifier;
        Connective connective = Connective.getConnective(clause[1]);
        assert(connective != null);
        switch(connective) {
            case OR:
                literals = length - 2 - zeros;
                if(literals <= 0) throw new UnsatInputClause(clause);
                if(literals == 1) return makeAllTrue(clause,2,1);
                break;
            case ATLEAST:
                literals = length - 3 - zeros;
                quantifier = clause[2];
                if(quantifier <= 0)        return null;  // atleast 0 ... is always true
                if(quantifier > literals)  throw new UnsatInputClause(clause);       // atleast n l1... ln-k is unsatisfiable
                if(quantifier == literals) return makeAllTrue(clause,3,1); // atleast n l1...ln means all literals are true.
                if(quantifier == 1)        return makeDisjunction(clause,3,literals,1);
                break;
            case ATMOST:
                literals = length - 3 - zeros;
                quantifier = clause[2];
                if(quantifier < 0)  throw new UnsatInputClause(clause);        // atmost -1 ... is unsatisfiable
                if(quantifier == 0) return makeAllTrue(clause,3,-1); // atmost 0 l1...ln means all literals are false.
                if(quantifier >= literals)   return null;                      // atmost n l1...ln is always true
                if(quantifier == literals-1) return makeDisjunction(clause,3,literals,-1);
                break;
            case EXACTLY:
                literals = length - 3 - zeros;
                quantifier = clause[2];
                if(quantifier < 0 || quantifier > literals) throw new UnsatInputClause(clause); // exactly -1 ... is unsatisfiable
                if(quantifier == 0)                  return makeAllTrue(clause,3,-1); // exactly 0 l1...ln means all literals are false.
                if(quantifier == literals)           return makeAllTrue(clause,3,1);  // exactly n l1...ln means all literals are true.
                if(quantifier == 1 && literals == 2) return makeEquivalence(clause,3);     // exactly 1 p,q  means p = -q
                break;
            case INTERVAL:
                literals = length - 4 - zeros;
                int min = clause[2];
                int max = clause[3];
                if(max < 0 || min > literals) throw new UnsatInputClause(clause);
                if(max == 0)        return makeAllTrue(clause,4,-1);  // [0,0] l1...ln means all literals are false.
                if(min == literals) return makeAllTrue(clause,4,1);   // [n,n] l1...ln means all literals are true.
                if(min == 1 && max >= literals) return makeDisjunction(clause,4,literals,+1);
                if(min == max) {
                    if(min == 1 && literals == 2) return makeEquivalence(clause,4); // [1,1] p,q  means p = -q

                    int[] exactly = new int[literals+3];   // [n,n] ...  is exactly n ...
                    exactly[0] = clause[0];
                    exactly[1] = cExactly;
                    exactly[2] = min;
                    int j = 2;
                    for(int i = 4; i < length; ++i) {
                        int literal = clause[i];
                        if(literal != 0) exactly[++j] = literal;}
                    return exactly;}
        }
        return clause;}

    /** eliminates complementary pairs from the clause.
     * Each complementary pair p,-p represents a true fact. <br>
     * Therefore, each complementary pair reduces the quantifier by 1.<br>
     *
     * @param clause a quantified clause.
     * @return null or the unchanged clause or a new clause without complementary pairs.
     * @throws Unsatisfiable if a contradiction is encountered.
     */
    protected int[] eliminateComplementaryPairs(int[] clause, boolean original) throws Unsatisfiable {
        boolean isInterval = clause[1] == cInterval;
        int start = isInterval ? 4 : 3;
        int length = clause.length;
        int zeros = 0;
        int pairs = 0;
        for(int i = start; i < length; ++i) {
            int literal1 = clause[i];
            if(literal1 == 0) {++zeros; continue;}
            for(int j = i+1; j < length; ++j) {
                int literal2 = clause[j];
                if(literal2 == 0) continue;
                if(literal1 == -literal2) {
                    if(original) {clause = Arrays.copyOf(clause,length); original = false;} // the original clause remains unchanged.
                    clause[i] = 0; ++zeros;
                    clause[j] = 0; ++pairs;
                    ++statistics.complementaryLiterals;
                    break;}}}
        if(pairs == 0) return clause; // no changes
        clause[2] -= pairs;
        if(isInterval) clause[3] -= pairs;
        return transform(clause,zeros);}

    /** deletes multiple occurrences of literals depending on the type of clause and the quantifier.
     * atleast clauses: literals occurring more than the quantifier allows can be deleted.<br>
     * Example:  atleast 2 p,p,p,q,q,q -&gt; atleast 2 p,p,q,q.<br>
     * <br>
     * Other quantified clauses with upper bound: <br>
     * Literals occurring more than the upper bound allows must be false.<br>
     * Example: atmost 2 p,p,p,q,r,s -> atmost 2 q,r,s and false(p)<br>
     * The quantifier is not changed by this transformation.<br>
     * <br>
     * If original = true then literals are deleted in a copy of the clause.<br>
     * The clause is like the original clause, but maybe with '0' literals.<br>
     * Changed clauses may be transformed to other clause types.<br>
     * Example: atmost 2 p,p,p,q,r,s -&gt; atmost 2 q,r,s -&gt; -q,-r,-s (disjunction)
     *
     * @param clause a quantified clause
     * @param original if true then the clause must not be changed.
     * @return       either the original clause, if nothing has changed, or a copy if original = true and literals have been zeroed.
     */
    protected int[] reduceMultiplicities(int[] clause, boolean original) throws Unsatisfiable{
        int length = clause.length;
        int start = 3; int quantifier = clause[2];
        if(clause[1] == cInterval) {start = 4; quantifier = clause[3];}
        boolean changed = false;
        int zeros = 0;
        if(clause[1] == cAtleast) {// multiplicities > quantifier can be deleted.
            for(int i = 3; i < length; ++i) {
                int literal1 = clause[i];
                if(literal1 == 0) {++zeros; continue;}
                int multiplicity = 1;
                for(int j = i+1; j < length; ++j) {
                    int literal2 = clause[j];
                    if(literal2 == 0) continue;
                    if(literal2 == literal1) {
                        if(++multiplicity <= quantifier) continue;
                        if(original) {clause = Arrays.copyOf(clause,length); original = false;}
                        changed = true;
                        clause[j] = 0;}}}}
        else {            // literals with multiplicities exceeding the upper bound must be false.
            for(int i = start; i < length; ++i) {
                int literal1 = clause[i];
                if(literal1 == 0) {++zeros; continue;}
                int multiplicity = 1;
                for(int j = i+1; j < length; ++j) {
                    int literal2 = clause[j];
                    if(literal2 == 0) continue;
                    if(literal2 == literal1) ++multiplicity;}
                if(multiplicity > quantifier) {
                    if(original) {clause = Arrays.copyOf(clause,length); original = false;}
                    changed = true;
                    ++zeros;
                    ++statistics.derivedTrueLiterals;
                    ++statistics.deletedMultiplicities;
                    model.add(-literal1,new InfInputClause(clause[0]));
                    for(int j = i; j < length; ++j) {
                        if(clause[j] == literal1) clause[j] = 0;}}}}
        return changed ? transform(clause,zeros) : clause;}


    /** Identifies true literals from multiplicities.
     * Example: atleast 2 p,p,q. If p was false, there would not be enough literals to satisfy atleast 2.<br>
     *          Therefore, p must be true.<br>
     * Example: atleast 5 p,p,q,q,r,s<br>
     *          p and q must be true, and the result is atleast 1 r,s, transformed to r or s<br>
     * The result may still contain zeros (0).
     *
     * @param clause     the clause to be investigated: quantified clause, but not an atmost clause.
     * @param original   true if it is still the original clause.
     * @return           null or the original or the simplified clause
     * @throws Unsatisfiable if the model discovers a contradiction.
     */
    protected int[] trueLiteralsFromMultiplicities(int[] clause, boolean original) throws Unsatisfiable{
        if(clause[1] == cAtmost) return clause;
        boolean isInterval = clause[1] == cInterval;
        int length = clause.length;
        int zeros = countZeros(clause);
        int start = isInterval ? 4 : 3;
        int literals = length - zeros - start;
        int min = clause[2];
        int newZeros = 0;
        for(int i = start; i < length; ++i) {
            int literal = clause[i];
            if(literal == 0) continue;
            int multiplicity = 1;
            for(int j = i+1; j < length; ++j) {if(clause[j] == literal) ++multiplicity;}
            if(literals - multiplicity < min) {
                model.add(literal,new InfInputClause(clause[0]));
                ++statistics.derivedTrueLiterals;
                min      -= multiplicity;
                zeros    += multiplicity;
                newZeros += multiplicity;
                literals -= multiplicity;
                if(original) {clause = Arrays.copyOf(clause,length); original = false;}
                for(int j = i; j < length; ++j) {if(clause[j] == literal) clause[j] = 0;}}}
        clause[2] -= newZeros;
        if(isInterval) clause[3] -= newZeros;
        if(newZeros > 0) clause = transform(clause,zeros);
        return clause;}



    /** A clause exactly 1 p,q is equivalent to p = -q.
     * The clause is turned into such an equivalence.
     *
     * @param clause an exactly-clause or an interval-clause [1,1] ... (possibly with zeros)
     * @param start  3 or 4 (for interval clauses)
     * @return the corresponding equiv clause.
     */
    protected int[] makeEquivalence(int[] clause, int start) {
        int[] equiv = new int[4];
        equiv[0] = clause[0];
        equiv[1] = cEquiv;
        boolean first = true;
        for(int i = start; i < clause.length; ++i) {
            int literal = clause[i];
            if(literal == 0) continue;
            if(first) {equiv[2] = literal; first = false;}
            else {equiv[3] = -literal; break;}}
        ++statistics.derivedEquivalences;
        return equiv;}


    /** turns the clause into a disjunction.
     *  The disjunction is purified.
     *
     * @param clause   the clause
     * @param start    4 (for intervals) 3 for other quantified clauses
     * @param literals the number of non-zero literals in the clause
     * @param sign     +1 or -1
     * @return         a new disjunction.
     * @throws Unsatisfiable should not happen.
     */
    protected int[] makeDisjunction(int[] clause, int start, int literals, int sign) throws Unsatisfiable{
        int[] disjunction = new int[literals+2];
        disjunction[0] = clause[0];
        disjunction[1] = cOr;
        int j = 1;
        for(int i = start; i < clause.length; ++i) {
            int literal = clause[i];
            if(literal != 0) disjunction[++j] = sign*literal;}
        return purifyDisjunction(disjunction,false);} // there may still be double literals


    /** makes all literals true/false.
     *
     * @param clause a clause, possibly with zeros.
     * @param start  3 or 4 (for interval-clauses).
     * @param sign   +1 or -1.
     * @return null.
     * @throws Unsatisfiable if the model discovers a contradiction.
     */
    protected int[] makeAllTrue(int[] clause, int start, int sign) throws Unsatisfiable {
        InfInputClause inference = new InfInputClause(clause[0]);
        for(int i = start; i < clause.length; ++i) {
            int literal = clause[i];
            if(literal != 0) {++statistics.derivedTrueLiterals; model.add(sign*literal,inference);}}
        return null;}


    /** removes all zeros (0) from a clause.
     *
     * @param clause the clause with zeros.
     * @param zeros the number of zeros (0) in the clause.
     * @return      a new shortened clause without the zeros.
     */
    protected int[] compactify(int[] clause, int zeros) {
        if(zeros == 0) return clause;
        int length = clause.length;
        int[] shortenedClause = new int[length-zeros];
        shortenedClause[0] = clause[0]; // identifier
        shortenedClause[1] = clause[1]; // type of clause

        int start = 0;
        Connective connective = Connective.getConnective(clause[1]);
        assert(connective != null);
        switch(connective) {
            case OR:       start = 2; break;
            case ATLEAST:
            case ATMOST:
            case EXACTLY:  start = 3; shortenedClause[2] = clause[2]; break;
            case INTERVAL: start = 4; shortenedClause[2] = clause[2]; shortenedClause[3] = clause[3];
        }
        int j = start - 1;
        for(int i = start; i < length; ++i) {
            if(clause[i] != 0) shortenedClause[++j] = clause[i];}
        return shortenedClause;}

    /** counts the number of zeros (0) in the clause
     *
     * @param clause a clause with quantifier
     * @return the number of zeros in the clause.
     */
    protected static int countZeros(int[] clause) {
        int start = (clause[1] == cInterval) ? 4 : 3;
        int zeros = 0;
        for(int i = start; i < clause.length; ++i) {
            if(clause[i] == 0) ++zeros;}
        return zeros;}


    @Override
    public Result solveProblem(InputClauses inputClauses) {
        return null;
    }

    @Override
    public void prepare() {

    }

    @Override
    public Statistic getStatistics() {
        return statistics;
    }
}
