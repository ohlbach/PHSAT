package Solvers;

import Datastructures.Clauses.Connective;
import Datastructures.Clauses.InputClauses;
import Datastructures.Results.UnsatInputClause;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Theory.Model;
import InferenceSteps.InfInputClause;

import java.util.ArrayList;
import java.util.Arrays;

public class ClausePurifier {

    private static final int cOr       = Connective.OR.ordinal();
    private static final int cAtleast  = Connective.ATLEAST.ordinal();
    private static final int cAtmost   = Connective.ATMOST.ordinal();
    private static final int cExactly  = Connective.EXACTLY.ordinal();
    private static final int cInterval = Connective.INTERVAL.ordinal();

    public static void purifyClauses(InputClauses inputClauses, Model model) throws Unsatisfiable {
        int changedDisjunctions = 0;
        if(!inputClauses.disjunctions.isEmpty()) changedDisjunctions = purifyDisjunctions(inputClauses,model);}

    protected static int purifyDisjunctions(InputClauses inputClauses, Model model) throws Unsatisfiable{
        ArrayList<int[]> purifiedClauses = new ArrayList<>();
        int changedClauses = 0;
        for(int[] clause : inputClauses.disjunctions) {
            int[] purifiedClause = purifyDisjunction(clause,model);
            if(purifiedClause == null) {++changedClauses; continue;}
            purifiedClauses.add(purifiedClause);
            if(clause != purifiedClause) ++changedClauses;}
        if(changedClauses != 0) inputClauses.purifiedDisjunctions = purifiedClauses;
        return changedClauses;}

    /** removes all redundancies from the atleast-clauses.
     * - multiple occurrences of literals are removed not to exceed the quantifier. <br>
     * - complementary literals are detected. Each complementary pair reduces the quantifier by 1.<br>
     * - atleast 0 ...  is always true. The clause is ignored.<br>
     * - atleast n l1,...,ln causes all literals to become true.<br>
     * - atleast 1 ... is turned into a disjunction <br>
     * The original clause remains always unchanged.<br>
     * The original lists in inputClauses are kept unchanged if there were no redundancies in the clauses.
     *
     * @param inputClauses   the input clauses.
     * @param model          the model.
     * @return               the number of changed clauses.
     * @throws Unsatisfiable if inserting a literal into the model causes a conflict.
     */
    protected static int purifyAtleasts(InputClauses inputClauses, Model model) throws Unsatisfiable{
        ArrayList<int[]> purifiedAtleasts = new ArrayList<>();
        ArrayList<int[]> purifiedDisjunctions = new ArrayList<>();
        int changedClauses = 0;
        for(int[] clause : inputClauses.atleasts) {
            int[] purifiedClause = purifyAtleast(clause,model);
            if(purifiedClause == null) {++changedClauses; continue;}
            if(purifiedClause == clause) {purifiedAtleasts.add(clause); continue;}
            ++changedClauses;
            if(purifiedClause[1] == Connective.OR.ordinal()) {
                 purifiedDisjunctions.add(purifiedClause);}
            else{purifiedAtleasts.add(purifiedClause);}}
        if(changedClauses == 0) return 0;
        inputClauses.purifiedAtleasts = purifiedAtleasts;
        if(purifiedDisjunctions.isEmpty()) return changedClauses;

        if(inputClauses.purifiedDisjunctions == inputClauses.disjunctions) {
            inputClauses.purifiedDisjunctions = new ArrayList<>();
            inputClauses.purifiedDisjunctions.addAll(inputClauses.disjunctions);}
        inputClauses.purifiedDisjunctions.addAll(purifiedDisjunctions);
        return changedClauses;}



    /** removes redundancies from disjunctions.
     * - multiple occurrences of literals are removed <br>
     * - complementary literals are detected. The clause can be ignored.<br>
     * - if the purified clause becomes a unit clause, the literal is put into the model.<br>
     * The original clause remains always unchanged.
     *
     * @param clause a raw input clause (disjunction)
     * @param model  the model
     * @return       null (tautology or unit clause) or the unchanged clause or the purified shortened clause.
     * @throws Unsatisfiable if the clause becomes a unit clause and inserting it into the model causes a contradiction.
     */
    protected static int[] purifyDisjunction(int[] clause, Model model) throws Unsatisfiable {
        boolean original = true;
        int length = clause.length;
        int zeros = 0;
        for(int i = 2; i < length; ++i) {
            int literal1 = clause[i];
            for(int j = i+1; j < length; ++j) {
                int literal2 = clause[j];
                if(literal2 == 0) continue;
                if(literal1 == -literal2) return null; // tautology
                if(literal1 == literal2) {
                    if(original) {
                        original = false;
                        clause = Arrays.copyOf(clause,length);} // the original clause remains unchanged.
                    ++zeros;
                    clause[j] = 0;}}}

        int newLength = length-zeros;
        if(newLength == 3) {model.add(clause[2],new InfInputClause(clause[0])); return null;}
        if(original) return clause; // no change necessary

        int[] purifiedClause = new int[newLength];
        purifiedClause[0] = clause[0]; // identifier
        purifiedClause[1] = clause[1]; // clause type
        int j = 1;
        for(int i = 2; i < length; ++i) {
            if(clause[i] == 0) continue;
            purifiedClause[++j] = clause[i];}
        return purifiedClause;}


    /** removes redundancies from the atleast clause.
     * - multiple occurrences of literals are removed not to exceed the quantifier. <br>
     * - complementary literals are detected. Each complementary pair reduces the quantifier by 1.<br>
     * - atleast 0 ...  is always true. The clause is ignored.<br>
     * - atleast n l1,...,ln causes all literals to become true.<br>
     * - atleast 1 ...  is turned into a disjunction.<br>
     * The original clause remains always unchanged.
     *
     * @param clause a raw input clause (atleast)
     * @param model  the model
     * @return       null (tautology of all literals true) or the unchanged clause or the purified shortened clause.
     * @throws Unsatisfiable if inserting literals into the model causes a contradiction.
     */
    protected static int[] purifyAtleast(int[] clause, Model model) throws Unsatisfiable {
        int quantifier = clause[2];
        if(quantifier == 0) return null;  // atleast 0 ... is always true
        int intLength = clause.length;    // length of the array
        int clauseLength = intLength - 3; // number of literals
        if(quantifier > clauseLength) throw new UnsatInputClause(clause); // atleast n l1... ln-k is unsatifiable

        // first of all we eliminate contradictory pairs of literals and reduce the quantifier.
        int[] newClause = eliminateComplementaryPairs(clause,model);
        boolean original = newClause == clause;
        clause = newClause;
        quantifier = clause[2];

        // now we have to reduce multiple occurrences of literals. At most 'quantifier' many are allowed.
        newClause = reduceMultiplicities(original,quantifier,clause);
        original &= newClause == clause;
        clause = newClause;

        boolean isOr = (quantifier == 1);    // it is actually a disjunction
        if(original && !isOr) return clause; // no change necessary

        if(isOr) return compactify(true,clause,model); // all zeros deleted, clause becomes a disjunction.

        // This deals with the following phenomenon:
        // Example: atleast 2 p,p,q. If p would be false, there would not be enough literals to satisfy atleast 2.
        //          Therefore, p must be true.
        // Example: atleast 5 p,p,q,q,r,s
        //          p and q must be true, and the result is atleast 1 r,s
        int zeros = countZeros(clause);
        for(int i = 3; i < intLength; ++i) {
            int literal = clause[i];
            if(literal == 0) continue;
            int counter = 1;
            for(int j = i+1; j < intLength; ++j) {
                if(clause[j] == literal) ++counter;}
            if(clauseLength - zeros - counter < quantifier) {
                model.add(literal,new InfInputClause(clause[0]));
                quantifier -= counter;
                zeros += counter;
                for(int j = i; j < intLength; ++j) {
                    if(clause[j] == literal) clause[j] = 0;}
                clause = reduceMultiplicities(false,quantifier,clause);
                original = false;
                if(quantifier == 0) return null;
                if(quantifier == 1) return compactify(true,clause,model);}}

        return original ? clause : compactify(isOr,clause,model);}


    /** removes redundancies from the atmost clause.
     * - complementary literals are detected. Each complementary pair reduces the quantifier by 1.<br>
     * - multiple occurrences of literals which are more than the quantifier are false. <br>
     * - atmost 0 ...  cause all literals to become false<br>
     * - atmost n l1,...,ln is always true.<br>
     * - atmost n-1 l1 ... ln  is turned into a disjunction -l1,...,-ln.<br>
     * The original clause remains always unchanged.
     *
     * @param clause a raw input clause (atleast)
     * @param model  the model
     * @return       null (tautology of all literals true) or the unchanged clause or the purified shortened clause.
     * @throws Unsatisfiable if inserting literals into the model causes a contradiction.
     */
    protected static int[] purifyAtmost(int[] clause, Model model) throws Unsatisfiable {
        int intLength = clause.length;    // length of the array
        int clauseLength = intLength - 3; // number of literals
        int quantifier = clause[2];
        if(quantifier >= clauseLength) return null;  // atmost n l1,...,ln ... is always true
        if(quantifier == 0) {                        // atmost 0 ... means all literals are false.
            for(int i = 3; i < intLength; ++i)
                model.add(-clause[i],new InfInputClause(clause[0]));
            return null;}
        if(quantifier == clauseLength - 1) {
            int[] disjunction = new int[clauseLength-1];
            disjunction[0] = clause[0];
            disjunction[1] = cOr;
            for(int j = 3; j < intLength; ++j) disjunction[j-1] = -clause[j];
            return purifyDisjunction(disjunction,model);}

        // first of all we eliminate contradictory pairs of literals and reduce the quantifier.
        int[] newClause = eliminateComplementaryPairs(clause,model);
        if(newClause == null) return null;
        boolean original = newClause == clause;
        clause = newClause;
        if(clause[1] == cOr) return compactify(true,reduceMultiplicities(false, 1, clause),model);
        quantifier = clause[2];

        //// all literals whose multiplicity exceeds quantifier must be false.
        newClause = eliminateExceedingMultiplicities(original,quantifier,4,clause,model);
        original &= newClause == clause;
        clause = newClause;

        return original ? clause : compactify(false,clause,model);}


    /** removes redundancies from the exactly clause.
     * - complementary literals are detected. Each complementary pair reduces the quantifier by 1.<br>
     * - multiple occurrences of literals which are more than the quantifier are false. <br>
     * - exctly -n ... is unsatisfiable<br>
     * - exactly 0 ...  cause all literals to become false<br>
     * - exactly n l1,...,ln all literals must be true.<br>
     * The original clause remains always unchanged.
     *
     * @param clause a raw input clause (atleast)
     * @param model  the model
     * @return       null (tautology of all literals true) or the unchanged clause or the purified shortened clause.
     * @throws Unsatisfiable if inserting literals into the model causes a contradiction.
     */
    protected static int[] purifyExactly(int[] clause, Model model) throws Unsatisfiable {
        int clauseLength  = clause.length;    // length of the array
        int literalLength = clauseLength - 3; // number of literals
        int quantifier = clause[2];
        if(quantifier > literalLength) throw new UnsatInputClause(clause);
        if(quantifier == 0) {                        // atmost 0 ... means all literals are false.
            for(int i = 3; i < clauseLength; ++i)
                model.add(-clause[i],new InfInputClause(clause[0]));
            return null;}
        if(quantifier == literalLength) {
            for(int i = 3; i < clauseLength; ++i)
                model.add(clause[i],new InfInputClause(clause[0]));
            return null;}

        // first of all we eliminate contradictory pairs of literals and reduce the quantifier.
        int[] newClause = eliminateComplementaryPairs(clause,model);
        if(newClause == null) return null;
        boolean original = newClause == clause;
        clause = newClause;
        quantifier = clause[2];

        /// all literals whose multiplicity exceeds quantifier must be false.
        newClause = eliminateExceedingMultiplicities(original,quantifier,4,clause,model);
        original &= newClause == clause;
        clause = newClause;

        return original ? clause : compactify(false,clause,model);}


    /** removes redundancies from the interval clause.
     * - complementary literals are detected. Each complementary pair reduces the quantifier by 1.<br>
     * - multiple occurrences of literals which are more than max are false. <br>
     * - [..,-n] ... is unsatisfiable<br>
     * - [..,0] ...  cause all literals to become false<br>
     * - [1,n] l1...ln  becomes a disjunction<br>
     * - [n,n] l1,...,ln becomes an exactly-clause
     * The original clause remains always unchanged.
     *
     * @param clause a raw input clause (atleast)
     * @param model  the model
     * @return       null (tautology of all literals true) or the unchanged clause or the purified shortened clause.
     * @throws Unsatisfiable if inserting literals into the model causes a contradiction.
     */
    protected static int[] purifyInterval(int[] clause, Model model) throws Unsatisfiable {
        int clauseLength  = clause.length;    // length of the array
        int literalLength = clauseLength - 3; // number of literals
        int min = clause[2];
        int max = clause[3];
        if(min > literalLength) throw new UnsatInputClause(clause);
        if(max == 0) {                        // atmost 0 ... means all literals are false.
            for(int i = 4; i < clauseLength; ++i)
                model.add(-clause[i],new InfInputClause(clause[0]));
            return null;}
        if(min == literalLength) {
            for(int i = 3; i < clauseLength; ++i)
                model.add(clause[i],new InfInputClause(clause[0]));
            return null;}
        if(min == 0) {
            int[] atmostClause = new int[clauseLength-1];
            atmostClause[0] = clause[0];
            atmostClause[1] = cAtmost;
            atmostClause[2] = max;
            for(int i = 4; i < clauseLength; ++i) atmostClause[i-1] = clause[i];
            return purifyAtmost(atmostClause,model);}

        // first of all we eliminate contradictory pairs of literals and reduce the quantifier.
        int[] newClause = eliminateComplementaryPairs(clause,model);
        if(newClause == null) return null;
        boolean original = newClause == clause;
        if(newClause[1] == cAtmost) return purifyAtmost(newClause,model);
        clause = newClause;
        max = clause[3];

        /// all literals whose multiplicity exceeds quantifier must be false.
        newClause = eliminateExceedingMultiplicities(original,max,4,clause,model);
        original &= newClause == clause;
        clause = newClause;

        return original ? clause : compactify(false,clause,model);}


    protected int[] transform(int[] clause, int zeros, Model model) throws Unsatisfiable{
        int length = clause.length;
        int literals, quantifier;
        int id = clause[0];
        switch(Connective.getConnective(clause[1])) {
            case OR:
                literals = length - 2 - zeros;
                if(literals <= 0) throw new UnsatInputClause(clause);
                if(literals == 1) return makeAllTrue(clause,2,1,model);
                break;
            case ATLEAST:
                literals = length - 3 - zeros;
                quantifier = clause[2];
                if(quantifier <= 0) return null;  // atleast 0 ... is always true
                if(quantifier > literals) throw new UnsatInputClause(clause); // atleast n l1... ln-k is unsatisfiable
                if(quantifier == literals) {      // atleast n l1...ln means all literals are true.
                    return makeAllTrue(clause,3,1,model);}
                if(quantifier == 1) {
                    return makeDisjunction(clause,3,literals,1);}
                break;
            case ATMOST:
                literals = length - 3 - zeros;
                quantifier = clause[2];
                if(quantifier < 0) throw new UnsatInputClause(clause); // atmost -1 ... is unsatisfiable
                if(quantifier == 0) {      // atmost 0 l1...ln means all literals are false.
                    return makeAllTrue(clause,3,-1,model);}
                if(quantifier >= literals) return null; // atmost n l1...ln is always true
                if(quantifier == literals-1) {
                    return makeDisjunction(clause,3,literals,-1);}
                break;
            case EXACTLY:
                literals = length - 3 - zeros;
                quantifier = clause[2];
                if(quantifier < 0 || quantifier > literals) throw new UnsatInputClause(clause); // exactly -1 ... is unsatisfiable
                if(quantifier == 0) {      // exactly 0 l1...ln means all literals are false.
                    return makeAllTrue(clause,3,-1,model);}
                if(quantifier == literals) {      // exactly n l1...ln means all literals are true.
                    return makeAllTrue(clause,3,1,model);}
                if(quantifier == 1 && literals == 2) { // exactly 1 p,q  means p = -q
                    int[] equiv = new int[4];
                    equiv[0] = clause[0];
                    equiv[1] = Connective.EQUIV.ordinal();
                    if(zeros == 0) {equiv[2] = clause[4]; equiv[3] = -clause[5]; return equiv;}
                    boolean first = true;
                    for(int i = 3; i < length; ++i) {
                        int literal = clause[i];
                        if(literal == 0) continue;
                        if(first) {equiv[4] = literal; first = false;}
                        else {equiv[5] = -literal; break;}}
                    return equiv;}
                break;
            case INTERVAL:
                literals = length - 4 - zeros;
                int min = clause[2];
                int max = clause[3];
                if(max < 0 || min > literals) throw new UnsatInputClause(clause);
                if(max == 0) {      // [0,0] l1...ln means all literals are false.
                    return makeAllTrue(clause,4,-1,model);}
                if(min == literals) {      // [n,n] l1...ln means all literals are true.
                    return makeAllTrue(clause,4,1,model);}
                if(min == max) {
                    if(min == 1 && literals == 2) { // [1,1] p,q  means p = -q
                        int[] equiv = new int[4];
                        equiv[0] = clause[0];
                        equiv[1] = Connective.EQUIV.ordinal();
                        if(zeros == 0) {equiv[2] = clause[5]; equiv[3] = -clause[6]; return equiv;}
                        boolean first = true;
                        for(int i = 4; i < length; ++i) {
                            int literal = clause[i];
                            if(literal == 0) continue;
                            if(first) {equiv[4] = literal; first = false;}
                            else {equiv[5] = -literal; break;}}
                        return equiv;}

                    int[] exactly = new int[literals+3];
                    exactly[0] = clause[0];
                    exactly[1] = Connective.EXACTLY.ordinal();
                    exactly[3] = min;
                    int j = 3;
                    for(int i = 4; i < length; ++i) {
                        int literal = clause[i];
                        if(literal != 0) exactly[++j] = literal;}
                    return exactly;}
        }
        return clause;}

    protected int[] makeDisjunction(int[] clause, int start, int literals, int sign) {
        int[] disjunction = new int[literals+2];
        disjunction[0] = clause[0];
        disjunction[1] = Connective.OR.ordinal();
        int j = 1;
        for(int i = start; i < clause.length; ++i) {
            int literal = clause[i];
            if(literal != 0) disjunction[++j] = sign*literal;}
        return disjunction;}


    protected int[] makeAllTrue(int[] clause, int start, int sign, Model model) throws Unsatisfiable {
        InfInputClause inference = new InfInputClause(clause[0]);
        for(int i = start; i < clause.length; ++i) {
            int literal = clause[i];
            if(literal != 0) model.add(sign*literal,inference);}
        return null;}


    /** returns the first (non-zero) literal
     *
     * @param clause a clause
     * @param start the start index for the search
     * @return the first literal (which is not 0)
     */
    protected int getFirstLiteral(int[] clause, int start) {
        for(int i = start; i < clause.length; ++i) {
            int literal = clause[i];
            if(literal != 0) return literal;}
        return 0;}

    /** derives false(literal) if its multiplicity exceeds the quentifier in atmost, exactly and interval clauses
     *
     * @param original   true if it is still the original clause
     * @param quantifier the quantifier in atleast and exactly clauses, the max in interval clauses
     * @param start      4 for interval clauses, otherwise 3
     * @param clause     the clause
     * @param model      the model
     * @return           either the original or the simplified clause
     * @throws Unsatisfiable if false(literal) causes a contradiction.
     */
    protected static int[] eliminateExceedingMultiplicities(boolean original, int quantifier,
                                                            int start, int[] clause, Model model) throws Unsatisfiable {
        int clauseLength = clause.length;
        for(int i = start; i < clauseLength; ++i) {
            int literal = clause[i];
            if(literal == 0) continue;
            int counter = 1;
            for(int j = i+1; j < clauseLength; ++j) {
                if(clause[j] == literal) ++counter;}
            if(++counter > quantifier) {
                model.add(-literal,new InfInputClause(clause[0]));
                if(original) {clause = Arrays.copyOf(clause,clauseLength); original = false;}
                for(int j = i; j < clauseLength; ++j) {
                    if(clause[j] == literal) clause[j] = 0;}}}
        return clause;}

    /** eliminates complementary pairs from the clause.
     * Each complementary pair p,-p represents a true fact. <br>
     * Therefore, each complementary pair reduces the quantifier by 1.<br>
     * Several phenomena may occur:<br>
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
     * @param clause a quantified clause.
     * @param model  a model.
     * @return null or the unchanged clause or a new clause without complementary pairs.
     * @throws Unsatisfiable if a contradiction is encountered.
     */
    protected static int[] eliminateComplementaryPairs(int[] clause, Model model) throws Unsatisfiable {
        int[] originalClause = clause;
        boolean original = true;
        boolean isInterval = clause[1] == cInterval;
        int start = isInterval ? 4 : 3;
        int length = clause.length;
        int pairs = 0;
        int nonZeros = 0;
        for(int i = start; i < length; ++i) {
            int literal1 = clause[i];
            if(literal1 == 0) continue;
            for(int j = i+1; j < length; ++j) {
                int literal2 = clause[j];
                if(literal2 == 0) continue;
                if(literal1 == -literal2) {
                    if(original) {
                        original = false;
                        clause = Arrays.copyOf(clause,length);} // the original clause remains unchanged.
                    clause[i] = 0;
                    clause[j] = 0;
                    ++pairs;
                    break;}}
            if(clause[i] != 0) ++nonZeros;}
        if(pairs == 0) return clause;

        switch(Connective.getConnective(clause[1])) {
            case ATLEAST:
                int quantifier = clause[2] - pairs;
                if(quantifier <= 0) return null;
                if(quantifier == 1) return compactify(true,clause,model);
                if(quantifier == nonZeros) {
                    for(int i = 3; i < length; ++i) {
                        int literal = clause[i];
                        if(literal != 0) model.add(literal,new InfInputClause(clause[0]));}
                    return null;}
                clause[2] = quantifier;
                return clause;

            case ATMOST:
                quantifier = clause[2] - pairs;
                if(quantifier < 0) throw new UnsatInputClause(originalClause);
                if(quantifier == 0) {
                    for(int i = 3; i < length; ++i) {
                        int literal = clause[i];
                        if(literal != 0) model.add(-literal,new InfInputClause(clause[0]));}
                    return null;}
                if(quantifier == nonZeros) return null; // atmost n l1,...,ln is always true
                if(quantifier == nonZeros - 1) {        // atmost 2 p,q,r -> atleast 1 -p,-q,-r
                    int[] disjunction = new int[nonZeros+2];
                    disjunction[0] = clause[0];
                    disjunction[1] = cOr;
                    int j = 1;
                    for(int i = 3; i < length; ++i) {
                        int literal = clause[i];
                        if(literal != 0) disjunction[++j] = -literal;}
                    return disjunction;}
                clause[2] = quantifier;
                return clause;

            case EXACTLY:
                quantifier = clause[2] - pairs;
                if(quantifier < 0) throw new UnsatInputClause(originalClause);
                if(quantifier == 0) { // all literals must be false
                    for(int i = 3; i < length; ++i) {
                        int literal = clause[i];
                        if(literal != 0) model.add(-literal,new InfInputClause(clause[0]));}
                    return null;}
                if(quantifier == nonZeros) { // all literals must be true
                    for(int i = 3; i < length; ++i) {
                        int literal = clause[i];
                        if(literal != 0) model.add(literal,new InfInputClause(clause[0]));}
                    return null;}
                return clause;

            case INTERVAL:
                int min = Math.max(0,clause[2] - pairs);
                int max = clause[3] - pairs;
                if(max < 0) throw new UnsatInputClause(originalClause);

                if(max == 0) { // all literals must be false
                    for(int i = 4; i < length; ++i) {
                        int literal = clause[i];
                        if(literal != 0) model.add(-literal,new InfInputClause(clause[0]));}
                    return null;}

                if(min == 0) { // [0,n] ... is the same as atmost n ...
                    int[] atmost = new int[nonZeros+3];
                    atmost[0] = clause[0];
                    atmost[1] = cAtmost;
                    atmost[2] = max;
                    int j = 2;
                    for(int i = 4; i < length; ++i) {
                        int literal = clause[i];
                        if(literal != 0) atmost[++j] = literal;}
                    return atmost;}

                if(min == max) { // [n,n] ... is the same as exactly n
                    int[] exactly = new int[nonZeros+3];
                    exactly[0] = clause[0];
                    exactly[1] = cExactly;
                    exactly[2] = max;
                    int j = 2;
                    for(int i = 4; i < length; ++i) {
                        int literal = clause[i];
                        if(literal != 0) exactly[++j] = literal;}
                    return exactly;}
                clause[2] = min;
                clause[3] = max;}
        return clause;}

    /** deletes literals which occur more often than maxMultiplicity allows.
     *  If original = true then literals are deleted in a copy of the clause.<br>
     *  The clause is like the original clause, but maybe with '0' literals
     *
     * @param original         true if it is the original clause.
     * @param maxMultiplicity  the maximum allowed occurrence of the same literal
     * @param clause           a quantified clause
     * @return                 either the original clause, if nothing has changed, or a copy if original = true and literals have been zeroed.
     */
    protected static int[] reduceMultiplicities(boolean original, int maxMultiplicity, int[] clause) {
        int start = 3;
        if(clause[1] == cOr) start = 2;
        if(clause[1] == cInterval) start = 4;

        int length = clause.length;
        for(int i = start; i < length; ++i) {
            int literal1 = clause[i];
            if(literal1 == 0) continue;
            int counter = 1;
            for(int j = i+1; j < length; ++j) {
                int literal2 = clause[j];
                if(literal2 == 0) continue;
                if(literal2 == literal1) {
                    if(++counter <= maxMultiplicity) continue;
                    if(original) {clause = Arrays.copyOf(clause,length); original = false;}
                    clause[j] = 0;}}}
        return clause;}

    /** removes all zeros (0) from a quantified clause.
     * If makeOr = true then the clause is turned into an or-clause (disjunction).<br>
     * If the disjunction is a unit clause then the literal is inserted into the model.
     *
     * @param makeOr true if the clause is to be turned into a disjunction.
     * @param clause the clause with zeros.
     * @param model  a model.
     * @return       null or the shortened clause without the zeros.
     * @throws       Unsatisfiable if the clause has become a unit clause (disjunction) and adding it to the model causes an unsatisfiability.
     */
    protected static int[] compactify(boolean makeOr, int[] clause, Model model) throws Unsatisfiable{
        boolean isInterval = clause[1] == cInterval;
        int start = isInterval ? 4 : 3;
        int zeros = countZeros(clause);
        if(zeros == 0 && !makeOr) return clause; // no change necessary
        int newLength = clause.length-zeros;
        if(makeOr) {
            newLength -= isInterval ? 2 : 1;
            if(newLength == 3) { // the clause is now a unit clause.
                for(int i = start; i < clause.length; ++i) {
                    int literal = clause[i];
                    if(literal != 0) {
                        model.add(literal,new InfInputClause(clause[0]));
                        return null;}}}}

        int[] purifiedClause = new int[newLength];
        purifiedClause[0] = clause[0]; // identifier
        purifiedClause[1] = makeOr ? cOr : clause[1]; // type of clause
        if(!makeOr) {
            purifiedClause[2] = clause[2];                 // quantifier or min
            if(isInterval) purifiedClause[3] = clause[3];} // max

        int j = 2;
        if(isInterval) j = 3;
        if(makeOr) j = 1;
        for(int i = start; i < clause.length; ++i) {
            if(clause[i] == 0) continue;
            purifiedClause[++j] = clause[i];}
        return purifiedClause;}



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



    }
