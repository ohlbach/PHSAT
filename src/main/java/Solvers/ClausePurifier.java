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
            inputClauses.purifiedDisjunctions = new ArrayList<int[]>();
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
        int[] newClause = eliminateContradictoryPairs(clause,model);
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
    protected static int[] eliminateContradictoryPairs(int[] clause, Model model) throws Unsatisfiable {
        int[] originalClause = clause;
        boolean original = true;
        boolean isInterval = clause[1] == Connective.INTERVAL.ordinal();
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
                    disjunction[1] = Connective.OR.ordinal();
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
                    atmost[1] = Connective.ATMOST.ordinal();
                    atmost[2] = max;
                    int j = 2;
                    for(int i = 4; i < length; ++i) {
                        int literal = clause[i];
                        if(literal != 0) atmost[++j] = literal;}
                    return atmost;}

                if(min == max) { // [n,n] ... is the same as exactly n
                    int[] exactly = new int[nonZeros+3];
                    exactly[0] = clause[0];
                    exactly[1] = Connective.EXACTLY.ordinal();
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
     *  If original = true then literals are deleted in a copy of the clause.
     *
     * @param original         true if it is the original clause.
     * @param maxMultiplicity  the maximum allowed occurrence of the same literal
     * @param clause           a quantified clause
     * @return                 either the original clause, if nothing has changed, or a copy if original = true and literals have been zeroed.
     */
    protected static int[] reduceMultiplicities(boolean original, int maxMultiplicity, int[] clause) {
        int start = 3;
        if(clause[1] == Connective.OR.ordinal()) start = 2;
        if(clause[1] == Connective.INTERVAL.ordinal()) start = 4;

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

    /** removes all zeros (0) from the clause
     *
     * @param isOr   true if the clause is to be turned into a disjunction.
     * @param clause the clause with zeros.
     * @param model  a model.
     * @return       the shortened clause without the zeros.
     * @throws       Unsatisfiable if the clause has become a unit clause (disjunction) and adding it to the model causes an unsatisfiability.
     */
    protected static int[] compactify(boolean isOr, int[] clause, Model model) throws Unsatisfiable{
        boolean isInterval = clause[1] == Connective.INTERVAL.ordinal();
        int start = isInterval ? 4 : 3;
        int newLength = clause.length-countZeros(clause);
        if(isOr) {
            newLength -= isInterval ? 2 : 1;
            if(newLength == 3) { // the clause is now a unit clause.
                for(int i = start; i < clause.length; ++i) {
                    int literal = clause[i];
                    if(literal != 0) model.add(literal,new InfInputClause(clause[0]));
                    return null;}}}

        int[] purifiedClause = new int[newLength];
        purifiedClause[0] = clause[0]; // identifier
        purifiedClause[1] = isOr ? Connective.OR.ordinal() : clause[1]; // type of clause

        int j = isOr ? 1 :  2 ;
        if(isInterval) ++j;
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
        int start = (clause[1] == Connective.INTERVAL.ordinal()) ? 4 : 3;
        int zeros = 0;
        for(int i = start; i < clause.length; ++i) {
            if(clause[i] == 0) ++zeros;}
        return zeros;}



    }
