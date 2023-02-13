package Solvers;

import Datastructures.Clauses.Connective;
import Datastructures.Clauses.InputClauses;
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
     * @return       null (all literals true) or the unchanged clause or the purified shortened clause.
     * @throws Unsatisfiable if inserting literals into the model causes a contradiction.
     */
    protected static int[] purifyAtleast(int[] clause, Model model) throws Unsatisfiable {
        boolean original = true;
        int length = clause.length;
        int quantifier = clause[2];
        int zeros = 0;

        // first of all we eliminate contradictory pairs of literals and reduce the quantifier.
        for(int i = 3; i < length; ++i) {
            int literal1 = clause[i];
            for(int j = i+1; j < length; ++j) {
                int literal2 = clause[j];
                if(literal2 == 0) continue;
                if(literal1 == -literal2) {
                    if(original) {
                        original = false;
                        clause = Arrays.copyOf(clause,length);} // the original clause remains unchanged.
                    zeros += 2;
                    if(--quantifier == 0) return null;
                    clause[i] = 0;
                    clause[j] = 0;
                    break;}}}

        // if quantifier == number of surviving literals then all literals must be true.
        if(length-zeros == quantifier + 3){
            for(int i = 3; i < length; ++i) {
                if(clause[i] != 0) model.add(clause[i],new InfInputClause(clause[0]));}
            return null;}

        // now we have to reduce multiple occurrences of literals. At most 'quantifier' many are allowed.
        for(int i = 3; i < length; ++i) {
            int literal1 = clause[i];
            if(literal1 == 0) continue;
            int counter = 1;
            for(int j = i+1; j < length; ++j) {
                if(literal1 == clause[j]) ++counter;}
            if(counter > quantifier) {
                if(original) {
                    original = false;
                    clause = Arrays.copyOf(clause,length);}
                for(int j = i; j < length; ++j) {
                    if(literal1 == clause[j]) {
                        clause[j] = 0;
                        ++zeros;
                        if(--counter == quantifier) break;}}}}

        boolean isOr = (quantifier == 1); // it is actually a disjunction
        if(original && !isOr) return clause; // no change necessary

        int newLength = length-zeros;
        if(isOr) --newLength;

        // now we remove the zeros
        int[] purifiedClause = new int[newLength];
        purifiedClause[0] = clause[0]; // identifier
        if(isOr) {purifiedClause[1] = Connective.OR.ordinal(); }
        else {
            purifiedClause[1] = clause[1]; // original clause type
            purifiedClause[2] = quantifier;}

        int j = isOr ? 1 :  2 ;
        for(int i = 3; i < length; ++i) {
            if(clause[i] == 0) continue;
            purifiedClause[++j] = clause[i];}
        return purifiedClause;}

    }
