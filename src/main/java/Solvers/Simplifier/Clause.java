package Solvers.Simplifier;

import Datastructures.Clauses.Connective;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;

import java.util.ArrayList;
import java.util.Arrays;

public class Clause {
    public int id;
    public Connective connective;
    public int quantifier;
    public int expandedSize = 0;
    public boolean isDisjunction;
    public boolean hasMultipleLiterals = false;
    public ArrayList<Literal> literals;
    public int timestamp;

    public Clause previousClause;
    public Clause nextClause;
    public InferenceStep inferenceStep;

    public Clause(int[] inputClause) {
        id = inputClause[0];
        connective = Connective.getConnective(inputClause[1]);
        assert(connective != null);
        isDisjunction = connective == Connective.OR;
        quantifier = inputClause[2];
        inferenceStep = new InfInputClause(id);
        int length = inputClause.length;
        int start = connective.firstLiteralIndex;
        literals = new ArrayList<>(length-start);
        if(isDisjunction)
            for(int i = start; i < length; ++i) {
                Literal literalData = new Literal(inputClause[i],1);
                literalData.clause = this;
                ++expandedSize;
                literals.add(literalData);}
        else {
            inputClause = Arrays.copyOf(inputClause,length);
            for(int i = start; i < length; ++i) {
                int literal1 = inputClause[i];
                if(literal1 == 0) continue;
                int multiplicity = 1;
                for(int j = i+1; j < length; ++j) {
                    if(literal1 == inputClause[j]) {
                        ++multiplicity;
                        inputClause[j] = 0;
                        hasMultipleLiterals = true;}}
                expandedSize += multiplicity;
                Literal literalData = new Literal(literal1,multiplicity);
                literalData.clause = this;
                literals.add(literalData);}}}

    /** The method applies a true literal to the clause.<br>
     * For a disjunction this means that the clause is true and can therefore be deleted.<br>
     * For a quantified clause this means that the literal can be deleted and the quantifier
     * must be reduced by the literal's multiplicity.
     * <br>
     * The resulting clause must be checked for the following phenomena: <br>
     *  - if the resulting quantifier is &lt;= 0, the clause is true and can be deleted.<br>
     *  - if the resulting quantifier is 1, the clause became a disjunction. <br>
     *  - if the quantifier is still &gt; 1, new true literals might be derived.<br>
     *  Example: atleast 4 p,q^2,r^2 and p is true<br>
     *  The clause is then: atleast 3 q^2,r^2. <br>
     *  Both q and r must now be true.
     *
     * @param literalData   the true literal to be deleted
     * @param literalIndex the literal index
     * @param model        the model
     * @return             true if the clause can be deleted.
     * @throws Unsatisfiable if a contradiction is discovered.
     */
    public boolean applyTrueLiteral(Literal literalData, Literals literalIndex, Model model) throws Unsatisfiable {
        if(isDisjunction) { // clause is true and can be removed.
            for(Literal lit : literals) literalIndex.removeLiteral(lit);
            return true;}

        quantifier -= literalData.multiplicity;
        if(quantifier <= 0) { // clause is true and can be removed.
            for(Literal lit : literals) literalIndex.removeLiteral(lit);
            return true;}

        literals.remove(literalData);
        literalIndex.removeLiteral(literalData);

        if(quantifier == 1) { // clause became a disjunction.
            isDisjunction = true;
            connective = Connective.OR;
            for(Literal lit : literals) lit.multiplicity = 1;
            expandedSize = literals.size();
            return false;} // clause is shorter, but cannot be removed.

        for(Literal lit : literals) {
            if(lit.multiplicity > quantifier) {
                expandedSize -= quantifier - lit.multiplicity;
                lit.multiplicity = quantifier;}}
        expandedSize -= literalData.multiplicity;

        return reclassify(literalData.literal, model);}

    /** The method applies a false literal to the clause.<br>
    * For a disjunction this means that the literal can be removed from the clause.<br>
     * If the clause becomes a unit clause, the surviving literal must be true and is put into the model.<br>
    * For a quantified clause this means that the literal can be deleted, but the quantifier remains the same.
    * <br>
    * The resulting clause must be checked for the following phenomena: <br>
    *  - if the resulting quantifier equals the remaining expandedSize all literals must be true.<br>
    *  - if the quantifier is &gt; 1, new true literals might be derived.<br>
    *  Example: atleast 3 p,q^2,r^2 and p is false<br>
    *  The clause is then: atleast 3 q^2,r^2. <br>
    *  Both q and r must now be true.
    *
    * @param literalData  the true literal to be deleted
    * @param literalIndex the literal index
    * @param model        the model
    * @return             true if the clause can be deleted.
    * @throws Unsatisfiable if a contradiction is discovered.
     */
    public boolean applyFalseLiteral(Literal literalData, Literals literalIndex, Model model) throws Unsatisfiable {
        literalIndex.removeLiteral(literalData);
        if(isDisjunction) {
            literals.remove(literalData);
            if(literals.size() == 1) {
                int literal = literals.get(0).literal;
                InferenceStep step = model.getInferenceStep(-literalData.literal);
                if(step != null) step = new InfFalseLiteralDeletion(literal,step);
                model.add(literals.get(0).literal, step);
                return true;}
            return false;}
        literals.remove(literalData);
        literalIndex.removeLiteral(literalData);
        return reclassify(-literalData.literal, model);}

    protected boolean reclassify(int literal,Model model) throws Unsatisfiable{
        for(Literal literalData: literals) {
            if(expandedSize-literalData.multiplicity < quantifier) {
                model.add(literalData.literal,null);
            }
        }

    }

    public int size() {return literals.size();}

    public String toString() {
        return toString(null,0);}

    public String toString(Symboltable symboltable, int size) {
        StringBuilder st = new StringBuilder();
        st.append(String.format("%"+size+"s",id));
        st.append(id).append(":");
        int length = literals.size()-1;
        for(int i = 0; i < length; ++i) {
            st.append(literals.get(i).toString(symboltable)).append(connective.separator);}
        st.append(literals.get(length).toString(symboltable));
        return st.toString();}

}
