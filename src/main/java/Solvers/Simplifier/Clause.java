package Solvers.Simplifier;

import Datastructures.Clauses.Connective;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;

import java.util.ArrayList;
import java.util.Arrays;

public class Clause {
    public int id;
    public Connective connective;
    public int quantifier;
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
                Literal literalData = new Literal(literal1,multiplicity);
                literalData.clause = this;
                literals.add(literalData);}}}

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
