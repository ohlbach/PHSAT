package Solvers.Simplifier;

import Datastructures.Symboltable;

public class Literals {

    int predicates;

    Literal[] positiveLiterals;

    Literal[] negativeLiterals;

    public Literals(int predicates) {
        this.predicates = predicates;
        positiveLiterals = new Literal[predicates+1];
        negativeLiterals = new Literal[predicates+1];
    }
    public void addLiteral(Literal literalData) {
        int literal = literalData.literal;
        int predicate = Math.abs(literal);
        Literal[] literals = (literal > 0) ? positiveLiterals : negativeLiterals;
        Literal firstLiteral = literals[predicate];
        literals[predicate] = literalData;
        if(firstLiteral != null) {
            literalData.nextLiteral = firstLiteral;
            firstLiteral.previousLiteral = literalData;}}

    public void removeLiteral(Literal literalData) {
        int literal = literalData.literal;
        int predicate = Math.abs(literal);
        Literal[] literals = (literal > 0) ? positiveLiterals : negativeLiterals;
        Literal previousLiteral = literalData.previousLiteral;
        Literal nextLiteral     = literalData.nextLiteral;
        if(previousLiteral == null) literals[predicate] = nextLiteral;
        else previousLiteral.nextLiteral = nextLiteral;
        if(nextLiteral != null) nextLiteral.previousLiteral = previousLiteral;
        literalData.previousLiteral = null;
        literalData.nextLiteral = null;}

    public int size(int literal) {
        int predicate = Math.abs(literal);
        Literal[] literals = (literal > 0) ? positiveLiterals : negativeLiterals;
        int size = 0;
        Literal literalData = literals[predicate];
        while(literalData != null) {
            ++size;
            literalData = literalData.nextLiteral;}
        return size;}

    public Literal getFirstLiteral(int literal) {
        return (literal > 0) ? positiveLiterals[literal] : negativeLiterals[-literal];}

    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("Positive Literals:\n");
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            int size = size(predicate);
            if(size != 0) st.append(Symboltable.toString(predicate, symboltable)).append(":").append(size).append(",");}
        st.append("\nNegative Literals:\n");
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            int size = size(-predicate);
            if(size != 0) st.append(Symboltable.toString(-predicate, symboltable)).append(":").append(size).append(",");}
        return st.toString();}


}
