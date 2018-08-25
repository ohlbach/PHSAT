package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;

import java.util.Arrays;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

/**
 * Created by Ohlbach on 25.08.2018.
 */
public class Clause {
    public int number;
    private int maxSize;
    private int actualSize;
    private CLiteral[] cliterals;

    public Clause(int maxSize) {
        cliterals = new CLiteral[maxSize];}

    public int getSize() {return actualSize;}

    public int addLiteral(CLiteral cliteral) {
        int newliteral = cliteral.getLiteral();
        for(int position = 0; position < actualSize; ++position) {
            int oldliteral = cliteral.getLiteral();
            if(newliteral == oldliteral) {return 1;}     // double literal
            if(newliteral == -oldliteral) {return -1;}}  // tautology
        cliteral.setClause(this,actualSize);
        cliterals[actualSize++] = cliteral;
        return 0;}

    public void removeLiteral(CLiteral literal) {
        int position = literal.getPosition();
        assert position < actualSize;
        for(int pos = position; pos < actualSize-1; ++pos) {
            CLiteral nextliteral = cliterals[pos+1];
            nextliteral.setClause(this,pos);
            cliterals[pos] = nextliteral;}
        literal.removeClause();}

    public void apply(UnaryOperator<CLiteral> operator) {
        for(int position = 0; position < actualSize; ++position) {operator.apply(cliterals[position]);}}

    public Stream<CLiteral> toStream() {
        return Arrays.stream(cliterals,0,actualSize);}

    public String toString() {
        StringBuffer st = new StringBuffer();
        st.append(Integer.toString(number)).append(": ");
        for(int position = 0; position < actualSize; ++position) {
            st.append(cliterals[position].toString()).append(",");}
        return st.toString();}
}
