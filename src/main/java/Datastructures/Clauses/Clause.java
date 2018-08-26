package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Model;

import java.util.Arrays;
import java.util.function.Consumer;
import java.util.stream.Stream;

/**
 * Created by Ohlbach on 25.08.2018.
 * A clause cosists of a number and an array of CLiterals.
 */
public class Clause {
    public int number;
    private int maxSize;
    private int actualSize;
    private CLiteral[] cliterals;

    public Clause(int number, int maxSize) {
        this.number = number;
        this.maxSize = maxSize;
        cliterals = new CLiteral[maxSize];}

    public int size() {return actualSize;}

    public int size(Model model) {
        int counter = 0;
        for(int position = 0; position < actualSize; ++position) {
            if(!model.isFalse(cliterals[position].literal)) {++counter;}}
        return counter;}

    public boolean isEmpty(Model model) {
        for(int position = 0; position < actualSize; ++position) {
            if(!model.isFalse(cliterals[position].literal)) {return false;}}
        return true;}

    public boolean isTrue(Model model) {
        for(int position = 0; position < actualSize; ++position) {
            if(model.isTrue(cliterals[position].literal)) {return true;}}
        return false;}

    public int addLiteral(CLiteral cliteral) {
        assert actualSize <= maxSize;
        int newliteral = cliteral.literal;
        for(int position = 0; position < actualSize; ++position) {
            int oldliteral = cliterals[position].literal;
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
        --actualSize;
        literal.removeClause();}

    public void apply(Consumer<CLiteral> operator) {
        for(int position = 0; position < actualSize; ++position) {operator.accept(cliterals[position]);}}

    public void apply( Model model,Consumer<CLiteral> operator) {
        for(int position = 0; position < actualSize; ++position) {
            CLiteral lit = cliterals[position];
            if(!model.isFalse(lit.literal)) {
                operator.accept(cliterals[position]);}}}


    public Stream<CLiteral> toStream() {
        return Arrays.stream(cliterals,0,actualSize);}

    public Stream<CLiteral> toStream(Model model) {
        return Arrays.stream(cliterals,0,actualSize).filter(lit->{return !model.isFalse(lit.literal);});}


    public String toString() {
        StringBuffer st = new StringBuffer();
        st.append(Integer.toString(number)).append(": ");
        for(int position = 0; position < actualSize; ++position) {
            st.append(cliterals[position].toString()).append(",");}
        return st.toString();}

    public String toString(Model model) {
        StringBuffer st = new StringBuffer();
        st.append(Integer.toString(number)).append(": ");
        for(int position = 0; position < actualSize; ++position) {
            CLiteral lit = cliterals[position];
            if(!model.isFalse(lit.literal)) {st.append(cliterals[position].toString()).append(",");}}
        return st.toString();}


}
