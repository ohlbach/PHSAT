package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Model;
import Datastructures.Symboltable;

import java.util.Arrays;
import java.util.function.Consumer;
import java.util.stream.Stream;

/**
 * Created by Ohlbach on 25.08.2018.
 * A clause consists of a clause number and an array of CLiterals.
 */
public class Clause {
    /** for enumerating the clauses */
    public int number;
    private int maxSize;    // the maximum length of the literal array
    private int actualSize; // the current number of literals
    public CLiteral[] cliterals;   // the literals

    /** constructs a clause
     *
     * @param number   the clause number
     * @param maxSize  the maximum number of literals
     */
    public Clause(int number, int maxSize) {
        this.number = number;
        this.maxSize = maxSize;
        cliterals = new CLiteral[maxSize];}

    /** constructs a clause from a string.
     *  Just for test purposes!
     *
     * @param number the clause number
     * @param literals the literal string.
     */
    public Clause(int number, String literals) {
        this.number = number;
        String[] lits = literals.split("\\s*,\\s*");
        maxSize = lits.length;
        actualSize = maxSize;
        cliterals = new CLiteral[maxSize];
        for(int i = 0; i < maxSize; ++i) {
            cliterals[i] = new CLiteral(Integer.parseInt(lits[i]),this,i);}}

    /** return the current number of literals
     *
     * @return the current number of literals
     */
    public int size() {return actualSize;}

    /** returns the current number of non-false literals in the clause
     *
     * @param model  a model (mapping predicates to true or false)
     * @return the current number of non-false literals in the clause
     */
    public int size(Model model) {
        int counter = 0;
        for(int position = 0; position < actualSize; ++position) {
            if(!model.isFalse(cliterals[position].literal)) {++counter;}}
        return counter;}

    /** checks if there are still non-false literals left.
     *
     * @param model a model (mapping predicates to true or false)
     * @return true if there are still non-false literals left
     */
    public boolean isEmpty(Model model) {
        for(int position = 0; position < actualSize; ++position) {
            if(!model.isFalse(cliterals[position].literal)) {return false;}}
        return true;}

    /** checks if there is a true literal in the clause
     *
     * @param model a model (mapping predicates to true or false)
     * @return true if there is a true literal in the clause
     */
    public boolean isTrue(Model model) {
        for(int position = 0; position < actualSize; ++position) {
            if(model.isTrue(cliterals[position].literal)) {return true;}}
        return false;}


    /** adds a literal to the clause.
     * The clause and the literal position within the clause are updated in the literal datastructure.
     * Double literals and tautologies are checked.
     *
     * @param cliteral the literal to be added.
     * @return 1 if the literal was already in the clause, -1 if the clause becomes a tautology, 0 otherwise.
     */
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

    public void resize() {
        if(actualSize < maxSize) {
            cliterals = Arrays.copyOfRange(cliterals,0,actualSize);}}

    public CLiteral getFirstLiteral(Model model) {
        for(CLiteral lit : cliterals) {if(!model.isFalse(lit.literal)) {return lit;}}
        return null;}

    /** removes a literal from the clause.
     *
     * @param literal the literal to be removed.
     */
    public void removeLiteral(CLiteral literal) {
        int position = literal.getPosition();
        assert position < actualSize;
        assert position >= 0;
        for(int pos = position; pos < actualSize-1; ++pos) {
            CLiteral nextliteral = cliterals[pos+1];
            nextliteral.setClause(this,pos);
            cliterals[pos] = nextliteral;}
        --actualSize;
        literal.removeClause();}

    /** applies a consumer to all literals in the clause.
     *
     * @param consumer a function to be applied to a literal
     */
    public void apply(Consumer<CLiteral> consumer) {
        for(int position = 0; position < actualSize; ++position) {consumer.accept(cliterals[position]);}}

    /** applies a consumer to all non-false literals in the clause
     *
     * @param model   a model (mapping predicates to true or false)
     * @param consumer a function to be applied to a literal
     */
    public void apply( Model model,Consumer<CLiteral> consumer) {
        for(int position = 0; position < actualSize; ++position) {
            CLiteral lit = cliterals[position];
            if(!model.isFalse(lit.literal)) {
                consumer.accept(cliterals[position]);}}}


    /** generates a stream of literals
     *
     * @return a stream of literals.
     */
    public Stream<CLiteral> toStream() {
        return Arrays.stream(cliterals,0,actualSize);}

    /** generates a stream of non-false literals.
     *
     * @param model a model (mapping predicates to true or false)
     * @return a stream of non-false literals.
     */
    public Stream<CLiteral> toStream(Model model) {
        return Arrays.stream(cliterals,0,actualSize).filter(lit->{return !model.isFalse(lit.literal);});}


    /** generates a string: clause-number: literals
     *
     * @return a string: clause-number: literals
     */
    public String toString() {
        StringBuffer st = new StringBuffer();
        st.append(Integer.toString(number)).append(": ");
        for(int position = 0; position < actualSize; ++position) {
            st.append(cliterals[position].toString()).append(",");}
        return st.toString();}

    /** generates a string: clause-number: literals
     *
     * @param symboltable for mapping numbers to names
     * @return a string: clause-number: literals
     */
    public String toString(Symboltable symboltable) {
        StringBuffer st = new StringBuffer();
        st.append(Integer.toString(number)).append(": ");
        for(int position = 0; position < actualSize; ++position) {
            st.append(cliterals[position].toString(symboltable)).append(",");}
        return st.toString();}


    /** generates a string: clause-number: non-false literals
     *
     * @param model  a model (mapping predicates to true or false)
     * @return a string: clause-number: non-false literals
     */
    public String toString(Model model, int numberSize) {
        return toString(model,numberSize,null);}

    /** generates a string: clause-number: non-false literals
     *
     * @param model  a model (mapping predicates to true or false)
     * @param numberSize the legth of the number-string
     * @param symboltable for mapping numbers to names
     * @return a string: clause-number: non-false literals
     */
    public String toString(Model model, int numberSize, Symboltable symboltable) {
        StringBuffer st = new StringBuffer();
        String n = String.format("%"+numberSize+"d",number);
        st.append(n).append(": ");
        for(int position = 0; position < actualSize; ++position) {
            CLiteral lit = cliterals[position];
            if(!model.isFalse(lit.literal)) {st.append(cliterals[position].toString(symboltable)).append(",");}}
        return st.toString();}


}
