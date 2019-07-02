package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;
import Utilities.Positioned;
import Utilities.Sizable;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

/** A clause is just a list of CLiterals.
 *
 * Created by ohlbach on 13.09.2018.
 */
public class Clause implements Iterable<CLiteral>, Positioned, Sizable {
    /** for identifying the clause */
    public String id;
    /** the literals */
    public ArrayList<CLiteral<Clause>> cliterals;
    /** a timestamp to be used by corresponding algorithms */
    public int timestamp = 0;
    /** indicates if the clause is an input clause or not */
    public boolean input = true;
    /** indicates that the clause has been removed */
    public boolean removed = false;
    /** for sorting clauses, for example in a listPosition queue */
    public int listPosition = 0;
    /** positive, negative or mixed */
    public ClauseStructure structure;


    /** constructs a clause
     *
     * @param id   the clause problemId
     * @param size  the estimated number of literals
     */
    public Clause(String id, int size) {
        this.id = id;
        cliterals = new ArrayList<CLiteral<Clause>>(size);
        setStructure();}

    /** constructs a new clause with given literals
     *
     * @param id        the id of the new clause
     * @param cLiterals the list of CLiterals
     */
    public Clause(String id, ArrayList<CLiteral<Clause>> cLiterals) {
        this.id = id;
        for(int i = 0; i < cLiterals.size(); ++i) {
            cLiterals.get(i).setClause(this,i);}
        cliterals = cLiterals;
        setStructure();}

    /** constructs a new clause as a copy of a given one.
     *
     * @param clause   the clause to be copied
     * @param listPosition the new listPosition
     * @param input    signals if the clause is an input clause or not
     */
    public Clause(Clause clause, int listPosition, boolean input) {
        this.id = clause.id;
        cliterals = new ArrayList<>(clause.size());
        for(int i = 0; i < clause.size(); ++i) {
            CLiteral clit = clause.cliterals.get(i);
            cliterals.add(new CLiteral(clit.literal,this,i));}
        this.listPosition = listPosition;
        this.input = input;
        setStructure();}

    /** constructs a clause from a set of literals
     *
     * @param id        the new id
     * @param cliterals the literals
     * @param listPosition  its listPosition
     * @param input     signals if the clause is an input clause or not
     */
    public Clause(String id, ArrayList<CLiteral<Clause>> cliterals, int listPosition, boolean input) {
        this.id = id;
        this.listPosition = listPosition;
        this.input = input;
        this.cliterals = cliterals;
        for(int i = 0; i < cliterals.size(); ++i) {
            cliterals.get(i).setClause(this,i);}
        setStructure();}


    /** checks if the clause is positive, negative or mixed and set the corresponding value for the structure.
     */
    private void setStructure() {
        int positive = 0;
        int negative = 0;
        for(CLiteral cLiteral : cliterals) {
            if(cLiteral.literal > 0) {++positive;} else {++negative;}}
        structure = ClauseStructure.MIXED;
        if(positive == 0) {structure = ClauseStructure.NEGATIVE;}
        else {if(negative == 0) {structure = ClauseStructure.POSITIVE;}}}

    public int getPosition() {return listPosition;}

    public void setPosition(int position) {listPosition = position;}

    public void delete() {

    }


        /** return the current number of literals
         *
         * @return the current number of literals
         */
    public int size() {return cliterals.size();}

    /** checks if the clause is empty
     *
     * @return true if the clause is empty.
     */
    public boolean isEmpty() {return cliterals.isEmpty();}

    /** gets the literal at the given clausePosition
     *
     * @param position a literal clausePosition
     * @return the literal at that clausePosition.
     */
    public int getLiteral(int position) {
        assert position >= 0 && position < cliterals.size();
        return cliterals.get(position).literal;}

    /** checks if the literal is in the clause
     *
     * @param literal a literal
     * @return the literal's clausePosition in the clause, or -1
     */
    public int contains(int literal) {
        for(int i = 0; i < cliterals.size(); ++i) {
            if(cliterals.get(i).literal == literal) {return i;}}
        return -1;}

    /** adds a cliteral to the end of the clause<br>
     * double literals and tautologies are avoided.
     *
     * @param cliteral the literal to be added.
     * @return +1 if the literal is already in the clause, -1 if -literal is in the clause, otherwise 0.
     */
    public int addCLiteral(CLiteral cliteral) {
        int literal = cliteral.literal;
        if(contains(literal) >= 0) {return 1;}
        if(contains(-literal) >= 0) {return -1;}
        int position = cliterals.size();
        cliterals.add(cliteral);
        cliteral.setClause(this,position);
        setStructure();
        return 0;}

    /** adds a cliteral to the end of the clause, without checking for double literals and tautologies.
     *
     * @param cliteral the literal to be added.
     */
    public void addCLiteralDirectly(CLiteral cliteral) {
        int position = cliterals.size();
        cliterals.add(cliteral);
        cliteral.setClause(this,position);
        setStructure();}

    /** removes a cliteral from the clause.
     *
     * @param cLiteral the literal to be removed.
     */
    public void removeLiteral(CLiteral cLiteral) {
        removeLiteralAtPosition(cLiteral.clausePosition);
        setStructure();}

    /** removes a cliteral at the given clausePosition from the clause.
     *
     * @param position the clausePosition of the literal to be removed
     */
    public void removeLiteralAtPosition(int position) {
        int size = cliterals.size();
        assert position >= 0 && position < size;
        for(int pos = position; pos < size-1; ++pos) {
            CLiteral nextliteral = cliterals.get(pos+1);
            nextliteral.clausePosition = pos;
            cliterals.set(pos,nextliteral);}
        cliterals.remove(size-1);
        setStructure();}


    /** checks if the literals in this, except cLiteral also occur in clause2
     *
     * @param cLiteral a cLiteral in 'this'
     * @param clause2 any other clause
     * @return true if the literals in this, except cLiteral also occur in clause2
     */
    public boolean isSubset(CLiteral cLiteral, Clause clause2) {
        boolean found = true;
        for(CLiteral cl : cliterals) {
            if(cl != cLiteral & clause2.contains(cl.literal) < 0) {found = false; break;}}
        return found;}

    /** clones the given clause
     *
     * @return a copy of the clause.
     */
    public Clause clone() {
        int size = cliterals.size();
        Clause newClause = new Clause(id,size);
        ArrayList<CLiteral<Clause>> newCliterals = new ArrayList<>(size);
        for(CLiteral cLiteral : cliterals) {
            newCliterals.add(new CLiteral(cLiteral.literal,newClause,cLiteral.clausePosition));}
        newClause.cliterals = newCliterals;
        newClause.structure = structure;
        newClause.listPosition = listPosition;
        return newClause;}

    /** applies the consumer to all cLiterals
     *
     * @param consumer to be applied to a CLiteral
     */
    public void applyToCLiteral(Consumer<CLiteral> consumer) {
        for(CLiteral cLiteral : cliterals) {consumer.accept(cLiteral);}}

    /** applies the consumer to all literals
     *
     * @param consumer to be applied to a literal
     */
    public void applyToLiteral(Consumer<Integer> consumer) {
        for(CLiteral cLiteral : cliterals) {consumer.accept(cLiteral.literal);}}

    /** generates a string: clause-number: literals
     *
     * @return a string: clause-number: literals
     */
    public String toString() {
        return toString(id.length(),null);}

    /** generates a string: clause-number: literals
     *
     * @param symboltable for mapping numbers to names
     * @return a string: clause-number: literals
     */
    public String toString(Symboltable symboltable) {
        return toString(id.length(),symboltable);
    }

    /** generates a string: clause-number: non-false literals
     *
     * @param numberSize the length of the number-string
     * @param symboltable for mapping numbers to names
     * @return a string: clause-number: non-false literals
     */
    public String toString(int numberSize, Symboltable symboltable) {
        StringBuffer st = new StringBuffer();
        String n = String.format("%"+numberSize+"s",id);
        st.append(n).append(": ").append("(");
        int size = cliterals.size();
        for(int position = 0; position < size; ++position) {
            st.append( cliterals.get(position).toString(symboltable));
            if(position < size-1) {st.append(",");}}
        st.append(")");
        return st.toString();}

    @Override
    public Iterator iterator() {
        return cliterals.iterator();}
}
