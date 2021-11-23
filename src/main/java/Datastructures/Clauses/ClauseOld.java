package Datastructures.Clauses;

import Datastructures.Literals.CLiteralOld;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import InferenceSteps.Input;
import Utilities.Positioned;
import Utilities.Sizable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.*;

import static Utilities.Utilities.sortIntArray;

/** A clause is just a list of CLiterals.
 * It may represent clauses with connective OR, AND, EQUIV, ATLEAST, ATMOST or EXACTLY
 *
 * Created by ohlbach on 13.09.2018.
 */
public class ClauseOld implements Iterable<CLiteralOld>, Positioned, Sizable {
    /** for identifying the clause */
    public int id;
    /** OR,DISJOINT, ... */
    public Connective connective;
    /** for numeric types (atleast, atmost, exactly)*/
    public int quAmount = 1;
    /** the literals */
    public ArrayList<CLiteralOld> cliterals;
    /** indicates that the clause has been removed */
    public boolean removed = false;
    /** for sorting clauses, for example in a listPosition queue */
    public int listPosition = -1;
    /** positive, negative or mixed */
    public ClauseStructure structure = null;
    /** a timestamp to be used by corresponding algorithms */
    public int timestamp = 0;
    /** some auxiliary pointer */
    public Object aux = null;
    /** the reason for deriving the clause */
    public InferenceStep inferenceStep;

    /** constructs a new clause
     *
     * @param id         its identifier
     * @param connective its connective
     */
    public ClauseOld(int id, Connective connective) {
        this.id = id;
        this.connective = connective;
        cliterals = new ArrayList<>();
        inferenceStep = new Input(id);
    }

    /** constructs a clause
     *
     * @param id   the clause problemId
     * @param connective  the clause's connective
     * @param size  the estimated number of literals
     */
    public ClauseOld(int id, Connective connective, int size) {
        this.id = id;
        this.connective = connective;
        cliterals = new ArrayList<>(size);
        inferenceStep = new Input(id);}

    /** constructs a new clause with given literals
     *
     * @param id        the id of the new clause
     * @param connective  the clause's connective
     * @param literals the list of literals
     */
    public ClauseOld(int id, Connective connective, IntArrayList literals) {
        this.id = id;
        this.connective = connective;
        cliterals = new ArrayList<>(literals.size());
        for(int i = 0; i < literals.size(); ++i) {
            cliterals.add(new CLiteralOld(literals.getInt(i),this,i));}
        inferenceStep = new Input(id);
        setStructure();}

    /** constructs a new clause with given literals
     *
     * @param id        the id of the new clause
     * @param connective  a clause's connective
     * @param quAmount a quantifier amount for the clause (if its a quantification)
     * @param literals the list of literals
     */
    public ClauseOld(int id, Connective connective, int quAmount, IntArrayList literals) {
        this.id = id;
        this.connective = connective;
        this.quAmount = quAmount;
        cliterals = new ArrayList<>(literals.size());
        for(int i = 0; i < literals.size(); ++i) {
            cliterals.add(new CLiteralOld(literals.getInt(i),this,i));}
        inferenceStep = new Input(id);
        setStructure();}

    /** constructs a new clause with given literals
     *
     * @param id        the id of the new clause
     * @param connective  the clause's connective
     * @param cLiterals the list of CLiterals
     */
    public ClauseOld(int id, Connective connective, ArrayList<CLiteralOld> cLiterals) {
        this.id = id;
        this.connective = connective;
        for(int i = 0; i < cLiterals.size(); ++i) {
            cLiterals.get(i).setClause(this,i);}
        cliterals = cLiterals;
        inferenceStep = new Input(id);
        setStructure();}

    /** constructs a new clause with given literals
     * Notice that the quAmount is set to 1 if the connective is not a quantification
     *
     * @param id          the id of the new clause
     * @param connective  the clause's connective
     * @param quAmount    for numeric quantifiers
     * @param cLiterals   the list of CLiterals
     */
    public ClauseOld(int id, Connective connective, int quAmount, ArrayList<CLiteralOld> cLiterals) {
        this.id = id;
        this.connective = connective;
        this.quAmount = connective.isQuantifier() ? quAmount : 0;
        for(int i = 0; i < cLiterals.size(); ++i) {
            cLiterals.get(i).setClause(this,i);}
        cliterals = cLiterals;
        inferenceStep = new Input(id);
        setStructure();}

    /** generates a clause from a basicClause
     *
     * @param id           the name of the clause
     * @param basicClause  a basic clause [number,connective,literal1,...]
     */
    public ClauseOld(int id, int[] basicClause) {  // deprecated because id
        this.id = id;
        connective = Connective.getType(basicClause[1]);
        if(connective == null) return;
        int start = 2;
        int shift = 2;
        if(connective.isQuantifier()) {start = 3; shift = 3; quAmount = basicClause[2];}
        int length = basicClause.length;
        cliterals = new ArrayList<>(length - shift);
        for(int i = start; i < length; ++i) {
            int literal = basicClause[i];
            cliterals.add(new CLiteralOld(literal,this,cliterals.size()));}
        inferenceStep = new Input(id);
        setStructure();}

    /** generates a clause from a basicClause
     * The new clause gets the same id as the basic clause
     *
     * @param basicClause  a basic clause [id,typenumber, [quantifier], literal1,...]
     */
    public ClauseOld(int[] basicClause) {
        id = basicClause[0];
        connective = Connective.getType(basicClause[1]);
        if(connective == null) return;
        int start = 2;
        int shift = 2;
        if(connective.isQuantifier()) {start = 3; shift = 3; quAmount = basicClause[2];}
        int length = basicClause.length;
        cliterals = new ArrayList<>(length - shift);
        for(int i = start; i < length; ++i) {
            int literal = basicClause[i];
            cliterals.add(new CLiteralOld(literal,this,cliterals.size()));}
        inferenceStep = new Input(id);
        setStructure();}

    /** creates a new clause with the given literals
     *
     * @param id          the new id
     * @param connective  the clause's type
     * @param literals    [quantifier] a list of literals
     */
    public ClauseOld(int id, Connective connective, int... literals) {
        this.id = id;
        int start = 0;
        if(connective.isQuantifier()) {quAmount = literals[0]; start = 1;}
        this.connective = connective;
        cliterals = new ArrayList<>(literals.length);
        for(int i = start; i< literals.length; ++i) {
            cliterals.add(new CLiteralOld(literals[i],this,i-start));}
        inferenceStep = new Input(id);
        setStructure();}

    /** checks if the clause is an OR-clause
     *
      * @return true if the clause is an OR-clause
     */
    public boolean clauseIsOr() {
        return connective == Connective.OR;}

    /** checks if the clause is an ATLEAST-clause
     *
     * @return true if the clause is an ATLEAST-clause
     */
    public boolean clauseIsAtleast() {
        return connective == Connective.ATLEAST;}

    /** checks if the clause is an ATMOST-clause
     *
     * @return true if the clause is an ATMOST-clause
     */
    public boolean clauseIsAtmost() {
        return connective == Connective.ATMOST;}

    /** checks if the clause is an EXACTLY-clause
     *
     * @return true if the clause is an EXACTLY-clause
     */
    public boolean clauseIsExactly() {
        return connective == Connective.EXACTLY;}


    /** creates a clone of the clause.
     * Only the literals themselves are cloned
     *
     * @param id the identifier for the new clone
     * @return the new clone
     */
    public ClauseOld clone(int id) {
        ClauseOld clause = new ClauseOld(id,this.connective);
        clause.quAmount = quAmount;
        for(CLiteralOld cLiteral : cliterals) {clause.add(cLiteral.literal);}
        return clause;}

    /** creates a clone of the clause.
     * Only the literals themselves are cloned
     *
     * @param id the identifier for the new clone
     * @param ignorePosition the position of a literal to be ignored
     * @return the new clone
     */
    public ClauseOld clone(int id, int ignorePosition) {
        ClauseOld clause = new ClauseOld(id,this.connective);
        clause.quAmount = quAmount;
        for(int i = 0; i < size(); ++i) {
            if(i != ignorePosition) {clause.add(getLiteral(i));}}
        return clause;}

    /** clones the clause except the given literal
     *
     * @param id             a new id of the clause
     * @param ignoreLiteral  a literal to be ignored
     * @return               the cloned clause
     */
    public ClauseOld cloneExcept(int id, int ignoreLiteral) {
        ClauseOld clause = new ClauseOld(id,this.connective);
        clause.quAmount = quAmount;
        for(int i = 0; i < size(); ++i) {
            int literal = getLiteral(i);
            if(literal != ignoreLiteral) {clause.add(literal);}}
        return clause;}

    /** copies the literals of the clause to an IntArrayList
     *
     * @return the literals as IntArrayList
     */
    public IntArrayList toArray() {
        IntArrayList list = new IntArrayList(cliterals.size());
        for(CLiteralOld cLiteral : cliterals) list.add(cLiteral.literal);
        return list;}

    /** checks if the clause is positive, negative or mixed and returns the corresponding value for the structure.
     */
    public ClauseStructure getStructure() {
        int positive = 0;
        int negative = 0;
        for(CLiteralOld cLiteral : cliterals) {
            if(cLiteral.literal > 0) {++positive;} else {++negative;}}
        ClauseStructure structure = ClauseStructure.MIXED;
        if(positive == 0) {structure =  ClauseStructure.NEGATIVE;}
        else {if(negative == 0) {structure =  ClauseStructure.POSITIVE;}}
        return structure;}

    /** computes and sets the structure of the clause
     */
    public void setStructure() {
        structure = getStructure();}

    /** returns the list position, or -1
     *
     * @return the list position, or -1
     */
    public int getPosition() {return listPosition;}

    /** sets the list position
     *
     * @param position a position within a list.
     */
    public void setPosition(int position) {listPosition = position;}

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

    /** checks if the clause has only positive literals
     *
     * @return true if the clause has only positive literals
     */
    public boolean isPositive() {return structure == ClauseStructure.POSITIVE;}

    /** checks if the clause has only negative literals
     *
     * @return true if the clause has only negative literals.
     */
    public boolean isNegative() {return structure == ClauseStructure.NEGATIVE;}

    /** gets the literal at the given clausePosition
     *
     * @param position a literal clausePosition
     * @return the cliteral at that clausePosition.
     */
    public CLiteralOld getCLiteral(int position) {
        assert position >= 0 && position < cliterals.size();
        return cliterals.get(position);}

    /** gets the literal at the given clausePosition
     *
     * @param position a literal clausePosition
     * @return the literal at that clausePosition.
     */
    public int getLiteral(int position) {
        assert position >= 0 && position < cliterals.size();
        return cliterals.get(position).literal;}

    /** converts the cLiterals into an array of integers
     *
     * @return the literals as integers.
     */
    public int[] getLiterals() {
        int size = cliterals.size();
        int[] literals = new int[size];
        for(int i = 0; i < size; ++i) {
            literals[i] = cliterals.get(i).literal;}
        return literals;}

    /** checks if the literal is in the clause
     *
     * @param literal a literal
     * @return +1: the literal is in the clause, -1: the negated literal is in the clause, otherwise 0
     */
    public int contains(int literal) {
        return contains(literal,cliterals.size());}

    /** checks if the literal is in the clause
     *
     * @param literal a literal
     * @param end     where the iteration over the literals stops
     * @return +1: the literal is in the clause, -1: the negated literal is in the clause, otherwise 0
     */
    public int contains(int literal, int end) {
        for(int i = 0; i < end; ++i) {
            int lit = cliterals.get(i).literal;
            if(lit ==  literal) {return +1;}
            if(lit == -literal) {return -1;}}
        return 0;}

    /** adds a cliteral to the end of the clause, without checking for double literals and tautologies.
     *
     * @param cliteral the literal to be added.
     */
    public void add(CLiteralOld cliteral) {
        int position = cliterals.size();
        cliterals.add(cliteral);
        cliteral.setClause(this,position);
        setStructure();}

    /** adds a new literal to the clause
     *
     * @param literal a literal
     */
    public void add(int literal) {
        int position = cliterals.size();
        CLiteralOld cliteral = new CLiteralOld(literal,this,position);
        cliterals.add(cliteral);
        setStructure();}

    /** removes a cliteral from the clause.
     *
     * @param cLiteral the literal to be removed.
     */
    public void remove(CLiteralOld cLiteral) {
        removeAtPosition(cLiteral.clausePosition);}

    /** removes a cliteral at the given clausePosition from the clause.
     *
     * @param position the clausePosition of the literal to be removed
     */
    public void removeAtPosition(int position) {
        int size = cliterals.size();
        assert position >= 0 && position < size;
        for(int pos = position; pos < size-1; ++pos) {
            CLiteralOld nextliteral = cliterals.get(pos+1);
            nextliteral.clausePosition = pos;
            cliterals.set(pos,nextliteral);}
        cliterals.remove(size-1);
        setStructure();}

    /** checks if the literal is in the clause (except cliteral)
     *
     * @param literal a literal
     * @param ignore  a cLiteral to be ignored.
     * @return +1: the literal is in the clause, -1: the negated literal is in the clause, otherwise 0
     */
    public int contains(int literal, CLiteralOld ignore) {
        for(CLiteralOld cLiteral : this) {
            if(cLiteral == ignore) continue;
            int lit = cLiteral.literal;
            if(lit ==  literal) {return +1;}
            if(lit == -literal) {return -1;}}
        return 0;}

    /** checks if the literals in this occur in clause2
     *
     * @param clause2 any other clause
     * @return true if the literals in this also occur in clause2
     */
    public boolean isSubset(ClauseOld clause2) {
        if(connective != clause2.connective) return false;
        for(CLiteralOld cl : cliterals) {
            if(clause2.contains(cl.literal) <= 0) {return false;}}
        return true;}

    /** checks if the clause has double literals
     *
     * @return true if the clause has double literals.
     */
    public boolean hasDoubles() {
        int size = cliterals.size();
        for(int i = 0; i < size-1; ++i) {
            int literal = cliterals.get(i).literal;
            for(int j = i+1; j < size; ++j) {
                if(literal == cliterals.get(j).literal) {return true;}}}
        return false;}

    /** checks if the clause has complementary literals, e.g. p and -p
     *
     * @return true if the clause has complementary literals.
     */
    public boolean hasComplementaries() {
        int size = cliterals.size();
        for(int i = 0; i < size-1; ++i) {
            int literal = cliterals.get(i).literal;
            for(int j = i+1; j < size; ++j) {
                if(literal == -cliterals.get(j).literal) {return true;}}}
        return false;}

    /** removes multiple occurrences of literals
     *
     * @return the number of removed literals
     */
    public int removeDoubles() {
        int doubled = 0;
        for(int i = 0; i < cliterals.size()-1; ++i) {
            int literal = cliterals.get(i).literal;
            for(int j = i+1; j < cliterals.size(); ++j) {
                CLiteralOld cliteral = cliterals.get(j);
                if(literal == cliteral.literal) {
                    removeAtPosition(j--);
                    ++doubled ;}}}
        return doubled;}

    /** sets the removed flag
     */
    public synchronized void setRemoved() {
        removed = true;}

    /** returns the removed flag
     *
     * @return the removed flag
     */
    public synchronized boolean isRemoved() {
        return removed;}

    /** checks if the two clauses overlap.
     *
     * @param clause a clause
     * @return [+1,literal] if they overlap with a literal, [-1,literal] if they overlap complementary, otherwise null
     */
    public int[] overlaps(ClauseOld clause) {
        if(connective != clause.connective) return null;
        for(CLiteralOld cLiteral1 : this) {
            int literal1 =cLiteral1.literal;
            for(CLiteralOld cLiteral2 : clause) {
                if(cLiteral2.literal ==  literal1) return new int[]{+1,literal1};
                if(cLiteral2.literal == -literal1) return new int[]{-1,literal1};}}
        return null;}

    /** returns the type-prefix with the clause id
     *
     * @return the type-prefix with the clause id
     */
    public String getName() {
        return connective.prefix+id;}

    /** computes the maximum width of the clause ids.
     *
     * @param clauses an array of clauses
     * @return the maximum width of the clause ids
     */
    public static int clauseNameWidth(ClauseOld[] clauses) {
        int width = 0;
        for(ClauseOld clause : clauses) {
            width = Math.max(width,clause.getName().length());}
        return width;}

    /** generates a string: clause-number: literals
     *
     * @return a string: clause-number: literals
     */
    public String toString() {
        return toString(0,null);}

    /** generates a string: clause-number: literals
     *
     * @return a string: clause-number: literals
     */
    public String toNumbers() {
        return toString(0,null);}

    /** generates a string: clause-number: literals
     *
     * @param width: 0 or the width of the id-part.
     * @param symboltable for mapping numbers to names
     * @return a string: clause-number: literals
     */
    public String toString(int width, Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        if(width > 0) {
            Formatter format = new Formatter(st, Locale.GERMANY);
            format.format("%-"+(width+ connective.prefix.length())+"s", getName()+":");}
        else st.append(connective.prefix+id+": ");
        if(connective.isQuantifier()) st.append(connective + " " + quAmount + ": ");
        int size = cliterals.size();
        for(int position = 0; position < size; ++position) {
            st.append(Symboltable.toString(cliterals.get(position).literal,symboltable));
            if(position < size-1) {st.append(connective.separator);}}
        return st.toString();}

    /** generates a string: clause-number: literals [origins]
     *
     * @param width       0 or the width of the id-part.
     * @param symboltable null or for mapping numbers to names
     * @return the clause as string
     */
    public String infoString(int width, Symboltable symboltable) {
        String st = toString(width,symboltable);
        if(inferenceStep != null) st += " " + sortIntArray(inferenceStep.origins()).toString();
        return st;}

    /** gets an iterator over the literals
     *
     * @return the iterator over the literals
     */
    @Override
    public Iterator<CLiteralOld> iterator() {
        return cliterals.iterator();}


    /** checks the clause and its literals for consistency.
     * Error messages are put into the StringBuilder
     *
     * @return true if the clause is okay
     */
    public boolean check(StringBuilder errors) {
        if(removed) {return true;}
        String prefix = "Clause " + id + ": ";
        boolean okay = true;

        if(connective.isQuantifier() && (quAmount <= 0 || quAmount > size())) {
            errors.append(prefix).append("Quantifier " + quAmount + " is not between 1 and " + size()+"\n");
            okay = false;}

        for(int i = 0; i < cliterals.size(); ++i) {
            CLiteralOld cliteral = cliterals.get(i);
            ClauseOld clause = cliteral.clause;
            if(clause == null) {
                errors.append(prefix).append("Literal " + cliteral.literal + " has no clause");
                okay = false;}
            else {
                if(clause != this) {
                    errors.append(prefix).append("Literal " + cliteral.literal + " has a different clause " + clause.id + "\n");
                    okay = false;}}
            if(cliteral.clausePosition != i) {
                errors.append(prefix).append("Literal " + cliteral.literal +
                        " has wrong position " + cliteral.clausePosition + " instead of " + i + "\n");
                okay = false;}}

        if(structure == null) {errors.append(prefix).append("Clause has no structure\n");}
        else {
            ClauseStructure struc = getStructure();
            if(structure != struc) {
                errors.append(prefix).append("Clause has wrong structure: " + structure.toString() +
                        ", and not " + struc.toString() + "\n");
                okay = false;}}
        return okay;}

}
