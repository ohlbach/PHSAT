package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import InferenceSteps.Input;
import Utilities.Positioned;
import Utilities.Sizable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.*;

import static Utilities.Utilities.sortIntArray;

/** A clause is just a list of CLiterals.
 * It may represent clauses of type OR, AND, EQUIV, ATLEAST, ATMOST or EXACTLY
 *
 * Created by ohlbach on 13.09.2018.
 */
public class Clause implements Iterable<CLiteral>, Positioned, Sizable {
    /** for identifying the clause */
    public int id;
    /** OR,DISJOINT, ... */
    public ClauseType clauseType;
    /** for numeric types (atleast, atmost, exactly)*/
    public int quantifier = 0;
    /** the literals */
    public ArrayList<CLiteral> cliterals;
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
     * @param clauseType its type
     */
    public Clause(int id, ClauseType clauseType) {
        this.id = id;
        this.clauseType = clauseType;
        cliterals = new ArrayList<>();
        inferenceStep = new Input(id);
    }

    /** constructs a clause
     *
     * @param id   the clause problemId
     * @param clauseType  the clause's type
     * @param size  the estimated number of literals
     */
    public Clause(int id, ClauseType clauseType, int size) {
        this.id = id;
        this.clauseType = clauseType;
        cliterals = new ArrayList<>(size);
        inferenceStep = new Input(id);}

    /** constructs a new clause with given literals
     *
     * @param id        the id of the new clause
     * @param clauseType  the clause's type
     * @param literals the list of literals
     */
    public Clause(int id, ClauseType clauseType, IntArrayList literals) {
        assert !clauseType.isNumeric();
        this.id = id;
        this.clauseType = clauseType;
        cliterals = new ArrayList<>(literals.size());
        for(int i = 0; i < literals.size(); ++i) {
            cliterals.add(new CLiteral(literals.getInt(i),this,i));}
        inferenceStep = new Input(id);
        setStructure();}

    /** constructs a new clause with given literals
     *
     * @param id        the id of the new clause
     * @param clauseType  a clause's type
     * @param quantifier a quantifier for the clause (if its numeric)
     * @param literals the list of literals
     */
    public Clause(int id, ClauseType clauseType, int quantifier, IntArrayList literals) {
        this.id = id;
        this.clauseType = clauseType;
        this.quantifier = clauseType.isNumeric() ? quantifier : 0;
        cliterals = new ArrayList<>(literals.size());
        for(int i = 0; i < literals.size(); ++i) {
            cliterals.add(new CLiteral(literals.getInt(i),this,i));}
        inferenceStep = new Input(id);
        setStructure();}

    /** constructs a new clause with given literals
     *
     * @param id        the id of the new clause
     * @param clauseType  the clause's type
     * @param cLiterals the list of CLiterals
     */
    public Clause(int id, ClauseType clauseType, ArrayList<CLiteral> cLiterals) {
        this.id = id;
        this.clauseType = clauseType;
        for(int i = 0; i < cLiterals.size(); ++i) {
            cLiterals.get(i).setClause(this,i);}
        cliterals = cLiterals;
        inferenceStep = new Input(id);
        setStructure();}

    /** constructs a new clause with given literals
     * Notice that the quantifier is set to 0 if the clause type is not numeric
     *
     * @param id        the id of the new clause
     * @param clauseType  the clause's type
     * @param quantifier for numeric clause types
     * @param cLiterals the list of CLiterals
     */
    public Clause(int id, ClauseType clauseType, int quantifier, ArrayList<CLiteral> cLiterals) {
        this.id = id;
        this.clauseType = clauseType;
        this.quantifier = clauseType.isNumeric() ? quantifier : 0;
        for(int i = 0; i < cLiterals.size(); ++i) {
            cLiterals.get(i).setClause(this,i);}
        cliterals = cLiterals;
        inferenceStep = new Input(id);
        setStructure();}

    /** generates a clause from a basicClause
     *
     * @param id           the name of the clause
     * @param basicClause  a basic clause [number,type,literal1,...]
     */
    public Clause(int id, int[] basicClause) {   // depricated
        this.id = id;
        int typenumber = basicClause[1];
        clauseType = ClauseType.getType(typenumber);
        int start = 2;
        int shift = 2;
        if(ClauseType.isNumeric(typenumber)) {start = 3; shift = 3; quantifier = basicClause[2];}
        int length = basicClause.length;
        cliterals = new ArrayList<>(length - shift);
        for(int i = start; i < length; ++i) {
            int literal = basicClause[i];
            cliterals.add(new CLiteral(literal,this,cliterals.size()));}
        inferenceStep = new Input(id);
        setStructure();}

    /** generates a clause from a basicClause
     * The new clause gets the same id as the basic clause
     *
     * @param basicClause  a basic clause [id,typenumber, [quantifier], literal1,...]
     */
    public Clause(int[] basicClause) {
        id = basicClause[0];
        int typenumber = basicClause[1];
        clauseType = ClauseType.getType(typenumber);
        int start = 2;
        int shift = 2;
        if(ClauseType.isNumeric(typenumber)) {start = 3; shift = 3; quantifier = basicClause[2];}
        int length = basicClause.length;
        cliterals = new ArrayList<>(length - shift);
        for(int i = start; i < length; ++i) {
            int literal = basicClause[i];
            cliterals.add(new CLiteral(literal,this,cliterals.size()));}
        inferenceStep = new Input(id);
        setStructure();}

    /** creates a new clause with the given literals
     *
     * @param id          the new id
     * @param clauseType  the clause's type
     * @param literals    [quantifier] a list of literals
     */
    public Clause(int id, ClauseType clauseType, int... literals) {
        this.id = id;
        int start = 0;
        if(clauseType.isNumeric()) {quantifier = literals[0]; start = 1;}
        this.clauseType = clauseType;
        cliterals = new ArrayList<>(literals.length);
        for(int i = start; i< literals.length; ++i) {
            cliterals.add(new CLiteral(literals[i],this,i-start));}
        inferenceStep = new Input(id);
        setStructure();}

    /** creates a clone of the clause.
     * Only the literals themselves are cloned
     *
     * @param id the identifier for the new clone
     * @return the new clone
     */
    public Clause clone(int id) {
        Clause clause = new Clause(id,this.clauseType);
        clause.quantifier = quantifier;
        for(CLiteral cLiteral : cliterals) {clause.add(cLiteral.literal);}
        return clause;}

    /** creates a clone of the clause.
     * Only the literals themselves are cloned
     *
     * @param id the identifier for the new clone
     * @param ignorePosition the position of a literal to be ignored
     * @return the new clone
     */
    public Clause clone(int id, int ignorePosition) {
        Clause clause = new Clause(id,this.clauseType);
        clause.quantifier = quantifier;
        for(int i = 0; i < size(); ++i) {
            if(i != ignorePosition) {clause.add(getLiteral(i));}}
        return clause;}

    /** clones the clause except the given literal
     *
     * @param id             a new id of the clause
     * @param ignoreLiteral  a literal to be ignored
     * @return               the cloned clause
     */
    public Clause cloneExcept(int id, int ignoreLiteral) {
        Clause clause = new Clause(id,this.clauseType);
        clause.quantifier = quantifier;
        for(int i = 0; i < size(); ++i) {
            int literal = getLiteral(i);
            if(literal != ignoreLiteral) {clause.add(literal);}}
        return clause;}

    /** checks if the clause is positive, negative or mixed and returns the corresponding value for the structure.
     */
    public ClauseStructure getStructure() {
        int positive = 0;
        int negative = 0;
        for(CLiteral cLiteral : cliterals) {
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
    public CLiteral getCLiteral(int position) {
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
        for(CLiteral cliteral : this) {
            int lit = cliteral.literal;
            if(lit ==  literal) {return +1;}
            if(lit == -literal) {return -1;}}
        return 0;}

    /** adds a cliteral to the end of the clause, without checking for double literals and tautologies.
     *
     * @param cliteral the literal to be added.
     */
    public void add(CLiteral cliteral) {
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
        CLiteral cliteral = new CLiteral(literal,this,position);
        cliterals.add(cliteral);
        setStructure();}

    /** removes a cliteral from the clause.
     *
     * @param cLiteral the literal to be removed.
     */
    public void remove(CLiteral cLiteral) {
        removeAtPosition(cLiteral.clausePosition);}

    /** removes a cliteral at the given clausePosition from the clause.
     *
     * @param position the clausePosition of the literal to be removed
     */
    public void removeAtPosition(int position) {
        int size = cliterals.size();
        assert position >= 0 && position < size;
        for(int pos = position; pos < size-1; ++pos) {
            CLiteral nextliteral = cliterals.get(pos+1);
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
    public int contains(int literal, CLiteral ignore) {
        for(CLiteral cLiteral : this) {
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
    public boolean isSubset(Clause clause2) {
        if(clauseType != clause2.clauseType) return false;
        for(CLiteral cl : cliterals) {
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
     * @return true if there were multiple occurrences of literals
     */
    public boolean removeDoubles() {  // INDEX
        boolean doubles = false;
        for(int i = 0; i < cliterals.size()-1; ++i) {
            int literal = cliterals.get(i).literal;
            for(int j = i+1; j < cliterals.size(); ++j) {
                CLiteral cliteral = cliterals.get(j);
                if(literal == cliteral.literal) {
                    removeAtPosition(j--);
                    doubles = true;}}}
        return doubles;}

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
    public int[] overlaps(Clause clause) {
        if(clauseType != clause.clauseType) return null;
        for(CLiteral cLiteral1 : this) {
            int literal1 =cLiteral1.literal;
            for(CLiteral cLiteral2 : clause) {
                if(cLiteral2.literal ==  literal1) return new int[]{+1,literal1};
                if(cLiteral2.literal == -literal1) return new int[]{-1,literal1};}}
        return null;}

    /** returns the type-prefix with the clause id
     *
     * @return the type-prefix with the clause id
     */
    public String getName() {
        return clauseType.prefix+id;}

    /** computes the maximum width of the clause ids.
     *
     * @param clauses an array of clauses
     * @return the maximum width of the clause ids
     */
    public static int clauseNameWidth(Clause[] clauses) {
        int width = 0;
        for(Clause clause : clauses) {
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
            format.format("%-"+(width+clauseType.prefix.length())+"s", getName()+":");}
        else st.append(clauseType.prefix+id+": ");
        if(clauseType.isNumeric()) st.append(clauseType.toString() + " " + quantifier + ": ");
        int size = cliterals.size();
        for(int position = 0; position < size; ++position) {
            st.append(Symboltable.toString(cliterals.get(position).literal,symboltable));
            if(position < size-1) {st.append(clauseType.separator);}}
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
    public Iterator<CLiteral> iterator() {
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

        if(clauseType.isNumeric() && (quantifier <= 0 || quantifier > size())) {
            errors.append(prefix).append("Quantifier " + quantifier + " is not between 1 and " + size()+"\n");
            okay = false;}

        for(int i = 0; i < cliterals.size(); ++i) {
            CLiteral cliteral = cliterals.get(i);
            Clause clause = cliteral.clause;
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
