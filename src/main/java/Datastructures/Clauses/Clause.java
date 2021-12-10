package Datastructures.Clauses;


import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import InferenceSteps.Input;
import Utilities.Positioned;
import Utilities.Sizable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Formatter;
import java.util.Iterator;
import java.util.Locale;
import java.util.function.IntSupplier;
import java.util.function.IntUnaryOperator;

import static Utilities.Utilities.sortIntArray;

/** A clause is just a list of CLiterals.
 * It may represent clauses with quantification interval
 *
 * Created by ohlbach on 13.09.2018.
 */
public class Clause implements Iterable<CLiteral>, Positioned, Sizable {
    /** for identifying the clause*/
    public int id;

    /** the connective */
    public Connective connective;

    /** the quantification limit */
    public int limit = 0;

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
     * @param id its identifier */
    public Clause(int id) {
        this.id = id;
        cliterals = new ArrayList<>();
        inferenceStep = new Input(id);
    }

    /** constructs a clause with an empty list of literals.
     *
     * @param id       the clause problemId
     * @param limit    the quantification limit
     * @param size     the estimated number of literals
     */
    public Clause(int id, Connective connective, int limit, int size) {
        this.id = id;
        this.connective = connective;
        this.limit = limit;
        cliterals = new ArrayList<>(size);
        inferenceStep = new Input(id);
    }

    /** constructs a new clause with given literals
     *
     * @param id       the id of the new clause
     * @param limit    the quantification limit
     * @param literals the list of literals
     */
    public Clause(int id, Connective connective, int limit, IntArrayList literals) {
        this.id = id;
        this.connective = connective;
        this.limit = limit;
        cliterals = new ArrayList<>(literals.size());
        for (int i = 0; i < literals.size(); ++i) {
            cliterals.add(new CLiteral(literals.getInt(i), this, i));}
        inferenceStep = new Input(id);
        setStructure();
    }


    /** generates a clause from a basicClause
     * The constructor does not work for INTERVAL-type basic clauses.<br>
     * The new clause gets the same id as the basic clause.<br>
     * There is no semantic check.<br>
     * That means the limits must be okay (checked in BasicClauseList),<br>
     * and there msy still be multiple literals and tautologies in the clause.<br>
     * The limit of clauses where there is no limit (AND, EQUIV) is set to -1 <br>
     * Atmost-clauses like atmost 2 p,q,r are transformed into atleast 1 -p,-q,-r.<br>
     * Atleast-clauses like atleast 3 p,q,r are transformed into and p,q,r<br>
     * If the limit of ATLEAST-clauses is 1,the connective is set to OR.
     * The clause's structure is set to POSITIVE,NEGATIVE or MIXED.
     *
     * @param basicClause a basic clause [id,typenumber, limit, literal1,...]
     */
    public Clause(int[] basicClause) {
        connective = Connective.getType(basicClause[1]);
        assert(connective != null);

        id = basicClause[0];
        int length = basicClause.length;
        int start = 2;
        switch (connective) {
            case OR:      limit = 1; break;
            case AND:
            case EQUIV:   limit = -1; break; // there is no limit
            case ATLEAST: start = 3; limit = basicClause[2]; break;
            case ATMOST:  start = 3; limit = basicClause.length - 3  - basicClause[2]; break;
            default: assert(false);} // should not happen.

        cliterals = new ArrayList<>(length - start);
        for (int i = start; i < length; ++i) {
            int literal = basicClause[i];
            if(connective == Connective.ATMOST) literal = -literal; // transformation to atleast
            cliterals.add(new CLiteral(literal, this, cliterals.size()));}

        if(connective == Connective.ATMOST) connective = Connective.ATLEAST;
        if(connective == Connective.ATLEAST && limit == 1) connective = Connective.OR;
        if(connective == Connective.ATLEAST && limit == cliterals.size()) {
            connective = Connective.AND; limit = -1;}
        inferenceStep = new Input(id);
        setStructure();
    }

    /** creates a new clause with the given literals
     * The constructor does not work for INTERVAL-type basic clauses.<br>
     * There is no semantic check.<br>
     * That means the limits must be okay (checked in BasicClauseList),<br>
     * and there msy still be multiple literals and tautologies in the clause.<br>
     * The limit of clauses where there is no limit (AND, EQUIV) is set to -1 <br>
     * Atmost-clauses like atmost 2 p,q,r are transformed into atleast 1 -p,-q,-r.<br>
     * Atleast-clauses like atleast 3 p,q,r are transformed into and p,q,r<br>
     * If the limit of ATLEAST-clauses is 1,the connective is set to OR.
     * The clause's structure is set to POSITIVE,NEGATIVE or MIXED.
     *
     * @param id         the new id
     * @param connective the clause's type (no INTERVAL-type)
     * @param literals   [limit] a list of literals
     */
    public Clause(int id, Connective connective, int... literals) {
        this.id = id;
        this.connective = connective;
        int start = 0;
        int length = literals.length;
        switch (connective) {
            case OR:      limit = 1; break;
            case AND:
            case EQUIV:   limit = -1;break;
            case ATLEAST: limit = literals[0];  start = 1;break;
            case ATMOST:
                start = 1;
                limit = length - 1 - literals[0];
                for(int i = 1; i < literals.length; ++i) literals[i] *= -1;
                break;
            default: assert(false);} // should not happen
        cliterals = new ArrayList<>(literals.length);
        for (int i = start; i < length; ++i) {
            cliterals.add(new CLiteral(literals[i], this, i - start));}

        if(this.connective == Connective.ATMOST) this.connective = Connective.ATLEAST;
        if(this.connective == Connective.ATLEAST && limit == 1) this.connective = Connective.OR;
        if(this.connective == Connective.ATLEAST && limit == cliterals.size()) {
            this.connective = Connective.AND; limit = -1;}
        inferenceStep = new Input(id);
        setStructure();}

    /** Transforms an INTERVAL-clause into (usually) two ATLEAST-clauses.
     * Example: [2,4] p,q,r,s,t -> atleast 2 p,q,r,s,t and atleast 1 -p,-q,-r,-s,-t<br>
     * Special cases are [0,m] p,... (actuall atmost m p,...) <br>
     * and [n,k] p_1,..,p_k  (actually atleast n p_1,...) <br>
     * They generate only one clause.
     *
     * @param nextInt     for determinining the clause id
     * @param basicClause a basic interval-clause
     * @return            one or two new clauses
     */
    public ArrayList<Clause> intervalClause(IntSupplier nextInt, int[] basicClause) {
        connective = Connective.getType(basicClause[1]);
        assert(connective != Connective.INTERVAL);
        ArrayList<Clause> clauses = new ArrayList<>();
        int min = basicClause[2];
        int max = basicClause[3];
        int length = basicClause.length - 3;
        int[] literals = new int[length+1];
        System.arraycopy(basicClause, 4, literals, 1, basicClause.length - 4);

        if(min == 0) {
            literals[0] = max;
            clauses.add(new Clause(nextInt.getAsInt(),Connective.ATMOST,literals));
            return clauses;}

        if(max == length) {
            literals[0] = min;
            clauses.add(new Clause(nextInt.getAsInt(),Connective.ATLEAST,literals));
            return clauses;}

        literals[0] = min;
        clauses.add(new Clause(nextInt.getAsInt(),Connective.ATLEAST,literals));

        literals[0] = max;
        clauses.add(new Clause(nextInt.getAsInt(),Connective.ATMOST,literals));
        return clauses;}


    /** creates a clone of the clause.
     * Only the literals themselves are cloned
     *
     * @param id the identifier for the new clone
     * @return the new clone
     */
    public Clause clone(int id) {
        Clause clause = new Clause(id, connective, limit, cliterals.size());
        for (CLiteral cLiteral : cliterals) {clause.add(cLiteral.literal);}
        clause.structure = structure;
        return clause;}


    /** copies the literals of the clause to an IntArrayList
     *
     * @return the literals as IntArrayList
     */
    public IntArrayList toArray() {
        IntArrayList list = new IntArrayList(cliterals.size());
        for (CLiteral cLiteral : cliterals) list.add(cLiteral.literal);
        return list;
    }

    /** determines for an ATLEAST- or OR-clause the clause's structure:
     * If the limit is too large: CONTRADICTORY<br>
     * It the limit is 0: TAUTOLOGY,<br>
     * otherwise if the clause has only positive literals (POSITIVE) or only negative literals (NEGATIVE)
     * or mixed signs (MIXED)
     *
     * @return the corresponding value for the structure.
     */
    public ClauseStructure detStructure() {
        assert(connective == Connective.OR || connective == Connective.ATLEAST);
        if(limit > cliterals.size()) return ClauseStructure.CONTRADICTORY;
        if(limit == 0) return ClauseStructure.TAUTOLOGY;
        if(limit == cliterals.size()) {connective = Connective.AND; return structure;}
        int positive = 0;
        int negative = 0;
        for (CLiteral cLiteral : cliterals) {
            if (cLiteral.literal > 0) {++positive;}
            else {++negative;}}
        if (positive == 0) {return ClauseStructure.NEGATIVE;}
        if (negative == 0) {return ClauseStructure.POSITIVE;}
        return ClauseStructure.MIXED;}

    /** computes and sets the structure of the clause */
    public void setStructure() {
        structure = detStructure();}

    /** returns the list position, or -1
     *
     * @return the list position, or -1 */
    public int getPosition() {
        return listPosition;
    }

    /** sets the list position
     *
     * @param position a position within a list. */
    public void setPosition(int position) {
        listPosition = position;
    }

    /** return the current number of literals
     *
     * @return the current number of literals */
    public int size() {
        return cliterals.size();
    }

    /** checks if the clause is empty
     *
     * @return true if the clause is empty. */
    public boolean isEmpty() {
        return cliterals.isEmpty();
    }

    /** checks if the clause has only positive literals
     *
     * @return true if the clause has only positive literals */
    public boolean isPositive() {
        return structure == ClauseStructure.POSITIVE;
    }

    /** checks if the clause has only negative literals
     *
     * @return true if the clause has only negative literals. */
    public boolean isNegative() {
        return structure == ClauseStructure.NEGATIVE;
    }

    /** gets the literal at the given clausePosition
     *
     * @param position a literal clausePosition
     * @return the cliteral at that clausePosition. */
    public CLiteral getCLiteral(int position) {
        assert position >= 0 && position < cliterals.size();
        return cliterals.get(position);
    }

    /** gets the literal at the given clausePosition
     *
     * @param position a literal clausePosition
     * @return the literal at that clausePosition.*/
    public int getLiteral(int position) {
        assert position >= 0 && position < cliterals.size();
        return cliterals.get(position).literal;}

    /** converts the cLiterals into an array of integers
     *
     * @return the literals as integers. */
    public int[] getLiterals() {
        int size = cliterals.size();
        int[] literals = new int[size];
        for (int i = 0; i < size; ++i) {literals[i] = cliterals.get(i).literal;}
        return literals;}

    /** checks if the literal is in the clause
     *
     * @param literal a literal
     * @return +1: the literal is in the clause, -1: the negated literal is in the clause, otherwise 0
     */
    public int contains(int literal) {
        return contains(literal, cliterals.size());}

    /** checks if the literal is in the clause
     *
     * @param literal a literal
     * @param end     where the iteration over the literals stops
     * @return +1: the literal is in the clause, -1: the negated literal is in the clause, otherwise 0
     */
    public int contains(int literal, int end) {
        for (int i = 0; i < end; ++i) {
            int lit = cliterals.get(i).literal;
            if (lit == literal)  {return +1;}
            if (lit == -literal) {return -1;}}
        return 0;}

    /** adds a cliteral to the end of the clause, without checking for double literals and tautologies.
     *
     * @param cliteral the literal to be added.
     */
    public void add(CLiteral cliteral) {
        int position = cliterals.size();
        cliterals.add(cliteral);
        cliteral.setClause(this, position);}

    /** adds a new literal to the clause
     *
     * @param literal a literal
     */
    public void add(int literal) {
        int position = cliterals.size();
        CLiteral cliteral = new CLiteral(literal, this, position);
        cliterals.add(cliteral);}

    /** removes a cliteral from the clause.
     *
     * @param cLiteral the literal to be removed.
     */
    public void remove(CLiteral cLiteral) {
        removeAtPosition(cLiteral.clausePosition);
    }

    /** removes a cliteral at the given clausePosition from the clause.
     * Nothing else is done.
     *
     * @param position the clausePosition of the literal to be removed
     */
    public void removeAtPosition(int position) {
        int size = cliterals.size();
        assert position >= 0 && position < size;
        for (int pos = position; pos < size - 1; ++pos) {
            CLiteral nextliteral = cliterals.get(pos + 1);
            nextliteral.clausePosition = pos;
            cliterals.set(pos, nextliteral);}
        cliterals.remove(size - 1);}

    /** replaces literals by the representatives in an equivalence class.
     * If nextInt != null then the replacement is done in a clone of the clause, otherwise in the original clause
     * Besides the replacements, nothing is changed.
     *
     * @param getRepresentative maps a literal to its representative in an equivalence class
     * @param nextInt           null or a function that returns the next clause id for a clone of the clause
     * @param replacements      collects pairs of replacements: oldLiteral -> newLiteral
     * @return either the original clause or the clone with the replacements
     */
    public Clause replaceEquivalences(IntUnaryOperator getRepresentative, IntSupplier nextInt, IntArrayList replacements) {
        replacements.clear();
        Clause clause = this;
        ArrayList<CLiteral> cLits = cliterals;
        for (int i = 0; i < cLits.size(); ++i) {
            int oldLiteral = cLits.get(i).literal;
            int newLiteral = getRepresentative.applyAsInt(oldLiteral);
            if (oldLiteral == newLiteral) continue;
            replacements.add(oldLiteral);
            replacements.add(newLiteral);
            if (clause == this && nextInt != null) {
                clause = clone(nextInt.getAsInt());
                cLits = clause.cliterals;}
            cLits.get(i).literal = newLiteral;}
        if (!replacements.isEmpty()) {clause.setStructure();}
        return clause;}

    /** replaces all true and false literals in the clause.
     * If nextId != null then the replacements is done on a clone of the clause, otherwise on the clause itself.<br>
     * The clause may become a tautology or contradictory.<br>
     * In both cases the structure of the clause is set accordingly<br>
     * The connective of the clause is adjusted at the new situation.
     *
     * @param getTruthStatus maps a literal to +1 (true), 0 (undefined) or -1 (false)
     * @param nextInt        null or a supplier for the identifier for a clone of the clause
     * @param removedTrueLiterals collects the removed true literals
     * @param removedFalseLiterals collects the removed false literals
     * @return               either the original changed clause or a clone.
     */
    public Clause removeTrueFalseLiterals(IntUnaryOperator getTruthStatus, IntSupplier nextInt,
                                      IntArrayList removedTrueLiterals, IntArrayList removedFalseLiterals) {
        removedTrueLiterals.clear();removedFalseLiterals.clear();
        Clause clause = this;
        ArrayList<CLiteral> cLits = cliterals;
        for (int i = 0; i < cLits.size(); ++i) {
            int literal = cLits.get(i).literal;
            int status = getTruthStatus.applyAsInt(literal);
            if(status == 0) continue;
            if (clause == this && nextInt != null) {
                clause = clone(nextInt.getAsInt());
                cLits = clause.cliterals;}
            if(status == 1) {
                if(!removedTrueLiterals.contains(literal)) removedTrueLiterals.add(literal);
                clause.limit -= 1;
                if(clause.limit == 0) {
                    clause.structure = ClauseStructure.TAUTOLOGY;
                    return clause;}}
            else {if(!removedFalseLiterals.contains(literal)) removedFalseLiterals.add(literal);}
            clause.removeAtPosition(i--);
            if(clause.limit > clause.size()) {
                clause.structure = ClauseStructure.CONTRADICTORY;
                return clause;}
            if(clause.limit == clause.size()) {
                clause.connective = Connective.AND;
                return clause;}}
        return clause;}

    /** checks for multiple occurrences and complementary literals
     *  Multiple occurrences of literals > limit are removed.<br>
     *  Complementary literals in other clause types are removed and the limit decreased.<br>
     *  The clause's structure may become TAUTOLOGY or CONTRADICTORY.
     *  If nextInt != null then the clause itself is not modified, but a clone is created and modified.
     *
     * @param nextInt        null or a supplier for the identifier for a clone of the clause
     * @param doubleLiterals collects the removed double literals
     * @param complementaryLiterals collects the removed complementary literals
     *
     * @return the simplified clause, or a simplified clone
     */
    public Clause removeMultipleAndComplementaryLiterals(IntSupplier nextInt,
                                                         IntArrayList doubleLiterals,
                                                         IntArrayList complementaryLiterals)  {
        assert(connective == Connective.OR || connective == Connective.ATLEAST);
        doubleLiterals.clear();
        complementaryLiterals.clear();
        Clause clause = this;
        ArrayList<CLiteral> cLits = cliterals;
        int multipleLiteralsCounter = 0;
        for(int i = 0; i < cLits.size(); ++i) {
            int literal1 = cLits.get(i).literal;
            for(int j = 0; j < i; ++j) {
                int literal2 = cLits.get(j).literal;
                if(literal1 == literal2) {
                    ++multipleLiteralsCounter;
                    if(multipleLiteralsCounter < clause.limit) break;
                    if(!doubleLiterals.contains(literal1)) doubleLiterals.add(literal1);
                    if (clause == this && nextInt != null) {
                        clause = clone(nextInt.getAsInt());
                        cLits = clause.cliterals;}
                    clause.removeAtPosition(i--); break;}
                if(literal1 == -literal2) {
                    if (clause == this && nextInt != null) {
                        clause = clone(nextInt.getAsInt());
                        cLits = clause.cliterals;}
                    complementaryLiterals.add(Math.abs(literal1));
                    clause.removeAtPosition(i); clause.removeAtPosition(j); i -= 2;
                    clause.limit -= 1;
                    if(clause.limit == 0) {clause.structure = ClauseStructure.TAUTOLOGY; return clause;}
                    break;}}}
        clause.setStructure();
        return clause;}

    /** removes all occurrences of the literal from the clause.
     * If nextInt != null, a new clause is created, otherwise the removal is destructive
     *
     * @param literal the literal to be removed
     * @param nextInt if != null, it provides the next id for the new clause
     * @param truth   if true then a true literal is removed, otherwise a false literal
     * @return        the old or the new clause.
     */
    public Clause removeLiteral(int literal, IntSupplier nextInt,  boolean truth) {
        Clause clause = this;
        ArrayList<CLiteral> cLits = cliterals;
        for(int i = 0; i < cLits.size(); ++i) {
            if(literal == cLits.get(i).literal) {
                if(truth && clause.limit == 1) return null;
                if(clause == this && nextInt != null) {
                    clause = clone(nextInt.getAsInt());
                    cLits = clause.cliterals;}
                cLits.remove(i--);
                if(truth) clause.limit -= 1;
                if(limit == 0) {clause.structure = ClauseStructure.TAUTOLOGY;break;}}}
        clause.setStructure();
        return clause;}

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
        if(limit < clause2.limit) return false;
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

    /** sets the removed flag */
    public synchronized void setRemoved() {
        removed = true;}

    /** returns the removed flag
     *
     * @return the removed flag*/
    public synchronized boolean isRemoved() {
        return removed;}

    /** checks if the two clauses overlap.
     *
     * @param clause a clause
     * @return [+1,literal] if they overlap with a literal, [-1,literal] if they overlap complementary, otherwise null
     */
    public int[] overlaps(Clause clause) {
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
        return connective.prefix+id;}

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
            format.format("%-"+(width+ connective.prefix.length())+"s", getName()+":");}
        else st.append(connective.prefix+id+": ");
        if(limit > 1) st.append(limit).append(": ");
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

        if(limit > size()) {
            errors.append(prefix).append("Limit " + limit + " is not between 0 and " + size()+"\n");
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
            ClauseStructure struc = detStructure();
            if(structure != struc) {
                errors.append(prefix).append("Clause has wrong structure: " + structure.toString() +
                        ", and not " + struc.toString() + "\n");
                okay = false;}}
        return okay;}

    public ArrayList<CLiteral> getCliterals() {
        return cliterals;
    }
}

