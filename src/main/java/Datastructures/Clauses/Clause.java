package Datastructures.Clauses;


import Datastructures.Literals.CLiteral;
import Datastructures.Results.Aborted;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import InferenceSteps.Input;
import Utilities.Interval;
import Utilities.Positioned;
import Utilities.Sizable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import java.util.*;
import java.util.function.BinaryOperator;
import java.util.function.IntSupplier;
import java.util.function.IntUnaryOperator;

import static Utilities.Utilities.makeClause;
import static Utilities.Utilities.sortIntArray;

/** A clause is just a list of CLiterals.
 * It may represent clauses with quantification interval
 *
 * Created by ohlbach on 13.09.2018.
 */
public class Clause implements Iterable<CLiteral>, Positioned, Sizable {
    /**
     * for identifying the clause
     */
    public int id;
    /**
     * the connective
     */
    public Connective connective;
    /**
     * the quantification interval
     */
    public Interval interval;
    /**
     * the literals
     */
    public ArrayList<CLiteral> cliterals;
    /**
     * indicates that the clause has been removed
     */
    public boolean removed = false;
    /**
     * for sorting clauses, for example in a listPosition queue
     */
    public int listPosition = -1;
    /**
     * positive, negative or mixed
     */
    public ClauseStructure structure = null;
    /**
     * a timestamp to be used by corresponding algorithms
     */
    public int timestamp = 0;
    /**
     * some auxiliary pointer
     */
    public Object aux = null;
    /**
     * the reason for deriving the clause
     */
    public InferenceStep inferenceStep;

    /**
     * constructs a new clause
     *
     * @param id its identifier
     */
    public Clause(int id) {
        this.id = id;
        cliterals = new ArrayList<>();
        inferenceStep = new Input(id);
    }

    /**
     * constructs a clause
     *
     * @param id       the clause problemId
     * @param interval the clause's interval
     * @param size     the estimated number of literals
     */
    public Clause(int id, Connective connective, Interval interval, int size) {
        this.id = id;
        this.connective = connective;
        this.interval = interval;
        cliterals = new ArrayList<>(size);
        inferenceStep = new Input(id);
    }

    /**
     * constructs a new clause with given literals
     *
     * @param id       the id of the new clause
     * @param interval the clause's interval
     * @param literals the list of literals
     */
    public Clause(int id, Connective connective, Interval interval, IntArrayList literals) {
        this.id = id;
        this.connective = connective;
        this.interval = interval;
        cliterals = new ArrayList<>(literals.size());
        for (int i = 0; i < literals.size(); ++i) {
            cliterals.add(new CLiteral(literals.getInt(i), this, i));
        }
        inferenceStep = new Input(id);
        setConnective();
        setStructure();
    }


    /**
     * generates a clause from a basicClause
     * The new clause gets the same id as the basic clause
     *
     * @param basicClause a basic clause [id,typenumber, [start,end], literal1,...]
     * @throws Aborted if the clause type is unknown.
     */
    public Clause(int[] basicClause) {
        connective = Connective.getType(basicClause[1]);
        id = basicClause[0];
        int length = basicClause.length;
        int start = 2;
        switch (connective) {
            case OR:
                interval = new Interval(1, length - start);
                break;
            case AND:
                interval = null;
                break;
            case EQUIV:
                interval = null;
                break;
            case INTERVAL:
                start = 4;
                interval = new Interval(basicClause[2], basicClause[3]);
                break;
            case ATLEAST:
                start = 3;
                interval = new Interval(basicClause[2], length - start);
                break;
            case ATMOST:
                start = 3;
                interval = new Interval(0, basicClause[2]);
                break;
            case EXACTLY:
                start = 3;
                interval = new Interval(basicClause[2], basicClause[2]);
                break;
        }
        cliterals = new ArrayList<>(length - start);
        for (int i = start; i < length; ++i) {
            int literal = basicClause[i];
            cliterals.add(new CLiteral(literal, this, cliterals.size()));
        }
        inferenceStep = new Input(id);
        setConnective();
        setStructure();
    }

    /**
     * creates a new clause with the given literals
     *
     * @param id         the new id
     * @param connective the clause's type
     * @param literals   [quantifier] a list of literals
     */
    public Clause(int id, Connective connective, int... literals) {
        this.id = id;
        this.connective = connective;
        int start = 0;
        int length = literals.length;
        switch (connective) {
            case OR:
                interval = new Interval(1, length);
                break;
            case AND:
                interval = null;
                break;
            case EQUIV:
                interval = null;
                break;
            case INTERVAL:
                start = 2;
                interval = new Interval(literals[0], literals[1]);
                break;
            case ATLEAST:
                start = 1;
                interval = new Interval(literals[0], length - start);
                break;
            case ATMOST:
                start = 1;
                interval = new Interval(0, literals[0]);
                break;
            case EXACTLY:
                start = 1;
                interval = new Interval(literals[0], literals[0]);
                break;
        }

        cliterals = new ArrayList<>(literals.length);
        for (int i = start; i < literals.length; ++i) {
            cliterals.add(new CLiteral(literals[i], this, i - start));
        }
        inferenceStep = new Input(id);
        setConnective();
        setStructure();
    }

    /**
     * determines the connective that corresponds to the interval and changes it to the correct value.
     * A clause exactly 0 l1...ln is changed to and -l1...-ln
     */
    private void setConnective() {
        if (connective == Connective.AND) {
            interval = null;
            return;}
        if (connective == Connective.OR) {
            if (cliterals.size() == 1) {
                interval = null;
                connective = Connective.AND;}
            return;}

        if (interval == null) return;
        interval.max = Math.min(interval.max, size());
        connective = detConnective();
        if (connective == Connective.AND) {
            interval = null;
            return;
        }
        if (connective == Connective.OR) {
            interval = null;
            if (cliterals.size() == 1) {
                connective = Connective.AND;
            }
        }
        if (connective == Connective.EXACTLY && interval.min == 0) {
            for (CLiteral cLiteral : cliterals) {
                cLiteral.literal *= -1;
            }
            connective = Connective.AND;
            interval = null;
        }
    }

    /**
     * determines the connective from the size of the interval and the size of the literals
     *
     * @return the corresponding connective.
     */
    public Connective detConnective() {
        if (interval == null || connective == Connective.OR) return connective;
        int min = interval.min;
        int max = interval.max;
        int length = cliterals.size();
        if (min == 1 && max == length) return Connective.OR;
        if (min == max && max == length && length > 0) return Connective.AND;
        if (min == max) return Connective.EXACTLY;
        if (max == length) return Connective.ATLEAST;
        if (min == 0) return Connective.ATMOST;
        return connective;
    }

    /**
     * creates a clone of the clause.
     * Only the literals themselves are cloned
     *
     * @param id the identifier for the new clone
     * @return the new clone
     */
    public Clause clone(int id) {
        Interval intv = interval == null ? null : interval.clone();
        Clause clause = new Clause(id, connective, intv, cliterals.size());
        for (CLiteral cLiteral : cliterals) {
            clause.add(cLiteral.literal);
        }
        clause.structure = structure;
        return clause;
    }

    /**
     * creates a clone of the clause.
     * Only the literals themselves are cloned
     *
     * @param id             the identifier for the new clone
     * @param ignorePosition the position of a literal to be ignored
     * @param truth          if true then the interval is reduced as if removing a true literal, otherwise a false literal
     * @return the new clone
     */
    public Clause clone(int id, int ignorePosition, boolean truth) {
        Clause clause = new Clause(id, connective, interval.clone(), cliterals.size() - 1);
        for (int i = 0; i < size(); ++i) {
            if (i != ignorePosition) {
                clause.add(getLiteral(i));
            }
        }
        clause.decrementInterval(truth, 1);
        setConnective();
        return clause;
    }

    /**
     * decrements the interval in case a literal is removed.
     *
     * @param truth   true if a true literal is removed, false if a false literal is removed
     * @param removed the number of removed occurrences of the literal
     */
    private void decrementInterval(boolean truth, int removed) {
        for (int i = 0; i < removed; ++i) {
            if (truth) {
                interval.decrement();
            } else {
                interval.max = Math.min(interval.max, cliterals.size());
            }
        }
    }

    /**
     * clones the clause except the given literal
     *
     * @param id            a new id of the clause
     * @param ignoreLiteral a literal to be ignored
     * @return the cloned clause
     */
    public Clause cloneExcept(int id, int ignoreLiteral, boolean truth) {
        Clause clause = new Clause(id, connective, interval.clone(), cliterals.size() - 1);
        int removed = 0;
        for (int i = 0; i < size(); ++i) {
            int literal = getLiteral(i);
            if (literal == ignoreLiteral) {
                ++removed;
                continue;
            }
            clause.add(literal);
        }
        clause.decrementInterval(truth, removed);
        clause.setConnective();
        return clause;
    }

    /**
     * copies the literals of the clause to an IntArrayList
     *
     * @return the literals as IntArrayList
     */
    public IntArrayList toArray() {
        IntArrayList list = new IntArrayList(cliterals.size());
        for (CLiteral cLiteral : cliterals) list.add(cLiteral.literal);
        return list;
    }

    /**
     * determines the clause's structure: if the clause is positive, negative or mixed
     *
     * @return the corresponding value for the structure.
     */
    public ClauseStructure detStructure() {
        if(interval != null) {
            int min = interval.min;
            int max = interval.max;
            if(min < 0 || max < 0 || max < min || min > cliterals.size()) {
                return ClauseStructure.CONTRADICTORY;}
            if(interval.min == 0 && interval.max == cliterals.size()) {
                return ClauseStructure.TAUTOLOGY;}}
        int positive = 0;
        int negative = 0;
        for (CLiteral cLiteral : cliterals) {
            if (cLiteral.literal > 0) {++positive;}
            else {++negative;}}
        ClauseStructure structure = ClauseStructure.MIXED;
        if (positive == 0) {structure = ClauseStructure.NEGATIVE;}
        else {if (negative == 0) {structure = ClauseStructure.POSITIVE;}}
        return structure;}

    /**
     * computes and sets the structure of the clause
     */
    public void setStructure() {
        structure = detStructure();}

    /**
     * returns the list position, or -1
     *
     * @return the list position, or -1
     */
    public int getPosition() {
        return listPosition;
    }

    /**
     * sets the list position
     *
     * @param position a position within a list.
     */
    public void setPosition(int position) {
        listPosition = position;
    }

    /**
     * return the current number of literals
     *
     * @return the current number of literals
     */
    public int size() {
        return cliterals.size();
    }

    /**
     * checks if the clause is empty
     *
     * @return true if the clause is empty.
     */
    public boolean isEmpty() {
        return cliterals.isEmpty();
    }

    /**
     * checks if the clause has only positive literals
     *
     * @return true if the clause has only positive literals
     */
    public boolean isPositive() {
        return structure == ClauseStructure.POSITIVE;
    }

    /**
     * checks if the clause has only negative literals
     *
     * @return true if the clause has only negative literals.
     */
    public boolean isNegative() {
        return structure == ClauseStructure.NEGATIVE;
    }

    /**
     * gets the literal at the given clausePosition
     *
     * @param position a literal clausePosition
     * @return the cliteral at that clausePosition.
     */
    public CLiteral getCLiteral(int position) {
        assert position >= 0 && position < cliterals.size();
        return cliterals.get(position);
    }

    /**
     * gets the literal at the given clausePosition
     *
     * @param position a literal clausePosition
     * @return the literal at that clausePosition.
     */
    public int getLiteral(int position) {
        assert position >= 0 && position < cliterals.size();
        return cliterals.get(position).literal;}

    /**
     * converts the cLiterals into an array of integers
     *
     * @return the literals as integers.
     */
    public int[] getLiterals() {
        int size = cliterals.size();
        int[] literals = new int[size];
        for (int i = 0; i < size; ++i) {
            literals[i] = cliterals.get(i).literal;}
        return literals;}

    /**
     * checks if the literal is in the clause
     *
     * @param literal a literal
     * @return +1: the literal is in the clause, -1: the negated literal is in the clause, otherwise 0
     */
    public int contains(int literal) {
        return contains(literal, cliterals.size());}

    /**
     * checks if the literal is in the clause
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

    /**
     * adds a cliteral to the end of the clause, without checking for double literals and tautologies.
     *
     * @param cliteral the literal to be added.
     */
    public void add(CLiteral cliteral) {
        int position = cliterals.size();
        cliterals.add(cliteral);
        cliteral.setClause(this, position);
        setStructure();}

    /**
     * adds a new literal to the clause
     *
     * @param literal a literal
     */
    public void add(int literal) {
        int position = cliterals.size();
        CLiteral cliteral = new CLiteral(literal, this, position);
        cliterals.add(cliteral);
        setStructure();
    }

    /**
     * removes a cliteral from the clause.
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

    /**
     * replaces literals by the representatives in an equivalence class.
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
                if(connective == Connective.OR) {
                    clause.structure = ClauseStructure.TAUTOLOGY;
                    return clause;}}
            else {if(!removedFalseLiterals.contains(literal)) removedFalseLiterals.add(literal);}
            clause.removeAtPosition(i--);
            if(connective == Connective.OR) continue; // removing the clause is sufficient
            if(interval == null) continue;
            if(status == 1) clause.interval.decrement();}

        clause.interval.max = Math.min(clause.cliterals.size(),clause.interval.max);
        clause.setStructure();
        if(clause.structure == ClauseStructure.CONTRADICTORY ||
           clause.structure == ClauseStructure.TAUTOLOGY) return clause;
        clause.setConnective();
        return clause;}

    /** checks for multiple occurrences (in OR-clauses) and complementary literals
     *  Multiple occurrences of literals in OR-clauses are removed.<br>
     *  Complementary literals in OR-clauses are a tautology.<br>
     *  Complementary literals in other clause types are removed.<br>
     *  The clause's structure may become TAUTOLOGY or CONTRADICTORY
     *
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
        doubleLiterals.clear();
        complementaryLiterals.clear();
        Clause clause = this;
        ArrayList<CLiteral> cLits = cliterals;
        if(connective == Connective.OR) {
            for(int i = 0; i < cLits.size(); ++i) {
                int literal1 = cLits.get(i).literal;
                for(int j = 0; j < i; ++j) {
                    int literal2 = cLits.get(j).literal;
                    if(literal1 == literal2) {
                        if(!doubleLiterals.contains(literal1)) doubleLiterals.add(literal1);
                        if (clause == this && nextInt != null) {
                            clause = clone(nextInt.getAsInt());
                            cLits = clause.cliterals;}
                        clause.removeAtPosition(i--); continue;}
                    if(literal1 == -literal2) {
                        complementaryLiterals.add(Math.abs(literal1));
                        clause.structure = ClauseStructure.TAUTOLOGY;
                        return clause;}}}
            int size = cLits.size();
            if(size == 1) {clause.connective = Connective.AND; clause.interval = null; return clause;}
            clause.interval.max = size;
            return clause;}

        for(int i = 0; i < cLits.size(); ++i) { // only complementary literals can be removed
            int literal = cLits.get(i).literal;
            for(int j = 0; j < i; ++j) {
                if(literal == -cLits.get(j).literal) {
                    complementaryLiterals.add(Math.abs(literal));
                    if (clause == this && nextInt != null) {
                        clause = clone(nextInt.getAsInt());
                        cLits = clause.cliterals;}
                    clause.removeAtPosition(i); clause.removeAtPosition(j); i -= 2;
                    clause.interval.decrement();
                    break;}}}
        clause.setStructure();
        clause.setConnective();
        return clause;}

    /** removes all occurrences of the literal from the clause.
     * If nextInt != null, a new clause is created, otherwise the removal is destructive
     *
     * @param literal the literal to be removed
     * @param nextInt if != null, it provides the next id for the new clause
     * @param truth   if true then a true literal is removed, otherwise a false literal
     * @return null, if the clause became true, or the old or the new clause.
     */
    public Clause removeLiteral(int literal, IntSupplier nextInt,  boolean truth) {
        Clause clause = this;
        ArrayList<CLiteral> cLits = cliterals;
        for(int i = 0; i < cLits.size(); ++i) {
            if(literal == cLits.get(i).literal) {
                if(truth && connective == Connective.OR) return null;
                if(clause == this && nextInt != null) {
                    clause = clone(nextInt.getAsInt(),i,truth);
                    cLits = clause.cliterals;
                    --i;}}
            else {cLits.remove(i--); decrementInterval(truth,1);}}
        clause.setConnective();
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
        if(!interval.isSubset(clause2.interval)) return false;
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
     * @return the number of removed literals
     */
    public int removeDoubles() {
        int doubled = 0;
        for(int i = 0; i < cliterals.size()-1; ++i) {
            int literal = cliterals.get(i).literal;
            for(int j = i+1; j < cliterals.size(); ++j) {
                CLiteral cliteral = cliterals.get(j);
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
    public int[] overlaps(Clause clause) {
        if(!interval.equals(clause.interval)) return null;
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
        if(interval != null && connective != Connective.OR) st.append(interval).append(": ");
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
        Connective detConn = detConnective();
        if(connective != detConn) {
            errors.append(prefix).append("connective " + connective + " should be " + detConn+"\n");
            okay = false;}

        if(interval.max > size()) {
            errors.append(prefix).append("Interval " + interval.toString() + " is not between 0 and " + size()+"\n");
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

}

