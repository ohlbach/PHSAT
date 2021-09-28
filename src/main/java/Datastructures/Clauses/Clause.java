package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses;
import Utilities.Positioned;
import Utilities.Sizable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.*;

import static Utilities.Utilities.joinIntArrays;
import static Utilities.Utilities.sortIntArray;

/** A clause is just a list of CLiterals.
 * It may represent disjunctions, conjunctions or any other logical list structure.
 *
 * Created by ohlbach on 13.09.2018.
 */
public class Clause implements Iterable<CLiteral>, Positioned, Sizable {
    /** for identifying the clause */
    public int id;
    /** OR,DISJOINT, ... */
    public ClauseType clauseType;
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
    /** contains the list of basicClause ids which produced this clause */
    public IntArrayList origins = null;
    /** some auxiliary pointer */
    public Object aux = null;

    public Clause(int id, ClauseType clauseType) {
        this.id = id;
        this.clauseType = clauseType;
        cliterals = new ArrayList<CLiteral>();}

    /** constructs a clause
     *
     * @param id   the clause problemId
     * @parma clauseType  the clause's type
     * @param size  the estimated number of literals
     */
    public Clause(int id, ClauseType clauseType, int size) {
        this.id = id;
        this.clauseType = clauseType;
        cliterals = new ArrayList<CLiteral>(size);}

    /** constructs a new clause with given literals
     *
     * @param id        the id of the new clause
     * @parma clauseType  the clause's type
     * @param literals the list of literals
     */
    public Clause(int id, ClauseType clauseType, IntArrayList literals, IntArrayList origins) {
        this.id = id;
        this.clauseType = clauseType;
        this.origins = origins == null ? IntArrayList.wrap(new int[]{id}) : origins;
        cliterals = new ArrayList<>(literals.size());
        for(int i = 0; i < literals.size(); ++i) {
            cliterals.add(new CLiteral(literals.getInt(i),this,i));}
        setStructure();}

    /** constructs a new clause with given literals
     *
     * @param id        the id of the new clause
     * @parma clauseType  the clause's type
     * @param cLiterals the list of CLiterals
     */
    public Clause(int id, ClauseType clauseType, ArrayList<CLiteral> cLiterals) {
        this.id = id;
        this.clauseType = clauseType;
        for(int i = 0; i < cLiterals.size(); ++i) {
            cLiterals.get(i).setClause(this,i);}
        cliterals = cLiterals;
        setStructure();}


    /** generates a clause from a basicClause (which should be a disjunction)
     * double literals are removed
     * complementary literals cause the structure to be TAUTOLOGY
     *
     * @param id           the name of the clause
     * @parma clauseType  the clause's type
     * @param basicClause  a basic clause [number,type,literal1,...]
     */
    public Clause(int id, int[] basicClause) {
        this.id = id;
        clauseType = ClauseType.getType(basicClause[1]);
        int length = basicClause.length;
        cliterals = new ArrayList<>(length-2);
        for(int i = 2; i < length; ++i) {
            int literal = basicClause[i];
            cliterals.add(new CLiteral(literal,this,cliterals.size()));}
        origins = IntArrayList.wrap(new int[]{basicClause[0]});
        setStructure();}

    /** creates a new clause with the two literals
     *
     * @param id       the new id
     * @parma clauseType  the clause's type
     * @param literal1 a literal
     * @param literal2 a literal
     * @param origins the basic clause ids
     */
    public Clause(int id, ClauseType clauseType, int literal1, int literal2, IntArrayList origins) {
        this.id = id;
        this.clauseType = clauseType;
        this.origins = origins;
        cliterals = new ArrayList<>(2);
        cliterals.add(new CLiteral(literal1,this,0));
        cliterals.add(new CLiteral(literal2,this,1));
        setStructure();}

    /** creates a clone of the clause.
     * The origins, the timestamp and the aux values are not cloned.
     *
     * @param id the identifier for the new clone
     * @return the new clone
     */
    public Clause clone(int id) {
        Clause clause = new Clause(id,this.clauseType);
        for(CLiteral cLiteral : cliterals) {clause.add(cLiteral.literal);}
        return clause;}

    /** checks if the clause is positive, negative or mixed and sets the corresponding value for the structure.
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
        for(int i = 0; i < cliterals.size(); ++i) {
            int lit = cliterals.get(i).literal;
            if(lit ==  literal) {return +1;}
            if(lit == -literal) {return -1;}}
        return 0;}

    /** checks if the literal is in the clause (except cliteral)
     *
     * @param literal a literal
     * @param ignore  a cLiteral to be ignored.
     * @return +1: the literal is in the clause, -1: the negated literal is in the clause, otherwise 0
     */
    public int contains(int literal, CLiteral ignore) {
        for(int i = 0; i < cliterals.size(); ++i) {
            CLiteral cLiteral = cliterals.get(i);
            if(cLiteral == ignore) continue;
            int lit = cLiteral.literal;
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


    /** checks if the literals in this occur in clause2
     *
     * @param clause2 any other clause
     * @return true if the literals in this, except cLiteral also occur in clause2
     */
    public boolean isSubset(Clause clause2) {
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

    /** replaces literals by equivalent literals
     *
     * @param eqClasses         the equivalence classes
     * @param trackReasoning    controls management of origins
     * @return true if some literals have been changed.
     */
    public boolean replaceEquivalences(EquivalenceClasses eqClasses, boolean trackReasoning) {
        boolean changed = false;
        for(CLiteral cliteral : cliterals) {
            int oldLiteral = cliteral.literal;
            int literal = eqClasses.getRepresentative(oldLiteral);
            if(literal != oldLiteral) {
                changed = true;
                cliteral.literal = literal;
                if(trackReasoning) origins = joinIntArrays(origins,eqClasses.getOrigins(oldLiteral));}}
        return changed;}

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



    /** joins the origins to the clause's origins
     *
     * @param origins null or a list of basic clause ids
     */
    public void joinOrigins(IntArrayList origins) {
            this.origins = joinIntArrays(this.origins,origins);}

    /** joins the origins (Ids of the basicClauses which caused this clause)
     *
     * @param clauses a list of clauses
     * @param clause an additional clauses
     * @return the joined origins of all the clauses
     */
    public static IntArrayList joinOrigins(ArrayList<Clause> clauses, Clause clause) {
        IntArrayList origins = new IntArrayList();
        if(clause != null) joinIntArrays(origins,clause.origins);
        for(Clause clause1 : clauses) {
            joinIntArrays(origins,clause1.origins);}
        return origins;}

    /** checks if the two clauses overlap.
     *
     * @param clause a clause
     * @return +1 if they overlap with a literal, -1 if they overlap complementary, otherwise 0
     */
    public int overlaps(Clause clause) {
        for(CLiteral cLiteral1 : this) {
            int literal1 =cLiteral1.literal;
            for(CLiteral cLiteral2 : clause) {
                if(cLiteral2.literal ==  literal1) return +1;
                if(cLiteral2.literal == -literal1) return -1;}}
        return 0;}

    /** removes from an array of clauses all removed clauses
     *
     * @param clauses an array of clauses
     * @return an array of clauses with the removed clauses removed.
     */
    public static Clause[] removeRemovedClauses(Clause[] clauses) {
        int removed = 0;
        for(int i = 0; i < clauses.length; ++i) {if(clauses[i].isRemoved()) ++removed;}
        if(removed == 0) return clauses;
        Clause[] newClauses = new Clause[clauses.length-removed];
        int i = 0;
        for(Clause clause : clauses) {
            if(!clause.isRemoved()) newClauses[i++] = clause;}
        return newClauses;}

    /** computes the maximum width of the clause ids.
     *
     * @param clauses an array of clauses
     * @return the maximum width of the clause ids
     */
    public static int clauseNameWidth(Clause[] clauses) {
        int width = 0;
        for(Clause clause : clauses) {
            width = Math.max(width,Integer.toString(clause.id).length());}
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
            format.format("%-"+(width+clauseType.prefix.length())+"s: ", clauseType.prefix+id);}
        else st.append(clauseType.prefix+Integer.toString(id)+": ");
        int size = cliterals.size();
        for(int position = 0; position < size; ++position) {
            st.append(Symboltable.toString(cliterals.get(position).literal,symboltable));
            if(position < size-1) {st.append(clauseType.separator);}};
        return st.toString();}

    /** generates a string: clause-number: literals [origins]
     *
     * @param width       0 or the width of the id-part.
     * @param symboltable null or for mapping numbers to names
     * @return the clause as string
     */
    public String infoString(int width, Symboltable symboltable) {
        String st = toString(width,symboltable);
        if(origins != null) st += " " + sortIntArray(origins).toString();
        return st;}

    /** gets an iterator over the literals
     *
     * @return the iterator over the literals
     */
    @Override
    public Iterator<CLiteral> iterator() {
        return cliterals.iterator();}


    /** checks the clause and its literals for consistency.
     * If an inconsistency is detected then an error message is printed and the entire system stops.
     */
    public void check() {
        if(removed) {return;}
        for(int i = 0; i < cliterals.size(); ++i) {
            CLiteral cliteral = cliterals.get(i);
            Clause clause = cliteral.clause;
            if(clause == null) {
                System.out.println("Error in Clause.check: clause" + id + ", literal " +
                        cliteral.literal + " has no clause");
                System.exit(1);}
            if(clause != this) {
                System.out.println("Error in Clause.check: clause" + id + ", literal " +
                        cliteral.literal + " has a different clause " + clause.id);
                System.exit(1);}
            if(cliteral.clausePosition != i) {
                System.out.println("Error in Clause.check: clause" + id + ", literal " +
                        cliteral.literal + " has wrong position " + cliteral.clausePosition);
                System.exit(1);}}

        if(structure == null) {
            System.out.println("Error in Clause.check: clause" + id + " has no structure");
            System.exit(1);}

        if(structure != getStructure()) {
            System.out.println("Error in Clause.check: clause" + id + " has wrong structure: " + structure.toString());
            System.exit(1);}
        }

}
