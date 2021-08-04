package Datastructures.Theory;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import com.sun.istack.internal.Nullable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import static Utilities.Utilities.addIntArray;

/** This class manages equivalence classes of literals together with the origins of the equivalences.
 * Equivalences are normalized such that the representative of the equivalence class is the
 * smallest integer, and it is a positive literal.
 * It is assumed that all literals in clauses are replaced by the representative of the equivalence class.
 * Therefore equivalence classes are disjoint.
 *
 * For an equivalence p = q = r, with p being the representative,
 * there may be an origin list o1 for q and an o2 for r.
 * o1 indicates the basic clause ids which allow to conclude p = q,
 * and o2 indicates the basic clause ids which allow to conclude p = r.
 *
 * The datastructures are not optimized because the lists are usually very small.
 */
public class EquivalenceClass {
    /** The smallest integer of the equivalence class. It is always positive */
    public int representative;

    /** the list of literals which are equivalent with the representative */
    public IntArrayList literals;

    /** the list of basic clause ids causing this equivalence */
    public IntArrayList origins;


    /** Constructs a new equivalence class from a preprocessed basic clause.
     *  The representative becomes the literal with the smallest absolut value.
     *  The origins may come from equivalence replacements.
     *
     * @param literals a preprocessed basic clause
     * @param origins  the list of basic clause ids for this equivalence
     */
    public EquivalenceClass(IntArrayList literals, IntArrayList origins) {
        this.origins = origins;
        representative = Integer.MAX_VALUE;
        for(int literal : literals) {
            if(Math.abs(literal) < Math.abs(representative)) {representative = literal;}}
        literals.rem(representative);
        if(representative < 0) {
            representative = -representative;
            for(int i = 0; i < literals.size(); ++i) {literals.set(i,-literals.getInt(i));}}
        this.literals = literals;}


    /** construct a new equivalence class literal1 = literal2.
     * The smallest literal becomes the representative.
     * If it is negative, both literals are negated.
     *
     * @param literal1
     * @param literal2
     * @param origins the list of basic clause indices causing this equivalence.
     */
    public EquivalenceClass(int literal1, int literal2, IntArrayList origins) {
        this.origins = origins;
        if(Math.abs(literal2) < Math.abs(literal1)) {
            int dummy = literal1; literal1 = literal2; literal2 = dummy;}
        if(literal1 < 0) {literal1 = -literal1; literal2 = -literal2;}
        representative = literal1;
        literals = new IntArrayList(1);
        literals.add(literal2);}

    /** adds a literal which is supposed to be equivalent to one of the other literals in the equivalence class
     *  If the literal is smaller than the representative, it replaces the representative.
     *  If the literal contradicts one of the literals then  an exception is thrown.
     *
     * @param literal     A literal which is equivalent with the representative of the class
     * @param origins  the list of basic clause indices causing this equivalence.
     * @throws Unsatisfiable if a contradiction is found.
     */
    public void addEquivalence(int literal, IntArrayList origins) throws Unsatisfiable {
        this.origins = addIntArray(this.origins,origins);
        if(literal == -representative || literals.contains(-literal)) {
            throw new Unsatisfiable(
                    "Wenn adding new literal " + literal + " to equivalence class: " + toNumbers() +
                            ":\nFound " + literal + " = " + representative, origins);}

        if(Math.abs(literal) < representative) {
            int sign = Integer.signum(literal);
            for(int i = 0; i < literals.size(); ++i) {literals.set(i,sign*literals.getInt(i));}
            literals.add(sign*representative);
            representative = Math.abs(literal);}
        else {literals.add(literal);}}


    /** checks if the literal is part of the equivalence class
     *
     * @param literal any literal
     * @return +1 if literal is part of the class, -1 if -literal is part of the class, otherwise 0.
     */
    public int contains(int literal) {
        if(literal ==  representative)  return  1;
        if(literal == -representative)  return -1;
        if(literals.contains(literal))  return  1;
        if(literals.contains(-literal)) return -1;
        return 0;}

    /** maps literals to their representative of the equivalence class.
     * Example: Class p = q,r <br>
     * The representative of q is p.<br>
     * The representative of -r is -p.<br>
     *
     * @param literal a literal
     * @return the literal or the representative of the literal's equivalence class.
     */
    public int getRepresentative(int literal) {
        if(literal ==  representative)  return  literal;
        if(literal == -representative)  return -literal;
        if(literals.contains(literal))  return  representative;
        if(literals.contains(-literal)) return -representative;
        return literal;}


    /** turns the equivalent literals into a =-separated string of names.
     *
     * @param symboltable null or a symboltable
     * @return the equivalent literals as a string
     */
    public String toString(@Nullable Symboltable symboltable) {
        String st = Symboltable.toString(representative,symboltable) + " = ";
        return st + Symboltable.toString(literals," = ",symboltable);}

    /** turns the equivalent literals into a =-separated string of numbers.
     *
     * @return the equivalent literals as a string
     */
    public String toNumbers() {
        return "" + representative + " = " + Symboltable.toString(literals," = ",null);}


    /** turns the equivalence class into a string "representative = literal1 = literal2 =  [origins]"
     *
     * @param symboltable or null
     * @return a string representation of the equivalence class with the origins.
     */
    public String infoString(@Nullable Symboltable symboltable) {
        StringBuilder string = new StringBuilder();
        string.append(Symboltable.toString(representative,symboltable));
        for(int i = 0; i < literals.size(); ++i) {
            string.append(" = ").append(Symboltable.toString(literals.getInt(i),symboltable));}
        if(origins != null) {
            string.append(" ").append(origins.toString());}
        return string.toString();}
}
