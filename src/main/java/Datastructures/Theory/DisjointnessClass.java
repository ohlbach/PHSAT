package Datastructures.Theory;

import it.unimi.dsi.fastutil.ints.IntArrayList;
import javafx.util.Pair;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

/** A disjointness class is a list of literal, e.g. p,q,r, with disjoint truth values.
 * If one of the literals is true, then all other ones must be false.
 * They all, however can be false.
 *
 * A disjointness class is either specified in the input clauses, or derived from two-literal clauses.<br>
 * To declare p,q,r as disjoint one needs to discover the two-literal clauses:<br>
 * -p,-q<br>
 * -p,-r<br>
 * -q,-r<br>
 */
public class DisjointnessClass {
    /** The list of disjoint literals */
    public IntArrayList literals;

    /** The list of basic clause indices, which cause the disjointness.
     * Example: a basic clause 5 declares p,q,r as disjoint (or xor)
     * The the origins are in all three cases [5].
     * If new clauses 6: -p,-s and 7: -q,-s and 8: -r,-s are discovered then s is added to the disjointness class with
     * origins: [5,6,7,8]
     */
    public ArrayList<IntArrayList> originsList;

    /** constructs a new disjointness class from a basic clause.
     * The literals in the basic clause might be replaced by representatives of their equivalence class.
     * The literals must not contain double literals or complementary literals
     *
     * @param literals a list of literals
     * @param origins the indices of the basic clauses causes this disjointness.
     */
    public DisjointnessClass(IntArrayList literals, IntArrayList origins) {
        this.literals = literals;
        originsList = new ArrayList<>();
        for(int l : literals) {originsList.add(origins);}}

    /** adds a literal which is disjoint to the remaining literals.
     *
     * @param literal  a new literal
     * @param origins  the list of basic clause indices which cause the dijointness with the remaining literals.
     */
    public void addLiteral(int literal, IntArrayList origins) {
        literals.add(literal);
        originsList.add(origins);}

    /** checks if the disjointness class contains the literal
     *
     * @param literal a literal
     * @return +1 if the literal is contained in the class, -1 of -literal is contained in the class, otherwise 0.
     */
    public int contains(int literal) {
        for(int i = 0; i < literals.size(); ++i) {
            int lit = literals.getInt(i);
            if(literal ==  lit) {return 1;}
            if(literal == -lit) {return -1;} }
        return 0; }

    /** If one of the literals in the disjointness class becomes true then all other ones become false.
     * This method returns the other true literals (negation of the false ones) together with the origins for their truth.
     *
     * @param literal a literal which became true
     * @param origs   the basic clause indices for the truth of the literal
     * @return null (literal not in class) or the list of pairs [true literal,origins for truth]
     */
    public ArrayList<Pair<Integer,IntArrayList>> derivedLiterals(int literal, IntArrayList origs) {
        int i = 0; int sign = 0;
        for(; i < literals.size(); ++i) {
            if(literal ==  literals.getInt(i)) {sign = 1; break;}
            if(literal == -literals.getInt(i)) {sign = -1; break;}}
        if(sign == 0) {return null;}
        if(sign == -1) {literals.removeInt(i); originsList.remove(i); return null;}

        ArrayList<Pair<Integer, IntArrayList>> derivedLits = new ArrayList<>(literals.size() - 1);
        for(int j=0; j < literals.size(); ++j) {
            if(j == i) {continue;}
            derivedLits.add(new Pair(-literals.getInt(j),joinIntArrays(origs,originsList.get(j)))); }
        return derivedLits;}

}
