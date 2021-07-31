package Datastructures.Theory;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import com.sun.istack.internal.Nullable;
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
 *
 * The datastructures are not optimized because the lists are usually very small.
 */
public class DisjointnessClass {
    /** The list of disjoint literals */
    public IntArrayList literals;

    /** The list of basic clause indices, which cause the disjointness.*/
    public IntArrayList origins;

    /** constructs a new disjointness class from a basic clause.
     * The literals in the basic clause might be replaced by representatives of their equivalence class.
     * The literals must not contain double literals or complementary literals
     *
     * @param literals a list of literals
     * @param origins the indices of the basic clauses causes the disjointness.
     */
    public DisjointnessClass(IntArrayList literals, IntArrayList origins) {
        this.literals = literals;
        this.origins = origins;}

    /** adds a literal which is disjoint to the remaining literals.
     * Adding a new literal overwrites the origins by the new origins which come from the new literal.
     *
     * @param literal  a new literal
     * @param origins  the list of basic clause indices which cause the disjointness between all literals.
     */
    public void addLiteral(int literal, IntArrayList origins) {
        literals.add(literal);
        this.origins = origins;}

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

    /** replaces the occurrence of literal by representative (which are equivalent)
     *
     * @param representative
     * @param literal
     * @param origin the basic clause ids which cause the equivalence
     * @return true if the class has been changed
     * @throws Unsatisfiable if representative now occurs twice in the class.
     */
    public boolean replaceEquivalence(int representative, int literal, IntArrayList origin) throws Unsatisfiable {
        int sign = 0;
        if(literals.contains(literal))  sign = 1;
        if(literals.contains(-literal)) sign = -1;
        if(sign == 0) return false;
        origins = joinIntArrays(origins,origin);
        if(literals.contains(sign*representative)) {
            throw new Unsatisfiable("replacing equivalent literal " + sign*literal +
                    " by " + sign*representative + "in " + toString() + " causes double literals.",
                    origins);}

        if(literals.contains(-sign*representative)) {
            literals.rem(sign*literal);}
        for(int i = 0; i < literals.size();++i) {
            if(literals.getInt(i) == sign*literal) {
                literals.set(i,sign*representative);
                break;}}
        return true;}



    /** turns the disjointness class into a string
     *
     * @param symboltable for mapping integers to symbols
     * @return the class as a comma separated string.
     */
    public String toString(@Nullable Symboltable symboltable) {
        return Symboltable.getLiteralNames(literals,symboltable);}

    /** turns the disjointness class into a string, together with the origins.
     *
     * @param symboltable for mapping integers to symbols
     * @return the class as a comma separated string.
     */
    public String infoString(@Nullable Symboltable symboltable) {
        return  Symboltable.getLiteralNames(literals,symboltable) + " @ " +
                Symboltable.getLiteralNames(origins,null);}



}
