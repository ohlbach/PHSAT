package Datastructures.Theory;

import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import com.sun.istack.internal.Nullable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import javafx.util.Pair;

import java.util.ArrayList;
import static Utilities.Utilities.joinIntArrays;

/** This class manages equivalence classes of literals together with the origins of the equivalences.
 * Equivalences are normalized such that the representative of the equivalence class is the
 * smallest integer, and it is a positive literal.
 * It is assumed that all literals in clauses are replaced by the representative of the equivalence class.
 * Therefore equivalence classes are disjoint.
 *
 * The datastructures are not optimized because the lists are usually very small.
 */
public class EquivalenceClass {
    /** The smallest integer of the equivalence class. It is always positive */
    public int representative;

    /** the list of literals which are equivalent with the representative */
    public IntArrayList literals;

    /** for each literal which is equivalent with the representative:
     * the list of basic clause ids causing this equivalence */
    public ArrayList<IntArrayList> origins;


    /** Constructs a new equivalence class from a preprocessed basic clause.
     *  The representative becomes the literal with the smallest absolut value.
     * @param clause a preprocessed basic clause [[literal1,origins],...]
     */
    public EquivalenceClass(ArrayList<Pair<Integer,IntArrayList>> clause) {
        representative = Integer.MAX_VALUE;
        literals = new IntArrayList(clause.size()-1);
        origins = new ArrayList<>(clause.size()-1);
        for(Pair<Integer,IntArrayList> pair : clause) {
            if(Math.abs(pair.getKey()) < representative) {representative = pair.getKey();}}
        int sign = Integer.signum(representative);
        for(Pair<Integer,IntArrayList> pair : clause) {
            if(pair.getKey() != representative) {
                literals.add(sign*pair.getKey());
                origins.add(pair.getValue());}}
        representative = Math.abs(representative);}


    /** construct a new equivalence class literal1 = literal2.
     * The smallest literal becomes the representative.
     * If it is negative, both literals are negated.
     *
     * @param literal1
     * @param literal2
     * @param newOrigins the list of basic clause indices causing this equivalence.
     */
    public EquivalenceClass(int literal1, int literal2, IntArrayList newOrigins) {
        if(Math.abs(literal2) < Math.abs(literal1)) {
            int dummy = literal1; literal1 = literal2; literal2 = dummy;}
        if(literal1 < 0) {literal1 = -literal1; literal2 = -literal2;}
        representative = literal1;
        literals = new IntArrayList(1);
        origins  = new ArrayList<>(1);
        literals.add(literal2);
        origins.add(newOrigins);}

    /** adds a literal which is supposed to be equivalent to one of the other literals in the equivalence class
     *  If the literal is smaller than the representative, it replaces the representative.
     *  If the literal contradicts one of the literals then  a non-null result is returned.
     *
     * @param literal     A literal which is equivalent with the representative of the class
     * @param newOrigins  the list of basic clause indices causing this equivalence.
     * @throws Unsatisfiable if a contradiction is found.
     */
    public void addEquivalence(int literal, IntArrayList newOrigins) throws Unsatisfiable {
        if(literal == -representative || literals.contains(-literal)) {
            throw new Unsatisfiable(
                    "Wenn adding new literal " + literal + " to equivalence class" + toString() +
                            ": Found " + literal + " = " + representative, newOrigins);}

        if(Math.abs(literal) < representative) {
            int sign = Integer.signum(literal);
            for(int i = 0; i < literals.size(); ++i) {
                literals.set(i,sign*literals.getInt(i));
                origins.set(i,joinIntArrays(origins.get(i),newOrigins));}
            literals.add(representative); origins.add(newOrigins);
            representative = Math.abs(literal);}
        else {literals.add(literal); origins.add(newOrigins);}}


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

    /** maps the literal to the origins of the equivalence with the representative of its class
     *
     * @param literal
     * @return null or the origins of the literal's equivalence with its representative.
     */
    public IntArrayList getOrigins(int literal) {
        if(literal ==  representative)  return null;
        if(literal == -representative)  return null;
        int index = literals.indexOf(literal);
        if(index < 0 ) index = literals.indexOf(-literal);
        if(index >= 0) return origins.get(index);
        return null;}


    /** turns the equivalent literals into a =-separated string of names.
     *
     * @param symboltable null or a symboltable
     * @return the equivalent literals as a string
     */
      public String toString(@Nullable Symboltable symboltable) {
        return Symboltable.getLiteralNames(literals," = ",symboltable);}


    /** turns the equivalence class into a string "representative = literal1[origins1] = literal2[origins2]"
     *
     * @param symboltable or null
     * @return a string representation of the equivalence class with the origins.
     */
    public String infoString(@Nullable Symboltable symboltable) {
        StringBuilder string = new StringBuilder();
        string.append(Symboltable.getLiteralName(representative,symboltable));
        for(int i = 0; i < literals.size(); ++i) {
            string.append(" = ").append(Symboltable.getLiteralName(literals.getInt(i),symboltable));
            if(origins != null) {
                string.append("[").append(origins.get(i).toString()).append("],");}}
        return string.toString();}


}
