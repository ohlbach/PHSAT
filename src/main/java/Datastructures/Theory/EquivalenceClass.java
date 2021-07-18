package Datastructures.Theory;

import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import javafx.util.Pair;

import java.util.HashMap;
import static Utilities.Utilities.addIntArray;
import static Utilities.Utilities.joinIntArrays;

/** This class manages equivalence classes of literals together with the origins of the equivalences.
 * Equivalences are normalized such that the representative of the equivalence class is the
 * smallest integer, and it is a positive literal.
 * It is assumed that all literals in clauses are replaced by the representative of the equivalence class.
 * Therefore the other liters can't occur anymore.
 */
public class EquivalenceClass {

    /** maps literals to the origins of the equivalence with the representative. */
    private HashMap<Integer, IntArrayList> origins;

    /** The smallest integer of the equivalence class. It is always positive */
    public int representative;

    /** Constructs a new equivalence class from a basic clause.
     *  The representative becomes the literal with the smallest absolut value.
     * @param clause a basic clause [id,type,literal1,...]
     */
    public EquivalenceClass(int[] clause) {
        IntArrayList orig = new IntArrayList(); orig.add(clause[0]);
        representative = clause[2];
        for(int i = 3; i < clause.length; ++i) {
            if(Math.abs(clause[i])< Math.abs(representative)) {representative = clause[i];}}
        int sign = Integer.signum(representative);
        for(int i = 3; i < clause.length; ++i) {
            if(clause[i] != representative) {origins.put(sign*clause[i],orig.clone());}}
        representative *= sign;}

    /** construct a new equivalence class literal1 = literal2.
     * The smallest literal becomes the representative.
     * If it is negative, both literals are negated.
     *
     * @param literal1
     * @param literal2
     * @param newOrigins the list of basic clause indices causing this equivalence.
     */
    public EquivalenceClass(int literal1, int literal2, IntArrayList newOrigins) {
        if(Math.abs(literal2) > Math.abs(literal1)) {
            int dummy = literal1; literal1 = literal2; literal2 = dummy;}
        if(literal1 < 0) {literal1 = -literal1; literal2 = -literal2;}
        representative = literal2;
        origins.put(literal1,newOrigins);}

    /** adds a literal which is supposed to be equivalent to one of the other literals in the equivalence class
     *  If the literal is smaller than the representative, it replaces the representative.
     *  If the literal contradicts one of the literals then  a non-null result is returned.
     *
     * @param literal     A literal which is equivalent with the representative of the class
     * @param newOrigins  the list of basic clause indices causing this equivalence.
     * @return null or a pair [literal,origins] which is a contradiction to the class.
     */
    public Pair<Integer,IntArrayList> addEquivalence(int literal, IntArrayList newOrigins) {
        if(literal == -representative) {return new Pair(literal,newOrigins);}
        IntArrayList orig = origins.get(-literal);
        if(orig != null) {return new Pair(literal,joinIntArrays(orig,newOrigins));}
        if(Math.abs(literal) < representative) {
            if(literal < 0) {
                HashMap<Integer,IntArrayList> oldOrigins = origins;
                origins = new HashMap<>();
                oldOrigins.forEach((Integer lit, IntArrayList origs) -> origins.put(-lit,addIntArray(origs,newOrigins)));
                representative = -representative;}
            else {origins.forEach((Integer lit, IntArrayList origins) -> addIntArray(origins,newOrigins));}
            origins.put(representative,newOrigins);
            representative = Math.abs(literal);}
        else {origins.put(literal,newOrigins);}
        return null;}

    /** adds an overlapping basic equivalence clause to the equivalence class.
     * A contradiction may occur if, for example, the clause p,-q is to be added to the class p,q.
     * In this case the pair [literal,origins] is returned.
     *
     * @param clause an equivalence clause
     * @return null or a pair [literal,origins] which is a contradiction to the class.
     */
    public Pair<Integer,IntArrayList> addOverlappingClause(int[] clause) {
        IntArrayList orig = new IntArrayList(); orig.add(clause[0]);
        int sign = 0;
        for(int i = 2; i < clause.length; ++i) {
            int literal = clause[i];
            if(origins.get(literal)  != null) {sign = 1; break;}
            if(origins.get(-literal) != null) {sign = -1; break;}}
        assert sign != 0;  // not overlapping
        for(int i = 2; i < clause.length; ++i) {
            int literal = sign * clause[i];
            if(origins.get(literal) != null) {continue;}
            if(origins.get(-literal) != null)  {return new Pair(literal,joinIntArrays(orig,origins.get(-literal)));}
            addEquivalence(literal,orig);}
        return null;}

    /** checks if the literal is part of the equivalence class
     *
     * @param literal any literal
     * @return +1 if literal is part of the class, -1 if -literal is part of the class, otherwise 0.
     */
    public int contains(int literal) {
        if(literal == representative) return 1;
        if(literal == -representative) return -1;
        if(origins.get(literal)  != null) return 1;
        if(origins.get(-literal) != null) return -1;
        return 0;}

    /** maps literals to their representative of the equivalence class.
     *
     * @param literal a literal
     * @return the literal or the representative of the literal's equivalence class.
     */
    public int getRepresentative(int literal) {
        IntArrayList orig = origins.get(literal);
        if(orig != null) {return representative;}
        orig = origins.get(-literal);
        if(orig != null) {return -representative;}
        return literal;}

    /** maps the literal to the origins of the equivalence with the representative of its class
     *
     * @param literal
     * @return null or the origins of the literal's equivalence with its representative.
     */
    public IntArrayList getOrigins(int literal) {
        IntArrayList orig = origins.get(literal);
        if(orig != null) {return orig;}
        orig = origins.get(-literal);
        if(orig != null) {return orig;}
        return null;}

    /** turns the equivalence class into a string "literal1 = literal2 = ... = representative"
     *
     * @return a string representation of the equivalence class.
     */
    public String toString() {return toString(null);}

    /** turns the equivalence class into a string "literal1 = literal2 = ... = representative"
     *
     * @param symboltable or null
     * @return a string representation of the equivalence class.
     */
    public String toString(Symboltable symboltable) {
        StringBuilder string = new StringBuilder();
        if(symboltable != null) {
            origins.forEach((Integer literal, IntArrayList origins) ->
                    string.append(symboltable.getLiteralName(literal) + " = "));
            string.append(symboltable.getLiteralName(representative));}
        else {origins.forEach((Integer literal, IntArrayList origins) ->
                    string.append(literal + " = "));
            string.append(Integer.toString(representative));}
        return string.toString();}


    /** turns the equivalence class into a string "literal1[origins] = literal2[origins] = ... = representative"
     *
     * @return a string representation of the equivalence class with the origins.
     */
    public String infoString() {return infoString(null);}


    /** turns the equivalence class into a string "literal1[origins] = literal2[origins] = ... = representative"
     *
     * @param symboltable or null
     * @return a string representation of the equivalence class with the origins.
     */
    public String infoString(Symboltable symboltable) {
        StringBuilder string = new StringBuilder();
        if(symboltable != null) {
            origins.forEach((Integer literal, IntArrayList origins) ->
                    string.append(symboltable.getLiteralName(literal) + "[").
                            append(origins.toString()).append("] = "));
            string.append(symboltable.getLiteralName(representative));}
        else {origins.forEach((Integer literal, IntArrayList origins) ->
                string.append(literal + "[").
                        append(origins.toString()).append("] = "));
        string.append(Integer.toString(representative));}
        return string.toString();}


}
