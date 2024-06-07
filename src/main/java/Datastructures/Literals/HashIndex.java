package Datastructures.Literals;

import Datastructures.Clauses.Clause;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.function.Consumer;

/** This class maps predicates to an ArrayList of CLiterals (Literal occurrences in clauses)  */
public class HashIndex {

    /** maps predicates to an ArrayList of CLiterals */
    public HashMap<Integer, ArrayList<CLiteral>> hashIndex = new HashMap<>();

    /** adds a CLiteral to the index
     *
     * @param cLiteral a CLiteral
     */
    public void add(CLiteral cLiteral) {
        hashIndex.computeIfAbsent(cLiteral.literal, k -> new ArrayList<>()).add(cLiteral);}

    public void remove(CLiteral cLiteral) {
        ArrayList<CLiteral> cliterals = hashIndex.get(cLiteral.literal);
        if(cliterals != null) cliterals.remove(cLiteral);}

    /** returns the indexed CLiterals
     *
     * @param literal a literal
     * @return null or the CLiterals with this literal
     */
    public ArrayList<CLiteral> get(int literal) {
        return hashIndex.get(literal);}

    /** removes an entire clause from the index
     *
     * @param clause a clause
     */
    public void removeClause(Clause clause) {
        for(CLiteral cLiteral : clause.cliterals){
            remove(cLiteral);}}

    /** removes all clauses with this literal from the index
     *
     * @param literal a literal
     */
    public void removeClauses(int literal) {
        ArrayList<CLiteral> cliterals = hashIndex.get(literal);
        if(cliterals == null) return;
        hashIndex.remove(literal);
        for(CLiteral cLiteral : cliterals) removeClause(cLiteral.clause);}

    /** checks if the index is empty for this literal
     *
     * @param literal a literal
     * @return true if the index is empty for the literal
     */
    public boolean isEmpty(int literal) {
        ArrayList<CLiteral> cliterals = hashIndex.get(literal);
        return cliterals == null || cliterals.isEmpty();}

    /** applies the consumer to every CLiteral with the given literal
     *
     * @param literal a literal
     * @param consumer a consumer function to be applied to all CLiterals with the literal.
     */
    public void forEach(int literal, Consumer<CLiteral> consumer) {
        ArrayList<CLiteral> cliterals = hashIndex.get(literal);
        if(cliterals == null) return;
        for(CLiteral cLiteral : cliterals) consumer.accept(cLiteral);}

    /** sets the timestamp for all clauses with the given literal
     *
     * @param literal    a literal
     * @param timestamp  an integer
     */
    public void setTimestamp(int literal, int timestamp) {
        ArrayList<CLiteral> cliterals = hashIndex.get(literal);
        if(cliterals != null) {
            for(CLiteral cLiteral : cliterals) cLiteral.clause.timestamp = timestamp;}}

    /** finds the first CLiteral with the literal whose clause has the given timestamp
     *
     * @param literal   a literal
     * @param timestamp an integer
     * @return null or the first CLiteral with the literal whose clause has the given timestamp
     */
    public CLiteral findFirst(int literal, int timestamp) {
        ArrayList<CLiteral> cliterals = hashIndex.get(literal);
        if(cliterals == null)  return null;
        for(CLiteral cLiteral : cliterals) if(cLiteral.clause.timestamp == timestamp) return cLiteral;
        return null;}
}
