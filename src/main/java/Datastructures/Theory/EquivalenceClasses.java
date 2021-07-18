package Datastructures.Theory;

import Datastructures.Clauses.ClauseType;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import javafx.util.Pair;

import java.util.ArrayList;

public class EquivalenceClasses {

    private ArrayList<EquivalenceClass> equivalenceClasses = null;

    /** adds a basic equivalence clause to the equivalence classes.
     * A contradiction may occur, if for example p,-q is to be added to p,q.
     *  In this case a pair [contradicting literal, origins] is returned
     *
     * @param clause a basic equivalence clause
     * @return null or a pair [contradicting literal, origins]
     */
    public Pair<Integer, IntArrayList> addEquivalenceClause(int[] clause) {
        assert clause.length > 3;
        assert ClauseType.getType(clause[1]) == ClauseType.EQUIV;
        if(equivalenceClasses != null) {
            for(int i = 2; i < clause.length; ++i) {
                for(EquivalenceClass eqClass : equivalenceClasses) {
                    if(eqClass.contains(clause[i]) != 0) {
                        return eqClass.addOverlappingClause(clause);}}}}
        else {equivalenceClasses = new ArrayList<>();}
        equivalenceClasses.add(new EquivalenceClass(clause));
        return null;}

    /** add the equivalence literal1 = literal2 to the equivalence classes, either to an existing one or a new one is created.
     * Adding an equivalence p = -q to a class p = q causes a contradiction to be reported.
     *
     * @param literal1
     * @param literal2
     * @param origins  the indices of the basic clauses causes this equivalence.
     * @return null or a pair [contradicting literal, origin]
     */
    public synchronized Pair<Integer,IntArrayList> addEquivalence(int literal1, int literal2, IntArrayList origins) {
        if(equivalenceClasses == null) {
            equivalenceClasses = new ArrayList<>();
            equivalenceClasses.add(new EquivalenceClass(literal1,literal2,origins));
            return null;}
        for(EquivalenceClass eqClass : equivalenceClasses) {
            switch(eqClass.contains(literal1)) {
                case 1:  return eqClass.addEquivalence(literal2,origins);
                case -1: return eqClass.addEquivalence(-literal2,origins);}
            switch(eqClass.contains(literal2)) {
                case 1:  return eqClass.addEquivalence(literal1,origins);
                case -1: return eqClass.addEquivalence(-literal1,origins);}}
        equivalenceClasses.add(new EquivalenceClass(literal1,literal2,origins));
        return null;}

    /** maps literals to their representative in the equivalence class.
     *
     * @param literal a literal
     * @return the literal or the representative of the literal's equivalence class.
     */
    public synchronized int getRepresentative(int literal) {
        if(equivalenceClasses == null) {return literal;}
        for(EquivalenceClass eqClass : equivalenceClasses) {
            int representative = eqClass.getRepresentative(literal);
            if(representative != literal) {return representative;}}
        return literal;}

    /** maps a literal to the origins of the equivalence with the literal with its representative
     *
     * @param literal
     * @return null or the indices of the basic clauses causing this equivalence.
     */
    public synchronized IntArrayList getOrigins(int literal) {
        if(equivalenceClasses == null) {return null;}
        for(EquivalenceClass eqClass : equivalenceClasses) {
            IntArrayList origins = eqClass.getOrigins(literal);
            if(origins != null) {return origins;}}
        return null;}

    /** turns the equivalence classes into a string "literal1 = literal2 = ... = representative\n..."
     *
     * @return a string representation of the equivalence classes.
     */
    public String toString() {return toString(null);}

    /** turns the equivalence classes into a string "literal1 = literal2 = ... = representative\n..."
     *
     * @param symboltable or null
     * @return a string representation of the equivalence classes.
     */
    public String toString(Symboltable symboltable) {
        StringBuilder string = new StringBuilder();
        for(EquivalenceClass eqClass : equivalenceClasses) {
            string.append(eqClass.toString(symboltable)).append("\n");}
        return string.toString();}

    /** turns the equivalence classes into a string "literal1[origins] = literal2[origins] = ... = representative"
     *
     * @return a string representation of the equivalence class with the origins.
     */
    public String infoString() {return infoString(null);}


    /** turns the equivalence classes into a string "literal1[origins] = literal2[origins] = ... = representative"
     *
     * @param symboltable or null
     * @return a string representation of the equivalence class with the origins.
     */
    public String infoString(Symboltable symboltable) {
        StringBuilder string = new StringBuilder();
        for(EquivalenceClass eqClass : equivalenceClasses) {
            string.append(eqClass.infoString(symboltable)).append("\n");}
        return string.toString();}

    }
