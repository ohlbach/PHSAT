package Datastructures.TwoLiteral;

import Datastructures.Clauses.ClauseType;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import javafx.util.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;

public class Clauses {
    ArrayList<Clause> clauses = new ArrayList<>();

    HashMap<Integer,ArrayList<Clause>> clauseMap = new HashMap<>();

    /** inserts the clause into the internal lists
     *
     * @param clause a new clause.
     */
    protected void integrateClause(Clause clause) {
        clauses.add(clause);
        int literal = clause.literal1;
        for(int i = 1; i <= 2; ++i) {
            ArrayList<Clause> clauses = clauseMap.get(literal);
            if(clauses == null) {clauses = new ArrayList<>(); clauseMap.put(literal, clauses);}
            clauses.add(clause);
            literal = clause.literal2;}}


    public void addDisjunction(int[] disjunction) {
        assert disjunction.length == 4;
        assert ClauseType.getType(disjunction[1]) == ClauseType.OR;
        Clause clause = new Clause(disjunction[2],disjunction[3],disjunction[0]);
        if(!isSubsumed(clause)) insertClause(clause);
    }

    public void addClause(int literal1, int literal2, IntArrayList origins) {
        Clause clause = new Clause(literal1, literal2, origins);
        if(!isSubsumed(clause)) insertClause(clause);}

    public void addUnitClause(int literal, IntArrayList origins) {

        }

    private void insertClause(Clause clause) {
        Pair<Integer, IntArrayList> unit = replacementResolves(clause);
        if (unit != null) {
            clearUnitClause(unit.getKey(), unit.getValue());
        }
    }


    /** checks if the clause is subsumed by another clause in the list.
     *
     * @param clause two-literal clause to be tested for subsumption
     * @return true if the clause is subsumed by another clause.
     */
    private boolean isSubsumed(Clause clause) {
        ArrayList<Clause> clauses = clauseMap.get(clause.literal1);
        if(clauses == null) {return false;}
        int literal2 = clause.literal2;
        for(Clause cl : clauses) {
            if(literal2 == cl.literal1 || literal2 == cl.literal2) {return true;}}
        return false;}

    /** tries a replacement resolution:  p,q and p,-q yields p
     *
     * @param clause a two-literal clause to be checked for replacement resolution
     * @return either null, or the pair [new unit literal, combined origins]
     */
    private Pair<Integer,IntArrayList> replacementResolves(Clause clause) {
        int literal1 = clause.literal1;
        int literal2 = clause.literal2;
        Clause cl = replacementResolves(literal1,literal2);
        if(cl != null) {
            IntArrayList newOrigins = clause.origins.clone();
            newOrigins.addAll(newOrigins.size(),cl.origins);
            return new Pair(literal1,newOrigins); }
        cl = replacementResolves(literal2,literal1);
        if(cl != null) {
            IntArrayList newOrigins = clause.origins.clone();
            newOrigins.addAll(newOrigins.size(),cl.origins);
            return new Pair(literal2,newOrigins); }
        return null;}

    /** searches for a partner clause for replacement resolution with the clause literal1,literal2.
     * literal1 would become a new unit clause.
     *
     * @param literal1
     * @param literal2
     * @return null or the other parent clause literal1,-literal2
     */
    private Clause replacementResolves(int literal1, int literal2) {
        ArrayList<Clause> clauses = clauseMap.get(literal1);
        if(clauses == null) {return null;}
        for(Clause clause : clauses) {
            if(literal2 == -clause.literal1 || literal2 == -clause.literal2) {return clause;}}
        return null;}

    /** clears up the internal data structures from a new unit clause, and computes the derived
     *   unit clauses.
     *   literal and -literal,lit yields lit as new unit clause.
     *   All two-literal clauses with the literal and its negation are removed from the clauses list,
     *   and from the clauseMap.
     *
     * @param literal  the unit clause
     * @param origins  the list of basic clause ids which caused the derivation of the new unit clause.
     * @return a list of pairs [lit,origins] which are the derived unit clauses
     */
    public ArrayList<Pair<Integer,IntArrayList>> clearUnitClause(int literal, IntArrayList origins) {
        ArrayList<Clause> clausesPos = clauseMap.get(literal);
        ArrayList<Clause> clausesNeg = clauseMap.get(-literal);
        if(clausesPos == null && clausesNeg == null) {return null;}
        clauseMap.remove(literal);
        clauses.removeIf((Clause clause) -> (
                 literal == clause.literal1 ||  literal == clause.literal2 ||
                -literal == clause.literal1 || -literal == clause.literal2));
        if(clausesNeg == null) {return null;}
        clauseMap.remove(-literal);
        ArrayList<Pair<Integer,IntArrayList>> newUnits = new ArrayList<>();
        for(Clause clauseNeg : clausesNeg) {
            if(literal == -clauseNeg.literal1) {
                IntArrayList newOrigins = origins.clone();
                newOrigins.addAll(newOrigins.size(),clauseNeg.origins);
                newUnits.add(new Pair(clauseNeg.literal2,newOrigins));
                continue;}
            if(literal == -clauseNeg.literal2) {
                IntArrayList newOrigins = origins.clone();
                newOrigins.addAll(newOrigins.size(),clauseNeg.origins);
                newUnits.add(new Pair(clauseNeg.literal1,newOrigins));}}
        return newUnits;
    }

    /** The method computes all direct and indirect resolvents between the given clause and the other two-literal clauses.
     *  The data structures are not changed.
     *
     * @param clause a new clause
     * @return the list of all direct and indirect resolvents with the given clause.
     */
    protected ArrayList<Clause> allResolvents(Clause clause) {
        ArrayList<Clause>  resolvents = new ArrayList<>();  resolvents.add(clause);
        LinkedList<Clause> clauses    = new LinkedList<>(); clauses.add(clause);
        while(!clauses.isEmpty()) {
            clause = clauses.removeFirst();
            int literal1 = clause.literal1;
            int literal2 = clause.literal2;
            ArrayList<Clause> parents;
            for(int i = 1; i <= 2; ++i) {
                parents = clauseMap.get(-literal1);
                if(parents != null) {
                    for(Clause parent : parents) {
                        IntArrayList origins = clause.origins.clone();
                        origins.addAll(origins.size(),parent.origins);
                        int literal3 = (literal1 == - parent.literal1) ? parent.literal2 : parent.literal1;
                        Clause resolvent = new Clause(literal2, literal3,origins);
                        clauses.add(resolvent); resolvents.add(resolvent);}}
                literal1 = clause.literal2;
                literal2 = clause.literal1;}}
        return resolvents;}
}
