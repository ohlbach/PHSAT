package Datastructures.Clauses.QuantifiedToCNF;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.Connective;
import Datastructures.Literals.CLiteral;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.IntSupplier;

/** This class contains the methods for generating CNF-clauses from quantification type clauses.
 *  Only in exceptional cases this might be necessary.
 */
public class CNFTransformer {
    private final IntSupplier nextId;
    private final boolean trackReasoning;

    /** constructs a new CNFTransformer
     *
     * @param nextId used for generating clause ids
     * @param trackReasoning controls the generation of inferenceSteps
     */
    public CNFTransformer(boolean trackReasoning, IntSupplier nextId) {
        this.nextId = nextId;
        this.trackReasoning = trackReasoning;}

    /** turns one of the numeric clauses into conjunctive normal form
     *
     * @param clause     a numeric clause
     * @return           an ArrayList of OR-clauses
     */
    public ArrayList<Clause> toCNF(Clause clause) {
        switch(clause.connective) {
            case ATLEAST: return atLeastToCNF(clause);
            case ATMOST:  return atmostToCNF(clause);
            case EXACTLY: return exactlyToCNF(clause);}
        return null;}

    /** turns an atleast-clause into conjunctive normal form
     *
     * @param clause an atleast-Clause
     * @return a list of OR-clauses as the conjunctive normal form of the atleast-clause
     */
    private ArrayList<Clause> atLeastToCNF(Clause clause) {
        assert clause.connective == Connective.ATLEAST;
        ArrayList<Clause> clauses = new ArrayList<>();
        boolean hasDoubles = clause.hasDoubles();
        for(IntArrayList literals :
                Utilities.combinations(clause.size()- clause.quAmount +1,clause.toArray(),
                        hasDoubles,hasDoubles,hasDoubles)) {
            clauses.add(new Clause(nextId.getAsInt(), Connective.OR,literals));}
        if(trackReasoning) {
            for(Clause orClause : clauses) {
                orClause.inferenceStep = new AtleastToCNF(clause, orClause);}}
        return clauses;}

    /** turns an atmost-clause into conjunctive normal form
     * Example: atmost 2 p,q,r,s -> -p,-q,-r & -p,-q,-s & -p,-r,-s & -q,-r,-s
     *
     * @param clause an atmost-Clause
     * @return a list of OR-clauses as the conjunctive normal form of the atmost-clause
     */
    private ArrayList<Clause> atmostToCNF(Clause clause) {
        assert clause.connective == Connective.ATMOST;
        ArrayList<Clause> clauses = new ArrayList<>();
        IntArrayList negLiterals = new IntArrayList();
        boolean hasDoubles = clause.hasDoubles();
        for(CLiteral cLiteral : clause) negLiterals.add(-cLiteral.literal);
        for(IntArrayList literals :
                Utilities.combinations(clause.quAmount +1,negLiterals,
                        hasDoubles,hasDoubles,hasDoubles)) {
            clauses.add(new Clause(nextId.getAsInt(), Connective.OR,literals));}
        if(trackReasoning) {
            for(Clause orClause : clauses) {
                orClause.inferenceStep = new AtmostToCNF(clause, orClause);}}
        return clauses;}


    /** turns an exactly-clause into conjunctive normal form
     *
     * @param clause an exactly-Clause
     * @return a list of OR-clauses as the conjunctive normal form of the exactly-clause
     */
    private ArrayList<Clause> exactlyToCNF(Clause clause) {
        assert clause.connective == Connective.EXACTLY;
        IntArrayList literals = clause.toArray();
        ArrayList<Clause> clauses = new ArrayList<>();
        boolean hasDoubles = clause.hasDoubles();
        for(IntArrayList posLiterals :
                Utilities.combinations(clause.size()- clause.quAmount +1,literals,
                        hasDoubles,hasDoubles,hasDoubles)) {
            clauses.add(new Clause(nextId.getAsInt(), Connective.OR,posLiterals));}
        for(int i = 0; i < literals.size(); ++i) literals.set(i,-literals.getInt(i));
        for(IntArrayList negLiterals :
                Utilities.combinations(clause.quAmount +1,literals,
                        hasDoubles,hasDoubles,hasDoubles)) {
            clauses.add(new Clause(nextId.getAsInt(), Connective.OR,negLiterals));}

        if(trackReasoning) {
            for(Clause orClause :clauses) {
                orClause.inferenceStep = new ExactlyToCNF(clause,orClause);}}
        return clauses;
    }

}
