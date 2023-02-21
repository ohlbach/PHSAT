package Datastructures.Clauses.QuantifiedToCNF;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.Connective;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.IntSupplier;

/** This class contains the methods for generating CNF-clauses from quantification type clauses.
 *  Only in exceptional cases this might be necessary.
 */
public class CNFTransformerAlt {
    private final IntSupplier nextId;
    private final boolean trackReasoning;

    /** constructs a new CNFTransformer
     *
     * @param nextId used for generating clause ids
     * @param trackReasoning controls the generation of inferenceSteps
     */
    public CNFTransformerAlt(boolean trackReasoning, IntSupplier nextId) {
        this.nextId = nextId;
        this.trackReasoning = trackReasoning;}

    /** turns one of the numeric clauses into conjunctive normal form
     *
     * @param clause     a numeric clause
     * @return           an ArrayList of OR-clauses
     */
    public ArrayList<Clause> toCNF(Clause clause)  throws Unsatisfiable {
        switch(clause.connective) {
            case ATLEAST: return atleastToCNF(clause);
            case ATMOST:  return atmostToCNF(clause);
           // case EXACTLY: return exactlyToCNF(clause);
           // case INTERVAL:return intervalToCNF(clause);
        }
        return null;}


    /** This method transforms an arbitrary quantified clause to CNF.
     *
     * @param clause any quantified clause
     * @return null (tautology) or a list of or-clauses
     */
    /*
    protected ArrayList<Clause> intervalToCNF(Clause clause) {
        IntArrayList cl = new IntArrayList(clause.size()+2);
        cl.add(clause.interval.min);
        cl.add(clause.interval.max);
        for(CLiteral cLiteral: clause) {cl.add(cLiteral.literal);}
        cl = simplify(cl); // remove complementary literals
        if(cl == null) return null;
        ArrayList<IntArrayList> cnf = intervalToCNF(cl);
        if(cnf == null) return null;
        ArrayList<Clause> clauses = new ArrayList<>();
        for(IntArrayList cla : cnf) {
            Clause orClause = new Clause(nextId.getAsInt(),Connective.OR,
                    new Interval(1,cla.size()), cla.size());
            for(int literal : cla) orClause.add(literal);
            clauses.add(orClause);}
        return clauses;}
*/
    /** This method provides a recursive call to the CNF-transformation algorithm
     *
     * @param clause a simplified representation of a clause: [min,max,literal_1,...]
     * @return null or a list of simplified clauses [literal_1,...]
     */
    /*
    private ArrayList<IntArrayList> intervalToCNF(IntArrayList clause) {
        if(clause.size() == 1) { // empty clause
            clause.clear();
            ArrayList<IntArrayList> clauses = new ArrayList<>();
            clauses.add(clause);
            return clauses;}
        int min = clause.getInt(0);
        int max = clause.getInt(1);
        int size = clause.size()-2;
        if(min == 0) {
            if(max == 0) {
                ArrayList<IntArrayList> clauses = new ArrayList<>();
                for(int i = 2; i < clause.size(); ++i) {
                    int literal = clause.getInt(i);
                    clauses.add(IntArrayList.wrap(new int[]{-literal}));}
                return clauses;}
            else {if(max == size) return null;}} // tautology
        if (min == 1 && max == size) { // is already an or-clause
            ArrayList<IntArrayList> clauses = new ArrayList<>();
            clause.removeInt(1); clause.removeInt(0);
            clauses.add(clause);
            return clauses;}

        if(min == size && max == size) {  //exactly n
            ArrayList<IntArrayList> clauses = new ArrayList<>();
            for(int i = 2; i < clause.size(); ++i) {
                int literal = clause.getInt(i);
                clauses.add(IntArrayList.wrap(new int[]{literal}));}
            return clauses;}

        int literal = clause.getInt(clause.size()-1);
        IntArrayList posRemoved = remove(literal,clause.clone());
        IntArrayList negRemoved = remove(-literal,clause);

        if(posRemoved == null && negRemoved == null) return null; // tautology

        ArrayList<IntArrayList> posCNF = null;
        if(posRemoved != null && !posRemoved.isEmpty()) posCNF = intervalToCNF(posRemoved);
        ArrayList<IntArrayList> negCNF = null;
        if(negRemoved != null && !negRemoved.isEmpty()) negCNF = intervalToCNF(negRemoved);

        if(posCNF == null && negCNF == null) return null;


        ArrayList<IntArrayList> clauses = new ArrayList<>();
        if(posRemoved == null) {
            if(negCNF == null) return null;
            for(IntArrayList negClause : negCNF) negClause.add(literal);
            return negCNF;}
        if(negRemoved == null) {
            if(posCNF == null) return null;
            for(IntArrayList posClause : posCNF) posClause.add(-literal);
            return posCNF;}

        if(posRemoved.isEmpty()) {
            IntArrayList negClause = IntArrayList.wrap(new int[]{-literal});
            if(posCNF != null) {posCNF.add(negClause); return posCNF;}
            clauses.add(negClause); return clauses;}
        if(negRemoved.isEmpty()) {
            IntArrayList posClause = IntArrayList.wrap(new int[]{literal});
            if(negCNF != null) {negCNF.add(posClause); return negCNF;}
            clauses.add(posClause); return clauses;}

        if(posCNF == null) {
            for(IntArrayList negClause : negCNF) negClause.add(literal);
            return negCNF;}

        if(negCNF == null) {
            for(IntArrayList posClause : posCNF) posClause.add(-literal);
            return posCNF;}

        for(IntArrayList posClause : posCNF) {
            if(isSubsumed(posClause,negCNF)) clauses.add(posClause);
            else {posClause = posClause.clone();
                    posClause.add(-literal); clauses.add(posClause);}}
        for(IntArrayList negClause : negCNF) {
            if(isSubsumed(negClause,posCNF)) clauses.add(negClause);
            else {negClause.add(literal); clauses.add(negClause);}}
        return clauses;}
*/
    private IntArrayList simplify(IntArrayList clause) {
        int min = clause.getInt(0);
        int max = clause.getInt(1);
        for(int i = 3; i < clause.size(); ++i) {
            int literal = clause.getInt(i);
            for(int j = 2; j < i; ++j) {
                if(literal == -clause.getInt(j)) {
                    clause.removeInt(i); clause.removeInt(j); i -= 2;
                    --min; --max;
                    break;}}}
        if(max < 0) {return null;} // contradiction
        clause.set(0,min);
        clause.set(1,max);
        return clause;}

    /** checks if the clauseis subumed by one of the given clauses
     *
     * @param clause   a simplified representation of a clause
     * @param clauses  a list of such clauses
     * @return true if clause is a superset of one of the given clauses
     */
    private boolean isSubsumed(IntArrayList clause, ArrayList<IntArrayList> clauses) {
        for(IntArrayList cl : clauses) {
            if(Utilities.isSubset(cl,clause)) return true;}
        return false;}

    private IntArrayList remove(int literal, IntArrayList clause) {
        int min = clause.getInt(0);
        int max = clause.getInt(1);
        for(int i = 2; i < clause.size(); ++i) {
            int lit = clause.getInt(i);
            if(lit == -literal) {clause.removeInt(i--);}}
        max = Math.min(max,clause.size()-2);
        if(max < min) {clause.clear(); clause.add(-1); return clause;} // empty clause

        for(int i = 2; i < clause.size(); ++i) {
            int lit = clause.getInt(i);
            if(lit == literal) {clause.removeInt(i--); --min; --max;}}
        min = Math.max(0,min);
        if(max < 0) {clause.clear(); clause.clear(); clause.add(-1); return clause;}
        if(min == 0 && max == clause.size()-2) return null; // tautology
        clause.set(0,min);
        clause.set(1,max);
        return clause;}

    /** turns an atleast-clause into conjunctive normal form
     *
     * @param clause an atleast-Clause
     * @return a list of OR-clauses as the conjunctive normal form of the atleast-clause
     */
    private ArrayList<Clause> atleastToCNF(Clause clause)  throws Unsatisfiable {
        assert clause.connective == Connective.ATLEAST;
        ArrayList<Clause> clauses = new ArrayList<>();
        boolean hasDoubles = clause.hasDoubles();
        for(IntArrayList literals :
                Utilities.combinations(clause.size()- clause.minLimit +1,clause.toArray(),
                        hasDoubles,hasDoubles,hasDoubles)) {
            clauses.add(new Clause(nextId.getAsInt(), Connective.OR,(short)1,literals));}
        if(trackReasoning) {
            for(Clause orClause : clauses) {
                orClause.inferenceStep = new InfAtleastToCNF(clause, orClause);}}
        return clauses;}

    /** turns an atmost-clause into conjunctive normal form
     * Example: atmost 2 p,q,r,s -> -p,-q,-r & -p,-q,-s & -p,-r,-s & -q,-r,-s
     *
     * @param clause an atmost-Clause
     * @return a list of OR-clauses as the conjunctive normal form of the atmost-clause
     */
    private ArrayList<Clause> atmostToCNF(Clause clause)  throws Unsatisfiable {
        assert clause.connective == Connective.ATMOST;
        ArrayList<Clause> clauses = new ArrayList<>();
        IntArrayList negLiterals = new IntArrayList();
        boolean hasDoubles = clause.hasDoubles();
        for(CLiteral cLiteral : clause) negLiterals.add(-cLiteral.literal);
        for(IntArrayList literals :
                Utilities.combinations(clause.minLimit +1,negLiterals,
                        hasDoubles,hasDoubles,hasDoubles)) {
            clauses.add(new Clause(nextId.getAsInt(), Connective.OR, (short)1,
                    literals));}
        if(trackReasoning) {
            for(Clause orClause : clauses) {
                orClause.inferenceStep = new InfAtmostToCNF(clause, orClause);}}
        return clauses;}


    /** turns an exactly-clause into conjunctive normal form
     *
     * @param clause an exactly-Clause
     * @return a list of OR-clauses as the conjunctive normal form of the exactly-clause
     */
    /*
    private ArrayList<Clause> exactlyToCNF(Clause clause) {
        assert clause.connective == Connective.EXACTLY;
        IntArrayList literals = clause.toArray();
        ArrayList<Clause> clauses = new ArrayList<>();
        boolean hasDoubles = clause.hasDoubles();
        for(IntArrayList posLiterals :
                Utilities.combinations(clause.size()- clause.interval.min +1,literals,
                        hasDoubles,hasDoubles,hasDoubles)) {
            clauses.add(new Clause(nextId.getAsInt(), Connective.OR, new Interval(1,posLiterals.size()),
                    posLiterals));}
        for(int i = 0; i < literals.size(); ++i) literals.set(i,-literals.getInt(i));
        for(IntArrayList negLiterals :
                Utilities.combinations(clause.interval.min +1,literals,
                        hasDoubles,hasDoubles,hasDoubles)) {
            clauses.add(new Clause(nextId.getAsInt(), Connective.OR,new Interval(1,negLiterals.size()),negLiterals));}

        if(trackReasoning) {
            for(Clause orClause :clauses) {
                orClause.inferenceStep = new InfExactlyToCNF(clause,orClause);}}
        return clauses;
    }
*/
}
