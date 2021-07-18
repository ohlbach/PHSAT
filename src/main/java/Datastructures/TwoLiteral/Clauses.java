package Datastructures.TwoLiteral;

import Datastructures.Clauses.ClauseType;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import javafx.util.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.function.Consumer;

public class Clauses {
    ArrayList<Clause> clauses = new ArrayList<>();

    /** maps a literal to the clauses containing this literal. */
    HashMap<Integer,ArrayList<Clause>> clauseMap = new HashMap<>();

    /** This list keeps track of equivalences.
     *  Each entry is a pair: [literal1,literal2],origins
     *  where literal1 == literal2 holds and origins is the list of basic clause ids causing this equivalence.
     *  The equivalence is normalized such that literal1 > 0 and literal1 > abs(literal2).
     */
    private ArrayList<ArrayList<IntArrayList>> equivalenceClasses = null;

    /** This list keeps track of disjointness classes.
     *  Each entry is a pair: [literal1,...],origins
     *  where literal1 /= literal2 /= ... holds and origins is the list of basic clause ids causing this disjointness.
      */
    private ArrayList<Pair<IntArrayList,IntArrayList>> disjointnessClasses = new ArrayList<>();

    /** stores all initial and derived unit clauses.
     *  It maps  literals to origins
     */
    private HashMap<Integer,IntArrayList> unitClauses = new HashMap<>();

    /** This predicate is called with the origins when an inconsistency is detected. */
    public Consumer<IntArrayList> inconsistencyReporter = null;

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
            literal = clause.literal2;}
        clauses.add(clause);}

    /** adds a two-literal disjunction to the data structures and performs all simplifications and inferences.
     * If a contradiction is encountered the inconsistencyReporter is called and the method stops.
     *
     * @param disjunction
     */
    public void addDisjunction(int[] disjunction) {
        assert disjunction.length == 4;
        assert ClauseType.getType(disjunction[1]) == ClauseType.OR;
        try {
            Clause clause = new Clause(disjunction[2],disjunction[3],disjunction[0]);
            if(normalizeClause(clause) && !isSubsumed(clause)) insertClause(clause);}
        catch(Exception e) {} // contradiction encountered
    }

    /** adds a two-literal clause to the data structures and performs all simplifications and inferences.
     * If a contradiction is encountered the inconsistencyReporter is called and the method stops.
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @param origins the list of basic clause indices causing this clause.
     */
    public void addClause(int literal1, int literal2, IntArrayList origins) {
        try {
            Clause clause = new Clause(literal1, literal2, origins);
            if (normalizeClause(clause) && !isSubsumed(clause)) insertClause(clause);}
        catch (Exception e) {} // contradiction encountered
        }

    /** adds a basic clause conjunction to the data structures and performs all simplifications and inferences.
     * If a contradiction is encountered the inconsistencyReporter is called and the method stops.
     *
     * @param conjunction a basic clause conjunction.
     */
    public void addConjunction(int[] conjunction) {
        assert ClauseType.getType(conjunction[1]) == ClauseType.AND;
        assert conjunction.length > 2;
        IntArrayList origins = new IntArrayList(); origins.add(conjunction[0]);
        try {
            if(equivalenceClasses == null) {
                for(int i = 2; i < conjunction.length; ++i) addUnitClause(conjunction[i],origins);}
            else {
            for(int i = 2; i < conjunction.length; ++i) {
                Pair<Integer,IntArrayList> replaced = replaceByEquivalence(conjunction[i],origins);
                addUnitClause(replaced.getKey(),replaced.getValue());}}}
        catch(Exception e) {} // contradiction encountered
        }

    /** adds a basic clause equivalence to the data structures and performs all simplifications and inferences.
     * If a contradiction is encountered the inconsistencyReporter is called and the method stops.
     *
     * @param equivalence a basic clause equivalence.
     */
    public void addEquivalence(int[] equivalence) {
        assert ClauseType.getType(equivalence[1]) == ClauseType.EQUIV;
        assert equivalence.length > 3;
        IntArrayList origins = new IntArrayList();
        origins.add(equivalence[0]);
        try {
        }
        catch(Exception e) {} // contradiction encountered
    }

    private void addUnitClause(int literal, IntArrayList origins) throws Exception {
        IntArrayList orig = unitClauses.get(-literal);
        if(orig != null) {
            inconsistencyReporter.accept(joinOrigins(origins,orig));
            throw new Exception();}
        orig = unitClauses.get(literal);
        if(orig != null) {return;}
        unitClauses.put(literal,origins);
        removeEQClass(literal);
        ArrayList<Pair<Integer,IntArrayList>> units = disjointLiterals(literal,origins);
        }

    /** The literals in the clause are replaced by equivalent ones (if necessary).
     * If the clause becomes a unit clause then all replacements are done (destructively).
     *
     * @param clause a new clause
     * @return true if the clause survived (no tautology and no unit clause).
     */
    protected boolean normalizeClause(Clause clause) throws Exception {
        replaceEquivalentLiterals(clause);

        int literal1 = clause.literal1;
        int literal2 = clause.literal2;

        if(literal1 == -literal2) {return false;} // tautology

        if(literal1 == literal2) {                // merge
            addUnitClause(clause.literal1,clause.origins);
            return false;}

        IntArrayList origins = unitClauses.get(-literal1); //UR-Resolution
        if(origins != null) {
            addUnitClause(literal2,joinOrigins(clause.origins,origins));
            return false; }
        origins = unitClauses.get(-literal2);
        if(origins != null) {
            addUnitClause(literal1,joinOrigins(clause.origins,origins));
            return false; }
        return true;}


    private void insertClause(Clause clause) {
        Pair<Integer, IntArrayList> unit = replacementResolves(clause);
        if (unit != null) {
            clearUnitClause(unit.getKey(), unit.getValue());
        }
    }


    /** checks if the clause is subsumed by a unit clause or another clause in the list.
     *
     * @param clause two-literal clause to be tested for subsumption
     * @return true if the clause is subsumed by a unit clause or another clause.
     */
    private boolean isSubsumed(Clause clause) {
        int literal1 = clause.literal1;
        int literal2 = clause.literal2;
        return  unitClauses.get(literal1) != null ||
                unitClauses.get(literal2) != null ||
                findClause(literal1,literal2) != null;}

    /** searches for a clause with the given literals
     *
     * @param literal1
     * @param literal2
     * @return null or the clause with the literals.
     */
    protected Clause findClause(int literal1, int literal2) {
        ArrayList<Clause> clauses = clauseMap.get(literal1);
        if(clauses == null) {return null;}
        for(Clause clause : clauses) {
            if(literal2 == clause.literal1 || literal2 == clause.literal2) {return clause;} }
        return null;}

    /** checks if the clause is together with an existing clause denote an equivalence.
     *  a new clause p,q together with an old clause -p,-q mean p == -q
     *
     * @param clause two-literal clause to be tested for equivalence
     * @return true if the clause p,q  means that p == -q holds.
     */
    private boolean partOfEquivalence(Clause clause) {
        ArrayList<Clause> clauses = clauseMap.get(-clause.literal1);
        if(clauses == null) {return false;}
        int literal2 = -clause.literal2;
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
        for(int i = 1; i <= 2; ++i) {
            Clause cl = replacementResolves(literal1,literal2);
            if(cl != null) {
                IntArrayList newOrigins = clause.origins.clone();
                newOrigins.addAll(newOrigins.size(),cl.origins);
                return new Pair(literal1,newOrigins);}
            literal1 = clause.literal2;
            literal2 = clause.literal1;}
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

    /** The method computes all resolvents between the given clause and the other two-literal clauses.
     *  The data structures are not changed. Redundant resolvents are ignored.
     *  If the clauses are kept resolution complete, a single layer of resolvents is sufficient to keep
     *  the clauses further resolution complete.
     *
     * @param clause a new clause
     * @return the list of all resolvents with the given clause.
     */
    protected ArrayList<Clause> allResolvents(Clause clause) {
        ArrayList<Clause>  resolvents = new ArrayList<>();  resolvents.add(clause);
        int literal1 = clause.literal1;
        int literal2 = clause.literal2;
        for(int i = 1; i <= 2; ++i) {
            ArrayList<Clause>  parents = clauseMap.get(-literal1);
            if(parents != null) {
                for(Clause parent : parents) {
                    int literal3 = (literal1 == - parent.literal1) ? parent.literal2 : parent.literal1;
                    Clause resolvent = new Clause(literal2, literal3,joinOrigins(clause.origins,parent.origins));
                    if(!isSubsumed(resolvent)) resolvents.add(resolvent);}}
            literal1 = clause.literal2;
            literal2 = clause.literal1;}
        return resolvents;}


    /** replaces the literal by its equivalent literal (if necessary)
     *
     * @param literal the literal to be replaced
     * @param origins the list of basic clause indices causing this literal
     * @return a pair [replaced literal, joined origins]
     */
    protected Pair<Integer,IntArrayList> replaceByEquivalence(int literal, IntArrayList origins) {
        Pair<Integer, IntArrayList> eqv = toBeReplacedByEquivalent(literal);
        if(eqv == null) {return new Pair(literal,origins);}
        return new Pair(eqv.getKey(),joinOrigins(origins,eqv.getValue())); }

    /** replaces the two literals by the representatives of their equivalence class (if necessary)
     *
     * @param clause a new clause
     */
    protected void replaceEquivalentLiterals(Clause clause) {
        Pair<Integer, IntArrayList> eqv = toBeReplacedByEquivalent(clause.literal1);
        if (eqv != null) {
            clause.literal1 = eqv.getKey();
            clause.origins = joinOrigins(clause.origins, eqv.getValue());}

        eqv = toBeReplacedByEquivalent(clause.literal2);
        if (eqv != null) {
            clause.literal2 = eqv.getKey();
            clause.origins = joinOrigins(clause.origins, eqv.getValue());}}

    /** removes the equivalence class containing the literals
     *
     * @param literal a literal
     */
    protected void removeEQClass(int literal) {
        if(equivalenceClasses != null) {
            equivalenceClasses.removeIf((ArrayList<IntArrayList> eqClass) ->
                    eqClass.get(0).contains(literal) || eqClass.get(0).contains(-literal));}}


    /** checks if the literal must be replaced by an equivalent literal
     *
     * @param literal the literal to be checked
     * @return null (no replacement) or the pair [replaced literal, origins]
     */
    protected Pair<Integer,IntArrayList> toBeReplacedByEquivalent(int literal) {
        if(equivalenceClasses == null) {return null;}
        for(ArrayList<IntArrayList> eqv : equivalenceClasses) {
            IntArrayList literals = eqv.get(0);
            if(literal ==  literals.getInt(0)) {return new Pair( literals.getInt(1),eqv.get(1));}
            if(literal == -literals.getInt(0)) {return new Pair(-literals.getInt(1),eqv.get(1));}}
        return null;}

    /** inserts literal1 == literal2 into the list of equivalences.
     *  If literal2 == literal3 is already an equivalence then literal1 == literal2 is changed to
     *  literal1 == literal3.
     *
     * @param literal1
     * @param literal2
     * @param origins the list of basic clause numbers causing this equivalence
     */
    protected void insertEquivalence(int literal1, int literal2, IntArrayList origins) {
        if(Math.abs(literal1) < Math.abs(literal2)) {
            int dummy = literal1;
            literal1 = literal2; literal2 = dummy;}
        if(literal1 < 0) {literal1 = -literal1; literal2 = -literal2;}
        // the equivalence is now normalized.
        if(equivalenceClasses != null) {
            for(ArrayList<IntArrayList> eqv : equivalenceClasses) {
                IntArrayList literals = eqv.get(0);
                boolean found = false;
                if(literals.getInt(1) ==  literal1) {literals.set(1, literal2); found = true;}
                if(literals.getInt(1) == -literal1) {literals.set(1,-literal2); found = true;}
                if(found) {eqv.set(1,joinOrigins(eqv.get(1),origins)); break;}}}
        else {equivalenceClasses = new ArrayList<>();}

        IntArrayList newPair = new IntArrayList();
        newPair.add(literal1); newPair.add(literal2);
        ArrayList<IntArrayList> newEqv = new ArrayList<>();
        newEqv.add(newPair); newEqv.add(origins);
        equivalenceClasses.add(newEqv);}

    /** tries to find a triple of disjoint literals.
     *  Three clauses: p,q  and p,r and q,r mean that -p,-q,-r are disjoint
     *
     * @param clause a potential partner of the triple
     * @return null or a tuple [literal1,literal2,literal3], origins indicating disjoint literals
     */
    protected Pair<IntArrayList,IntArrayList> findDisjointness(Clause clause) {
        int literal1 = clause.literal1;
        int literal2 = clause.literal2;
        ArrayList<Clause> clauses = clauseMap.get(literal1);
        if(clauses == null) return null;
        for(Clause cl2 : clauses) {
            int literal3 = cl2.literal1 == literal1 ? cl2.literal2 : cl2.literal1;
            Clause cl3 = findClause(literal2, literal3);
            if(cl3 != null) {
                IntArrayList origins = joinOrigins(clause.origins,cl2.origins);
                origins = joinOrigins(origins,cl3.origins);
                IntArrayList disjoints = new IntArrayList();
                disjoints.add(-literal1); disjoints.add(-literal2); disjoints.add(-literal3);
                return new Pair(disjoints,origins);}}
        return null;}

    /** tries to extend disjointness classes.
     * Suppose there is a disjointness class -p,-q,-r
     * Suppose a new clause p,s arrives.
     * We need to find clauses q,s and r,s.
     * Together with p,s we can extend the class to -p,-q,-r,-s
     * 
     * @param clause a new clause
     * @return null or a pair [disjoint literals, origins]
     */
    protected Pair<IntArrayList,IntArrayList> extendDisjointness(Clause clause) {
        int literal1 = clause.literal1;
        int literal2 = clause.literal2;
        ArrayList<Clause> clauses = new ArrayList<>();
        for(Pair<IntArrayList,IntArrayList> disjoints : disjointnessClasses) {
            IntArrayList disjLiterals = disjoints.getKey();
            int lit1 = 0; int lit2 = 0;
            if(disjLiterals.contains(-literal1)) {lit1 = literal1; lit2 = literal2;}
            else{if(disjLiterals.contains(-literal2)) {lit1 = literal2; lit2 = literal1;}}
            if(lit1 == 0) continue;
            // now the disjointness class is a potential candidate for extension
            clauses.clear();  // we need to collect their origins.
            for(int literal :disjLiterals) {
                if(literal1 != -lit1) {
                    Clause cl = findClause(-literal,lit2);
                    if(cl == null) {lit1 = 0; break;}
                    clauses.add(cl);}}
            if(lit1 == 0) continue; // not enough clauses found
            disjLiterals.add(-lit2);
            IntArrayList origins = disjoints.getValue();
            clauses.add(clause);
            for(Clause cl : clauses) {
                for(int index : cl.origins) {
                    if(!origins.contains(index)) {origins.add(index);}}}
            return disjoints;}
        return null;}

    /** returns for a true literal the other true literals.
     *  Example: a disjointness class is p,q,r
     *  For a true literal q: -p,-r become true as well.
     *  The disjointness class containing q in this case is removed.
     *
     * @param literal a literal
     * @param origins the basic clause indices causing the literal to be true
     * @return null or a list of pairs: [true literal, combined origins]
     */
    protected ArrayList<Pair<Integer,IntArrayList>> disjointLiterals(int literal, IntArrayList origins) {
        if(disjointnessClasses == null) {return null;}
        ArrayList<Pair<Integer,IntArrayList>> trueLiterals = new ArrayList<>();
        disjointnessClasses.removeIf((Pair<IntArrayList,IntArrayList> disjoints) -> {
            IntArrayList disj = disjoints.getKey();
            if(disj.contains(literal)) {
                for(int lit : disj) {
                    if(lit != literal) {trueLiterals.add(new Pair(-lit,joinOrigins(origins,disjoints.getValue())));}}
                return true;}
            return false;});
        return trueLiterals;}

    /** joins the two lists and returns the joined list.
     * Double occurrences are avoided.
     *
     * @param origins1 a list of integers
     * @param origins2 a list of integers
     * @return the joined list.
     */
    protected IntArrayList joinOrigins(IntArrayList origins1, IntArrayList origins2) {
        IntArrayList origins = origins1.clone();
        for(int index : origins2) {
            if(!origins1.contains(index)) {origins.add(index);}}
        return origins;}

}
