package Datastructures.Clauses;


import Datastructures.Clauses.QuantifiedToCNF.InfAtleastToCNF;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Results.UnsatisfiableClause;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses.EquivalenceClasses;
import InferenceSteps.InferenceStep;
import InferenceSteps.InfInputClause;
import Utilities.Positioned;
import Utilities.Sizable;
import Utilities.Utilities;
import Utilities.DiophantineEquation;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.*;
import java.util.function.IntSupplier;
import java.util.function.IntUnaryOperator;

import static Utilities.Utilities.sortIntArray;

/** A clause is primarily a list of CLiterals.
 * OR-Clauses and ATLEAST-clauses have an addition parameter limit:<br>
 * 2:p,q,r  means atleast 2 literals must be true in order to make the clause true.<br>
 * Multiple occurrences of the same literal in a clause are represented by the
 * parameter multiplicity in CLiterals.
 *
 *
 * Created by ohlbach on 13.09.2018.
 */
public class Clause implements Iterable<CLiteral>, Positioned, Sizable {
    /** for identifying the clause*/
    public int id;

    /** the connective */
    public Connective connective;

    /** the left limit of the quantification interval */
    public short minLimit = 0;

    /** the right limit of the quantification interval */
    public short maxLimit = 0;

    /** the literals */
    public ArrayList<CLiteral> cliterals;

    /** indicates that the clause has been removed */
    public boolean removed = false;

    /** for sorting clauses, for example in a listPosition queue */
    public int listPosition = -1;

    /** positive, negative or mixed */
    public ClauseStructure structure = null;

    /** a timestamp to be used by corresponding algorithms */
    public int timestamp = 0;

    /** the reason for deriving the clause */
    public InferenceStep inferenceStep;

    /** constructs a new clause
     *
     * @param id its identifier */
    public Clause(int id) {
        this.id = id;
        cliterals = new ArrayList<>();
        inferenceStep = new InfInputClause(id);
    }

    /** constructs a clause with an empty list of literals.
     *
     * @param id       the clause problemId
     * @param minLimit    the quantification limit
     * @param size     the estimated number of literals
     */
    public Clause(int id, Connective connective, short minLimit, int size) {
        this.id = id;
        this.connective = connective;
        this.minLimit = minLimit;
        cliterals = new ArrayList<>(size);
        inferenceStep = new InfInputClause(id);
    }

    /** constructs a new clause with given literals
     *
     * @param id         the id of the new clause
     * @param connective the connective for the clause
     * @param minLimit      the quantification limit
     * @param literals   the list of literals
     */
    public Clause(int id, Connective connective, short minLimit, IntArrayList literals)   {
        assert connective != Connective.OR || minLimit == 1;
        this.id = id;
        this.connective = connective;
        this.minLimit = minLimit;
        inferenceStep = new InfInputClause(id);
        cliterals = new ArrayList<>(literals.size());
        for (int i = 0; i < literals.size(); ++i) cliterals.add(new CLiteral(literals.getInt(i),this,cliterals.size(),(short)1));
        }


    /** generates a clause from a inputClause
     * The constructor does not work for INTERVAL-type basic clauses.<br>
     * The new clause gets the same id as the basic clause.<br>
     * There is no semantic check.<br>
     * That means the limits must be okay (checked in BasicClauseList),<br>
     * and there msy still be multiple literals and tautologies in the clause.<br>
     * The limit of clauses where there is no limit (AND, EQUIV) is set to -1 <br>
     * Atmost-clauses like atmost 2 p,q,r are transformed into atleast 1 -p,-q,-r.<br>
     * Atleast-clauses like atleast 3 p,q,r are transformed into and p,q,r<br>
     * If the limit of ATLEAST-clauses is 1,the connective is set to OR.
     * The clause's structure is set to POSITIVE,NEGATIVE or MIXED.
     *
     * @param inputClause a basic clause [id,typenumber, limit, literal1,...]
     */
    public Clause(int[] inputClause) {
        connective = Connective.getConnective(inputClause[1]);
        assert(connective != null);
        id = inputClause[0];
        inferenceStep = new InfInputClause(id);
        int length = inputClause.length;
        int start = 0;
        switch (connective) {
            case OR:       minLimit = 1; start = 2; maxLimit = (short)(length - start); break;
            case AND:
            case EQUIV:    minLimit = 1; start = 2; break;
            case ATLEAST:  minLimit = (short)inputClause[2]; start = 3; maxLimit = (short)(length - start); break;
            case ATMOST:   minLimit = 0; maxLimit = (short)inputClause[2]; start = 3; break;
            case EXACTLY:  minLimit = (short)inputClause[2]; maxLimit = minLimit; start = 3; break;
            case INTERVAL: minLimit = (short)inputClause[2]; maxLimit = (short)inputClause[3]; start = 4; break;
            default: assert(false);} // should not happen.

        cliterals = new ArrayList<>(length - start);
        for (int i = start; i < length; ++i) add(inputClause[i],(short)1);}

    /** creates a new clause with the given literals
     * The constructor does not work for INTERVAL-type basic clauses.<br>
     * There is no semantic check.<br>
     * That means the limits must be okay (checked in BasicClauseList),<br>
     * and there msy still be multiple literals and tautologies in the clause.<br>
     * The limit of clauses where there is no limit (AND, EQUIV) is set to -1 <br>
     * Atmost-clauses like atmost 2 p,q,r are transformed into atleast 1 -p,-q,-r.<br>
     * Atleast-clauses like atleast 3 p,q,r are transformed into and p,q,r<br>
     * If the limit of ATLEAST-clauses is 1,the connective is set to OR.
     * The clause's structure is set to POSITIVE,NEGATIVE or MIXED.
     *
     * @param id         the new id
     * @param connective the clause's type (no INTERVAL-type)
     * @param literals   [limit] a list of literals
     */
    public Clause(int id, Connective connective, int... literals) {
        this.id = id;
        inferenceStep = new InfInputClause(id);
        this.connective = connective;
        int start = 0;
        switch (connective) {
            case OR:
            case AND:
            case EQUIV:   minLimit = 1; start = 0; break;
            case ATLEAST:
            case ATMOST:
            case EXACTLY: minLimit = (short)literals[0]; start = 1; break;
            default: assert(false);} // should not happen
        int length = literals.length;
        cliterals = new ArrayList<>(literals.length);
        for (int i = start; i < length; ++i) cliterals.add(new CLiteral(literals[i],this,cliterals.size(),(short)1));}

    /** Transforms an INTERVAL-clause into (usually) two ATLEAST-clauses.
     * Example: [2,4] p,q,r,s,t -> atleast 2 p,q,r,s,t and atleast 1 -p,-q,-r,-s,-t<br>
     * Special cases are [0,m] p,... (actuall atmost m p,...) <br>
     * and [n,k] p_1,..,p_k  (actually atleast n p_1,...) <br>
     * They generate only one clause.
     *
     * @param nextId     for determinining the clause id
     * @param basicClause a basic interval-clause
     * @return            one or two new clauses
     */
    public static ArrayList<Clause> intervalClause(IntSupplier nextId, int[] basicClause)  throws Unsatisfiable {
        Connective connective = Connective.getConnective(basicClause[1]);
        assert(connective == Connective.INTERVAL);
        ArrayList<Clause> clauses = new ArrayList<>();
        int min = basicClause[2];
        int max = basicClause[3];
        int length = basicClause.length - 4;
        int[] literals = new int[length+1];
        System.arraycopy(basicClause, 4, literals, 1, basicClause.length - 4);

        if(min == max) {
            literals[0] = min;
            Clause clause = new Clause(nextId.getAsInt(),Connective.EXACTLY,literals);
            clause.analyseSemantically(nextId,clauses);
            return clauses;}

        if(min == 0) {
            literals[0] = max;
            Clause clause = new Clause(nextId.getAsInt(),Connective.ATMOST,literals);
            clause = clause.analyseSemantically(nextId,clauses);
            if(clause.structure != ClauseStructure.TAUTOLOGY) clauses.add(clause);
            return clauses;}

        if(max == length) {
            literals[0] = min;
            Clause clause = new Clause(nextId.getAsInt(),Connective.ATLEAST,literals);
            clause = clause.analyseSemantically(nextId,clauses);
            if(clause.structure != ClauseStructure.TAUTOLOGY) clauses.add(clause);
            return clauses;}

        literals[0] = min;
        Clause clause = new Clause(nextId.getAsInt(),Connective.ATLEAST,literals);
        if(clause.structure != ClauseStructure.TAUTOLOGY) clauses.add(clause);

        literals[0] = max;
        clause = new Clause(nextId.getAsInt(),Connective.ATMOST,literals);
        if(clause.structure != ClauseStructure.TAUTOLOGY) clauses.add(clause);
        return clauses;}

    public Clause analyseSemantically(IntSupplier nextId, ArrayList<Clause> clauses) throws Unsatisfiable {
        clauses.clear();
        compactify();
        Clause clause = this;
        switch (connective) {
            case OR:
                clause = removeComplementaryLiterals(null);
                if (clause.structure == ClauseStructure.TAUTOLOGY) return clause;
                if (clause.size() == 1) {
                    clause.connective = Connective.AND;
                    return clause;}
                if (clause.size() == 0) throw new UnsatisfiableClause(clause);
                setPositiveNegative();
                return clause;
            case AND:
            case EQUIV:
                if(clause.size() <= 1) clause.structure = ClauseStructure.TAUTOLOGY;
                return clause;
            case ATMOST:
                Clause newClause = new Clause(nextId.getAsInt(), Connective.ATLEAST,
                        (short)(clause.expandedSize() - minLimit), clause.cliterals.size());
                ArrayList<CLiteral> newCLiterals = newClause.cliterals;
                for (CLiteral cLiteral : clause.cliterals)
                    newCLiterals.add(new CLiteral(-cLiteral.literal, newClause, newCLiterals.size(),
                            cLiteral.multiplicity));
                newClause.inferenceStep = new InfAtmostToAtleast(clause, newClause);
                clause = newClause;

            case ATLEAST:
                clause = clause.removeComplementaryLiterals(nextId);
                if (clause.structure == ClauseStructure.TAUTOLOGY ||
                        clause.connective != Connective.ATLEAST) return clause;
                int expandedSize = clause.expandedSize();
                if (minLimit > expandedSize) throw new UnsatisfiableClause(clause);
                if(minLimit == expandedSize) return clause.toAnd(nextId,+1);
                DiophantineEquation eq = getDiophantineEquation();
                int newLimit = eq.minSolution();
                if(minLimit != newLimit) {
                    if(nextId != null) {
                        newClause = clause.clone(nextId.getAsInt());
                        newClause.minLimit = (short)newLimit;
                        newClause.inferenceStep = new InfIncreasedLimit(clause,newClause);
                        clause = newClause;}
                    else {clause.minLimit = (short)newLimit;}
                    if(clause.minLimit == expandedSize) return clause.toAnd(nextId,+1);}
                int andId = nextId != null ? nextId.getAsInt() : -clause.id;
                Clause andClause = clause.getNeededLiterals(andId,eq);
                if(andClause != null) clauses.add(andClause);
                return clause;

            case EXACTLY:
                clause = clause.removeComplementaryLiterals(nextId);
                if (clause.structure == ClauseStructure.TAUTOLOGY) return clause;
                clause.setPositiveNegative();
                eq = getDiophantineEquation();
                if(!eq.isSolvable()) throw new UnsatisfiableClause(clause);
                andId = nextId != null ? nextId.getAsInt() : -clause.id;
                andClause = clause.getNeededLiterals(andId,eq);
                if(andClause != null) clauses.add(andClause);
                Clause atleastClause = clause.clone(nextId.getAsInt());
                atleastClause.connective = Connective.ATLEAST;
                Clause atmostClause = clause.clone(nextId.getAsInt());
                atmostClause.connective = Connective.ATMOST;
                clauses.add(atleastClause);
                clauses.add(atmostClause.toAtleast(nextId.getAsInt()));
                return null;
            default:
                assert (false);}
        return null;}


    /** generates a constrained diophantine equation from a clause.
     *  Example: atleast 5 p^3,q^4,r,s,t -> 3*x_1+4*x_2+y = 5 where<br>
     *  the x_i take 0 or 1, and y is constrained to 0 <= y <= 3 <br>
     *  If there is no solution then there is no model for the clause.
     *
     * @return the generated diophantine equation
     */
    private DiophantineEquation getDiophantineEquation() {
        IntArrayList multiplicities = new IntArrayList();
        int singletons = 0;
        for (CLiteral cLiteral : cliterals) {
            if (cLiteral.multiplicity > 1) multiplicities.add(cLiteral.multiplicity);
            else ++singletons;}
        return new DiophantineEquation(multiplicities, singletons, minLimit);
    }

    /** checks if the clause implies true literals.
     * Example: atleast 4: p^2,q^2,r enforces p and q to be true in order to get enough true literals
      *
     * @param andId      an id for the new and-clause (if negative then the clauses need not be copied)
     * @param eq      the diophantine equation which investigated the satisfiability of the clause
     * @return        null or an and-clause
     */
    protected Clause getNeededLiterals(int andId, DiophantineEquation eq) {
        IntArrayList needed = eq.needed(minLimit);
        if(needed == null) return null;
        Clause andClause = new Clause(andId,Connective.AND,(short)1,needed.size());
        int i = -1;
        IntArrayList positions = new IntArrayList(needed.size());
        for(CLiteral cLiteral : cliterals) {
            if(cLiteral.multiplicity > 1) {++i;}
            if(needed.contains(i)) {
                andClause.add(cLiteral.literal,(short)1);
                positions.add(cLiteral.clausePosition);}}
        if(andId > 0) andClause.inferenceStep = new InfNeededLiterals(this,andClause);
        return andClause;}



    /** creates a clone of the clause.
     * Only the literals themselves are cloned
     *
     * @param id the identifier for the new clone
     * @return the new clone
     */
    public Clause clone(int id) {
        Clause clause = new Clause(id, connective, minLimit, cliterals.size());
        for(int i = 0; i < cliterals.size(); ++i) {
            CLiteral cLiteral = cliterals.get(i);
            clause.cliterals.add(new CLiteral(cLiteral.literal,clause,i,cLiteral.multiplicity));}
        clause.structure = structure;
        return clause;}

    public void compactify() {
        for(int i = 0; i < cliterals.size(); ++i) {
            int literal = cliterals.get(i).literal;
            for(int j = 0; j < i; ++j) {
                CLiteral cLiteral = cliterals.get(j);
                if(literal == cLiteral.literal) {
                    if(cLiteral.multiplicity < minLimit) ++cLiteral.multiplicity;
                    removeAtPosition(i--);
                    break;}}}}


    /** turns an atleast-clause into an atmost-clause
     *
     * @param id the id for the new atmost-clause
     * @return a new atmost-clause equivalent to the atleast-clause.
     */
    public Clause toAtmost(int id) {
        assert connective == Connective.OR || connective == Connective.ATLEAST;
        Clause clause = new Clause(id,Connective.ATMOST,(short)(expandedSize()- minLimit),cliterals.size());
        for (CLiteral cLiteral : cliterals) {
            clause.add(-cLiteral.literal, cLiteral.multiplicity);}
        if(inferenceStep != null) clause.inferenceStep = new InfSwitchAtleastAtmost(this,clause);
        clause.structure = ClauseStructure.MIXED;
        switch(structure) {
            case POSITIVE: clause.structure = ClauseStructure.NEGATIVE; break;
            case NEGATIVE: clause.structure = ClauseStructure.POSITIVE;}
        return clause;}

    /** turns an atleast-clause into an atmost-clause
     *
     * @param id the id for the new atmost-clause
     * @return a new atmost-clause equivalent to the atleast-clause.
     */
    public Clause toAtleast(int id) {
        assert connective == Connective.ATMOST;
        Clause clause = new Clause(id,Connective.ATLEAST,(short)(expandedSize()- minLimit),cliterals.size());
        for (CLiteral cLiteral : cliterals) {
            clause.add(-cLiteral.literal, cLiteral.multiplicity);}
        if(inferenceStep != null) clause.inferenceStep = new InfSwitchAtleastAtmost(this,clause);
        clause.structure = ClauseStructure.MIXED;
        switch(structure) {
            case POSITIVE: clause.structure = ClauseStructure.NEGATIVE; break;
            case NEGATIVE: clause.structure = ClauseStructure.POSITIVE;}
        return clause;}

    /** turns a clause to an AND-clause.
     * The limit is set to a and the multiplicities are set to 1
     *
     * @param nextId for computing the next identifier
     * @param sign +1 or -1
     * @return either the  original clause (nextId == null) or a new AND-clause
     */
    public Clause toAnd(IntSupplier nextId, int sign) {
        if(nextId == null) {
            connective = Connective.AND;
            minLimit = 1;
            for(CLiteral cLiteral : cliterals) {
                cLiteral.literal *= sign;
                cLiteral.multiplicity = 1;}
            return this;}

        Clause clause = clone(nextId.getAsInt());
        clause.connective = Connective.AND;
        clause.minLimit = 1;
        for(CLiteral cLiteral : clause.cliterals) {
            cLiteral.literal *= sign;
            cLiteral.multiplicity = 1;}
        clause.inferenceStep = new InfToAnd(this,clause);
        return clause;}

    /** turns an atleast-clause into conjunctive normal form
     *
     * @param nextId it provides the next id for the new clause
     * @param trackReasoning if true then a corresponding inference step is attached to the new clauses
     * @return a list of OR-clauses as the conjunctive normal form of the atleast-clause
     */
    public ArrayList<Clause> toCNF(IntSupplier nextId, boolean trackReasoning)  throws Unsatisfiable {
        ArrayList<Clause> clauses = new ArrayList<>();
        if(connective == Connective.OR) {clauses.add(this); return clauses;}
        assert connective == Connective.ATLEAST;
        IntArrayList lits = toArray();
        boolean hasDoubles = lits.size() > cliterals.size();
        for(IntArrayList literals :
                Utilities.combinations(lits.size()- minLimit +1,lits,
                        hasDoubles,hasDoubles,hasDoubles)) {
            clauses.add(new Clause(nextId.getAsInt(), Connective.OR,(short)1,literals));}
        if(trackReasoning) {
            for(Clause orClause : clauses) {
                orClause.inferenceStep = new InfAtleastToCNF(this, orClause);}}
        return clauses;}

    private final ArrayList<Object> replacements = new ArrayList();

    /** replaces literals by the representatives in an equivalence class.
     * If nextInt != null then the replacement is done in a clone of the clause, otherwise in the original clause
     * Besides the replacements, nothing is changed.
     *
     * @param equivalenceClasses maps a literal to its representative in an equivalence class
     * @param nextId           null or a function that returns the next clause id for a clone of the clause
      * @return either the original clause or the clone with the replacements
     */
    public Clause replaceEquivalences(EquivalenceClasses equivalenceClasses, IntSupplier nextId)  throws Unsatisfiable  {
        replacements.clear();
        Clause clause = this;
        ArrayList<CLiteral> cLits = cliterals;
        for (int i = 0; i < cLits.size(); ++i) {
            CLiteral cLiterali = cLits.get(i);
            int oldLiteral = cLiterali.literal;
            int newLiteral = equivalenceClasses.getRepresentative(oldLiteral);
            if (oldLiteral == newLiteral) continue;
            replacements.add(oldLiteral);
            replacements.add(newLiteral);
            //replacements.add(equivalenceClasses.getEClause(oldLiteral).inferenceStep);
            if (clause == this && nextId != null) {
                clause = clone(nextId.getAsInt());
                cLits = clause.cliterals;
                cLiterali = cLits.get(i);}
            cLiterali.literal = newLiteral;}
        if (!replacements.isEmpty()) {
            clause.compactify();
            clause.setPositiveNegative();}
        if(clause != this) clause.inferenceStep = new InfEquivalenceReplacements(this,clause,replacements,equivalenceClasses);
        return clause;}

    private final IntArrayList removedTrueLiterals  = new IntArrayList(3);
    private final IntArrayList removedFalseLiterals = new IntArrayList(3);

    /** replaces all true and false literals in the clause.
     * If nextId != null then the replacements is done on a clone of the clause, otherwise on the clause itself.<br>
     * The clause may become a tautology or contradictory.<br>
     * In both cases the structure of the clause is set accordingly<br>
     * The connective of the clause is adjusted at the new situation.
     *
     * @param getTruthStatus maps a literal to +1 (true), 0 (undefined) or -1 (false)
     * @param nextInt        null or a supplier for the identifier for a clone of the clause
     * @return               either the original changed clause or a clone.
     */
    public Clause removeTrueFalseLiterals(IntUnaryOperator getTruthStatus, IntSupplier nextInt)  throws Unsatisfiable {
        removedTrueLiterals.clear();removedFalseLiterals.clear();
        Clause clause = this;
        ArrayList<CLiteral> cLits = cliterals;
        for (int i = 0; i < cLits.size(); ++i) {
            CLiteral cLiteral = cLits.get(i);
            int literal = cLiteral.literal;
            int status = getTruthStatus.applyAsInt(literal);
            if(status == 0) continue;
            if (clause == this && nextInt != null) {
                clause = clone(nextInt.getAsInt());
                cLits = clause.cliterals;
                cLiteral = cLits.get(i);}
            if(status == 1) {
                if(!removedTrueLiterals.contains(literal)) removedTrueLiterals.add(literal);
                clause.minLimit -= cLiteral.multiplicity;
                if(clause.minLimit <= 0) {
                    clause.structure = ClauseStructure.TAUTOLOGY;
                    return clause;}}
            else {if(!removedFalseLiterals.contains(literal)) removedFalseLiterals.add(literal);}
            clause.removeAtPosition(i--);}
        clause.setPositiveNegative();
        if(clause != this) clause.inferenceStep = new InfTrueFalseLiterals(this,clause,removedTrueLiterals,removedFalseLiterals);
        return clause;}

    private final IntArrayList complementaryLiterals = new IntArrayList();

    /** removes complementary literals from the clause.
     * If nextId != null then the clause is cloned before the literals are removed.
     * If the limit is reduced to 0 then the clause is marked as TAUTOLOGY
     *
     * @param nextId  null or a supplier for clause ids
     * @return the possibly shortened clause (original or clone)
     */
    public Clause removeComplementaryLiterals(IntSupplier nextId)  throws Unsatisfiable {
        assert (connective == Connective.OR || connective == Connective.ATLEAST);
        complementaryLiterals.clear();
        Clause clause = this;
        boolean removed = false;
        ArrayList<CLiteral> cliterals = clause.cliterals;
        for(int i = 0; i < cliterals.size(); ++i) {
            CLiteral cLiterali = cliterals.get(i);
            int literali = cLiterali.literal;
            for(int j = 0; j < i; ++j) {
                CLiteral cLiteralj = cliterals.get(j);
                if(literali == -cLiteralj.literal) {
                    removed = true;
                    if(clause == this && nextId != null) {
                        clause = clone(nextId.getAsInt());
                        cliterals = clause.cliterals;
                        cLiterali = cliterals.get(i);
                        cLiteralj = cliterals.get(j);
                    }
                    complementaryLiterals.add(Math.abs(literali));
                    short multiplicityi = cLiterali.multiplicity;
                    short multiplicityj = cLiteralj.multiplicity;
                    if(multiplicityi == multiplicityj) {
                        clause.removeAtPosition(i);
                        clause.removeAtPosition(j);
                        clause.minLimit -= multiplicityi;
                        i -= 2;
                        break;}
                    if(multiplicityi < multiplicityj) { // remove i
                        cLiteralj.multiplicity = (short)(multiplicityj-multiplicityi);
                        clause.removeAtPosition(i);
                        clause.minLimit -= multiplicityi;
                        i -= 1;
                        break;}
                    // remove j
                    cLiterali.multiplicity = (short)(multiplicityi-multiplicityj);
                    clause.removeAtPosition(j);
                    clause.minLimit -= multiplicityj;
                    i -= 1;
                    break;
                }}
            if(clause.minLimit <= 0) {clause.structure = ClauseStructure.TAUTOLOGY; return clause;}}
        if(!removed) return clause;
        if(clause != this) clause.inferenceStep =
                new InfComplementaryLiterals(this,clause,complementaryLiterals);
        clause.setPositiveNegative();
    return clause;}

    /** extracts OR-Clauses from the literals with multiplicities > 1.
     * Example: atleast 3 pp,qq,r<br>
     * Since there is only one r, atleast 2 pp,qq must hold <br>
     * This is turned into CNF and gets just p,q
     *
     * @param nextId for generating an id for the clauses
     * @return null or a list of OR-clauses.
     */
    public ArrayList<Clause> splitOffMultiples(IntSupplier nextId, boolean trackReasoning)  throws Unsatisfiable  {
        int singleCounter = 0;
        for(CLiteral cLiteral : cliterals){if(cLiteral.multiplicity == 1) ++singleCounter;}
        if(singleCounter >= minLimit) return null;
        IntArrayList literals = new IntArrayList();
        for(CLiteral cLiteral: cliterals) {
            short multiplicity = cLiteral.multiplicity;
            if(multiplicity == 1) continue;
            int literal = cLiteral.literal;
            for(int m = 0; m < multiplicity; ++m) literals.add(literal);}
        ArrayList<Clause> clauses = new ArrayList<>();
        for(IntArrayList lits :
                Utilities.combinations(literals.size()-(minLimit -singleCounter)+1,literals,
                        true,true,true)) {
            Clause clause = new Clause(nextId.getAsInt(), Connective.OR,(short)1,lits);
            if(trackReasoning) clause.inferenceStep = new InfExtractMultiples(this,clause);
            clauses.add(clause);}
        return clauses;}


    /** copies the literals of the clause to an IntArrayList
     *
     * @return the literals as IntArrayList
     */
    public IntArrayList toArray() {
        IntArrayList list = new IntArrayList(cliterals.size());
        for(CLiteral cLiteral : cliterals) {
            for(short m = 0; m < cLiteral.multiplicity; ++m) list.add(cLiteral.literal);}
        return list;}


    /** return the current number of literals
     *
     * @return the current number of literals */
    public int size() {
        return cliterals.size();}

    /** checks if the clause is empty
     *
     * @return true if the clause is empty. */
    public boolean isEmpty() {
        return cliterals.isEmpty();}

    /** computes the expanded size of the clause, with all multiple occurrences of literals counted
     *
     * @return  the expanded size of the clause
     */
    public int expandedSize() {
        int size = 0;
        for(CLiteral cLiteral : cliterals) size += cLiteral.multiplicity;
        return size;}


    /** determines for an ATLEAST- or OR-clause the clause's structure:
     * If the limit is too large: CONTRADICTORY<br>
     * If the limit is 0: TAUTOLOGY,<br>
     * If the limit is the clause's size then the connective is changed to AND, and the limit to -1<br>
     * otherwise if the clause has only positive literals (POSITIVE) or only negative literals (NEGATIVE)
     * or mixed signs (MIXED)
     *
     * @return the corresponding value for the structure.
     */
    public void setPositiveNegative() {
        int positive = 0;
        int negative = 0;
        for (CLiteral cLiteral : cliterals) {
            if (cLiteral.literal > 0) {++positive;}
            else {++negative;}}
        if (positive == 0) {structure = ClauseStructure.NEGATIVE; return;}
        if (negative == 0) {structure = ClauseStructure.POSITIVE; return;}
        structure = ClauseStructure.MIXED;}


    /** returns the list position, or -1
     *
     * @return the list position, or -1 */
    public int getPosition() {
        return listPosition;}

    /** sets the list position
     *
     * @param position a position within a list. */
    public void setPosition(int position) {
        listPosition = position;}


    /** checks if the clause has only positive literals
     *
     * @return true if the clause has only positive literals */
    public boolean isPositive() {
        return structure == ClauseStructure.POSITIVE;
    }

    /** checks if the clause has only negative literals
     *
     * @return true if the clause has only negative literals. */
    public boolean isNegative() {
        return structure == ClauseStructure.NEGATIVE;
    }

    /** gets the literal at the given clausePosition
     *
     * @param position a literal clausePosition
     * @return the cliteral at that clausePosition. */
    public CLiteral getCLiteral(int position) {
        assert position >= 0 && position < cliterals.size();
        return cliterals.get(position);
    }

    /** gets the literal at the given clausePosition
     *
     * @param position a literal clausePosition
     * @return the literal at that clausePosition.*/
    public int getLiteral(int position) {
        assert position >= 0 && position < cliterals.size();
        return cliterals.get(position).literal;}

    /** converts the cLiterals into an array of integers
     *
     * @return the literals as integers. */
    public int[] getLiterals() {
        int size = cliterals.size();
        int[] literals = new int[size];
        for (int i = 0; i < size; ++i) {literals[i] = cliterals.get(i).literal;}
        return literals;}

    /** checks if the literal is in the clause
     *
     * @param literal a literal
     * @return +1: the literal is in the clause, -1: the negated literal is in the clause, otherwise 0
     */
    public int contains(int literal) {
        return contains(literal, cliterals.size());}

    /** checks if the literal is in the clause
     *
     * @param literal a literal
     * @param end     where the iteration over the literals stops
     * @return +1: the literal is in the clause, -1: the negated literal is in the clause, otherwise 0
     */
    public int contains(int literal, int end) {
        for (int i = 0; i < end; ++i) {
            int lit = cliterals.get(i).literal;
            if (lit == literal)  {return +1;}
            if (lit == -literal) {return -1;}}
        return 0;}

    /** adds a cliteral to the end of the clause, without checking for double literals and tautologies.
     *
     * @param cliteral the literal to be added.
     */
    public void add(CLiteral cliteral) {
        int position = cliterals.size();
        cliterals.add(cliteral);
        cliteral.setClause(this, position);}

    /** adds a new literal to the clause
     * If the literal is already in the clause, its multiplicity is incremented,
     * but only up to minLimit, if minLimit > 0.
     *
     * @param literal a literal
     */
    public void add(int literal, short multiplicity) {
        for (CLiteral cLiteral : cliterals) {
            if (literal == cLiteral.literal) {
                cLiteral.multiplicity = (short) (multiplicity + cLiteral.multiplicity);
                if(minLimit > 0) cLiteral.multiplicity = (short)Math.min(minLimit,cLiteral.multiplicity);
                return;}}
        int position = cliterals.size();
        if(minLimit > 0) multiplicity = (short)Math.min(minLimit,multiplicity);
        CLiteral cLiteral = new CLiteral(literal, this, position,multiplicity);
        cliterals.add(cLiteral);}

    /** removes a cliteral from the clause.
     *
     * @param cLiteral the literal to be removed.
     */
    public void remove(CLiteral cLiteral) {
        removeAtPosition( cLiteral.clausePosition);
    }

    /** removes a cliteral at the given clausePosition from the clause.
     * Nothing else is done.
     *
     * @param position the clausePosition of the literal to be removed
     */
    public void removeAtPosition(int position) {
        int size = cliterals.size();
        assert position >= 0 && position < size;
        for (int pos = position; pos < size - 1; ++pos) {
            CLiteral nextliteral = cliterals.get(pos + 1);
            nextliteral.clausePosition = pos;
            cliterals.set(pos, nextliteral);}
        cliterals.remove(size - 1);}

    /** removes all multiplicities of the literal from the clause.
     * If nextInt != null, a new clause is created, otherwise the removal is destructive
     *
     * @param literal the literal to be removed
     * @param nextId  if != null, it provides the next id for the new clause
     * @param truth   if true then a true literal is removed, otherwise a false literal
     * @return        the old or the new clause.
     */
    public Clause removeLiteral(int literal, IntSupplier nextId,  boolean truth)  throws Unsatisfiable  {
        Clause clause = this;
        ArrayList<CLiteral> cLits = clause.cliterals;
        for(int i = 0; i < cLits.size(); ++i) {
            CLiteral cLiteral = cLits.get(i);
            if(literal == cLiteral.literal) {
                if(nextId != null) {clause = clone(nextId.getAsInt());}
                if(truth) clause.minLimit -= cLiteral.multiplicity;
                clause.removeAtPosition(i);
                clause.setPositiveNegative();
                break;}}
        return clause;}


    /** checks if the literal is in the clause (except cliteral)
     *
     * @param literal a literal
     * @param ignore  a cLiteral to be ignored.
     * @return +1: the literal is in the clause, -1: the negated literal is in the clause, otherwise 0
     */
    public int contains(int literal, CLiteral ignore) {
        for(CLiteral cLiteral : this) {
            if(cLiteral == ignore) continue;
            int lit = cLiteral.literal;
            if(lit ==  literal) {return +1;}
            if(lit == -literal) {return -1;}}
        return 0;}

    /** checks if the literals in this occur in clause2
     *
     * @param clause2 any other clause
     * @return true if the literals in this also occur in clause2
     */
    public boolean isSubset(Clause clause2) {
        if(minLimit < clause2.minLimit) return false;
        for(CLiteral cl : cliterals) {
            if(clause2.contains(cl.literal) <= 0) {return false;}}
        return true;}

    /** checks if the clause has double literals
     *
     * @return true if the clause has double literals.
     */
    public boolean hasDoubles() {
        for(CLiteral cLiteral : cliterals) {
            if(cLiteral.multiplicity > 1) return true;}
        return false;}

    /** checks if the clause has complementary literals, e.g. p and -p
     *
     * @return true if the clause has complementary literals.
     */
    public boolean hasComplementaries() {
        int size = cliterals.size();
        for(int i = 0; i < size-1; ++i) {
            int literal = cliterals.get(i).literal;
            for(int j = i+1; j < size; ++j) {
                if(literal == -cliterals.get(j).literal) {return true;}}}
        return false;}

    /** sets the removed flag */
    public synchronized void setRemoved() {
        removed = true;}

    /** returns the removed flag
     *
     * @return the removed flag*/
    public synchronized boolean isRemoved() {
        return removed;}

    /** checks if the two clauses overlap.
     *
     * @param clause a clause
     * @return [+1,literal] if they overlap with a literal, [-1,literal] if they overlap complementary, otherwise null
     */
    public int[] overlaps(Clause clause) {
        for(CLiteral cLiteral1 : cliterals) {
            int literal1 = cLiteral1.literal;
            for(CLiteral cLiteral2 : clause) {
                if(cLiteral2.literal ==  literal1) return new int[]{+1,literal1};
                if(cLiteral2.literal == -literal1) return new int[]{-1,literal1};}}
        return null;}

    /** returns the type-prefix with the clause id
     *
     * @return the type-prefix with the clause id
     */
    public String getName() {
        return connective.prefix+id;}

    /** computes the maximum width of the clause ids.
     *
     * @param clauses an array of clauses
     * @return the maximum width of the clause ids
     */
    public static int clauseNameWidth(Clause[] clauses) {
        int width = 0;
        for(Clause clause : clauses) {
            width = Math.max(width,clause.getName().length());}
        return width;}

    /** generates a string: clause-number: literals
     *
     * @return a string: clause-number: literals
     */
    public String toString() {
        return toString(0,null);}

    /** generates a string: clause-number: literals
     *
     * @return a string: clause-number: literals
     */
    public String toNumbers() {
        return toString(0,null);}

    /** generates a string: clause-number: literals
     *
     * @param width: 0 or the width of the id-part.
     * @param symboltable for mapping numbers to names
     * @return a string: clause-number: literals
     */
    public String toString(int width, Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        if(width > 0) {
            Formatter format = new Formatter(st, Locale.GERMANY);
            format.format("%-"+(width+ connective.prefix.length())+"s", getName()+":");}
        else st.append(connective.prefix+id+": ");
        switch(connective) {
            case OR:
            case AND:
            case EQUIV:    break;
            case ATLEAST:  st.append(minLimit).append(": "); break;
            case ATMOST:   st.append(maxLimit).append(": "); break;
            case INTERVAL: st.append("[").append(minLimit).append(",").append(maxLimit).append("]: ");}
        int size = cliterals.size();
        for(int position = 0; position < size; ++position) {
            CLiteral cLiteral = cliterals.get(position);
            st.append(cLiteral.toString(symboltable));
            if(position < size-1) {st.append(connective.separator);}}
        return st.toString();}

    /** generates a string: clause-number: literals [origins]
     *
     * @param width       0 or the width of the id-part.
     * @param symboltable null or for mapping numbers to names
     * @return the clause as string
     */
    public String infoString(int width, Symboltable symboltable) {
        String st = toString(width,symboltable);
        if(inferenceStep != null) st += " " + sortIntArray(inferenceStep.inputClauseIds()).toString();
        return st;}

    /** gets an iterator over the literals
     *
     * @return the iterator over the literals
     */
    @Override
    public Iterator<CLiteral> iterator() {
        return cliterals.iterator();}


    /** checks the clause and its literals for consistency.
     * Error messages are put into the StringBuilder
     *
     * @return true if the clause is okay
     */
    public boolean check(StringBuilder errors) {
        if(removed) {return true;}
        String prefix = "Clause " + id + ": ";
        boolean okay = true;

        if(minLimit > size()) {
            errors.append(prefix).append("Limit " + minLimit + " is not between 0 and " + size()+"\n");
            okay = false;}

        for(int i = 0; i < cliterals.size(); ++i) {
            CLiteral cliteral = cliterals.get(i);
            Clause clause = cliteral.clause;
            if(clause == null) {
                errors.append(prefix).append("Literal " + cliteral.literal + " has no clause");
                okay = false;}
            else {
                if(clause != this) {
                    errors.append(prefix).append("Literal " + cliteral.literal + " has a different clause " + clause.id + "\n");
                    okay = false;}}
            if(cliteral.clausePosition != i) {
                errors.append(prefix).append("Literal " + cliteral.literal +
                        " has wrong position " + cliteral.clausePosition + " instead of " + i + "\n");
                okay = false;}}

        return okay;}

    public ArrayList<CLiteral> getCliterals() {
        return cliterals;
    }
}

