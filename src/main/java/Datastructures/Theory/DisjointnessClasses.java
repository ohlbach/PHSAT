package Datastructures.Theory;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseType;
import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;
import Utilities.Utilities;
import Utilities.TriConsumer;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import java.util.*;
import java.util.function.BiConsumer;
import Utilities.BucketSortedIndex;
import Utilities.BucketSortedList;

/** A disjointness class is a set of literals which are pairwise contradictory.
 * Such a class may come from the input data, or be derived from binary clauses.
 * This class, however can deal only with disjoint predicates (positive literals).
 * Allowing mixed signs would cause the entire SAT-solving for binary clauses.
 * <br>
 * A disjointness like p,q,r is internally stored as a hash map:<br>
 *     p -> q,r <br>
 *     q -> p,r <br>
 *     r -> p,q <br>
 *  In addition the origins, i.e. the ids of the basicClauses defining the disjointnesses are stored.
 *  If there are equivalences then the literals are mapped to their representatives, and the origins of the
 *  equivalences are also stored.
 *
 * Created by Ohlbach on 21.09.2018.
 */
public class DisjointnessClasses {

    private EquivalenceClasses equivalenceClasses = null;

    private int predicates;

    /** disjointnesses maps literals to tuples [literals, origins]
     * origins is an ArrayList[IntArrayList]. literals and origins have the same size.<br>
     * Example: literals = 1,2,3 <br>
     *     origins = [[10,11], [8,11], [7,11]] <br>
     *     11 may be the id of the basic clause [11,3,5,6,7] <br>
     *     10, 8, 7 may be the ids of the basic clauses of equivalence type: 1 = 5, 2 = 6, 3 = 7, <br>
     *      which where used to replace 5,6,7 by their representatives.
     * */
    private HashMap<Integer,Object[]> disjointnesses = null;


    private ArrayList<Clause> disjointnessClasses = null;

    BucketSortedIndex<CLiteral> literalIndex = null;


    /** for mapping predicate numbers to their names */
    private Symboltable symboltable = null;

    /** Disjointnesses like p != p imply -p, which are sent back to the main solver by
     * unaryClauseHandler.accept(literal,origin) */
    BiConsumer<Integer,IntArrayList> unaryClauseHandler = null;

    /** disjointnesses with negative literals like p != -q cause SAT-Solving for binary clauses.
     * These must be handled in the main Solver. If they are submitted to this class, they are sent back to the solver
     * using binaryClauseHandler.accept(literal1, literal2, origins) */
    TriConsumer<Integer,Integer,IntArrayList> binaryClauseHandler = null;


    Model model = null;


    /** creates an empty disjointness class
     *
     * @param symboltable         for mapping predicate ids to their names (or null)
     * @param unaryClauseHandler  treats p != p by unaryClauseHandler.accept(-p,origin)
     * @param binaryClauseHandler treats p != -q by binaryClauseHandler.accept(-p, q, origins)
     */
    public DisjointnessClasses(Symboltable symboltable, Model model, EquivalenceClasses equivalenceClasses,
                               BiConsumer<Integer,IntArrayList> unaryClauseHandler,
                               TriConsumer<Integer,Integer,IntArrayList> binaryClauseHandler) {
        this.symboltable = symboltable;
        this.model = model;
        this.predicates = model.predicates;
        disjointnessClasses = new ArrayList<>();
        literalIndex = new BucketSortedIndex<CLiteral>(predicates+1,
                (cLiteral->cLiteral.literal),
                (cLiteral->cLiteral.clause.size()));
        this.equivalenceClasses = equivalenceClasses;
        this.unaryClauseHandler = unaryClauseHandler;
        this.binaryClauseHandler = binaryClauseHandler;}


    /** copies an existing DisjointnessClass into the local data structures, but with new handlers.
     *
     * @param disClass            a previously generated disjointness class
     * @param unaryClauseHandler  a new handler
     * @param binaryClauseHandler a new handler
     */
    public DisjointnessClasses(DisjointnessClasses disClass,
                               BiConsumer<Integer,IntArrayList> unaryClauseHandler,
                               TriConsumer<Integer,Integer,IntArrayList> binaryClauseHandler) {
        symboltable = disClass.symboltable;
        this.unaryClauseHandler = unaryClauseHandler;
        this.binaryClauseHandler = binaryClauseHandler;
        disjointnesses = new HashMap<>();
        disClass.disjointnesses.forEach((predicate, object) -> {
            IntArrayList predicates = (IntArrayList)object[0];
            ArrayList<IntArrayList> origins = (ArrayList<IntArrayList>)object[0];
            ArrayList<IntArrayList> thisOrigins = new ArrayList<>();
            for(IntArrayList list : origins) {thisOrigins.add(list.clone());}
            disjointnesses.put(predicate, new Object[]{predicates,thisOrigins});});}




    /** turns a basicClause into a disjointness class. <br>
     *  Before treating the literals, they are mapped to their representatives in the equivalence classes (if necessary) <br>
     *  The resulting clause may contain literals with mixed sign. <br>
     *  These are mapped back to the solver by calling the binaryClauseHandler. <br>
     *  The resulting clause may also contain literals p,p which imply -p. <br>
     *  These are mapped back to the solver by calling the unaryClauseHandler.
     *
     * @param basicClause        [clause-problemId,typenumber,literal1,...]
     * @param equivalenceClasses null or some equivalence classes.
     * @return true if the clause caused a change, otherwise false.
     */
    public boolean addDisjointnessClass(int[] basicClause, EquivalenceClasses equivalenceClasses) {
        assert basicClause.length > 3;
        assert basicClause[1] == ClauseType.DISJOINT.ordinal() || basicClause[1] == ClauseType.XOR.ordinal();
        Object[] result = analyseClause(basicClause,equivalenceClasses); // [IntArrayList of literals, ArrayList[IntArrayList] of origins]
        if(result == null) {return false;}
        IntArrayList            literals = (IntArrayList)result[0];
        ArrayList<IntArrayList> origins  = (ArrayList<IntArrayList>)result[1];
        if(disjointnesses == null) {disjointnesses = new HashMap<>();}
        for(int literal : literals) {
            result = disjointnesses.get(literal);
            if(result == null) {
                IntArrayList joinedLiterals = literals.clone();
                ArrayList<IntArrayList> joinedOrigins = Utilities.deepClone(origins);
                removeDuplicates(literal,joinedLiterals,joinedOrigins);
                disjointnesses.put(literal,new Object[]{joinedLiterals,joinedOrigins});}
            else {
                IntArrayList joinedLiterals = ((IntArrayList)result[0]);
                joinedLiterals.addAll(literals);
                ArrayList<IntArrayList> joinedOrigins = (ArrayList<IntArrayList>)result[1];
                joinedOrigins.addAll(origins);
                removeDuplicates(literal,joinedLiterals,joinedOrigins);
                disjointnesses.put(literal,new Object[]{joinedLiterals,joinedOrigins});}}
        return true;}

    /** This method can be called when a new disjointness clause -p,-q is derived, which means than p and q are disjoint.
     *  Both literals must be positive, different, and replaced by their representative in an equivalence class.
     *
     * @param predicate1  a positive literal
     * @param predicate2  a positive literal
     * @param origins     the origins of the derived disjointness clause.
     */
    public void addDisjointnessClass(int predicate1, int predicate2, IntArrayList origins) {
        assert predicate1 > 0;
        assert predicate2 > 0;
        assert predicate1 != predicate2;
        if(disjointnesses == null) {disjointnesses = new HashMap<>();}

        ArrayList<IntArrayList> originList = new ArrayList<>();
        originList.add(origins);
        Object[] result = disjointnesses.get(predicate1);
        if(result == null) {
            disjointnesses.put(predicate1,new Object[]{IntArrayList.wrap(new int[]{predicate2}),originList});}
        else {
            IntArrayList joinedLiterals = ((IntArrayList)result[0]);
            joinedLiterals.add(predicate2);
            ArrayList<IntArrayList> joinedOrigins = (ArrayList<IntArrayList>)result[1];
            joinedOrigins.add(origins);
            removeDuplicates(0,joinedLiterals,joinedOrigins);
            disjointnesses.put(predicate1,new Object[]{joinedLiterals,joinedOrigins});}

        result = disjointnesses.get(predicate2);
        if(result == null) {disjointnesses.put(predicate2,new Object[]{IntArrayList.wrap(new int[]{predicate1}),originList});}
        else {
            IntArrayList joinedLiterals = ((IntArrayList)result[0]);
            joinedLiterals.add(predicate1);
            ArrayList<IntArrayList> joinedOrigins = (ArrayList<IntArrayList>)result[1];
            joinedOrigins.add(origins);
            removeDuplicates(0,joinedLiterals,joinedOrigins);
            disjointnesses.put(predicate2,new Object[]{joinedLiterals,joinedOrigins});}}

    /** This method removes predicate from predicates, together with all duplicates.
     * In parallel it removes the origins from the origins list. <br>
     * When duplicates are removed, the shortest origins are kept. <br>
     * In the normal case, there should be no duplicates. <br>
     * There are only duplicates if the basicClause contains duplicates, or if equivalent literals are mapped
     * to the same predicate.
     *
     * @param predicate  a predicate (or 0)
     * @param predicates some predicates
     * @param origins    the origins for each predicate
     */
    static void removeDuplicates(int predicate, IntArrayList predicates, ArrayList<IntArrayList> origins) {
        boolean removed = false;
        for(int i = 0; i < predicates.size(); ++i) {
            int literali = predicates.getInt(i);
            if(literali == predicate) {
                predicates.removeInt(i);
                origins.remove(i--);}
            else {
                IntArrayList originsi = origins.get(i);
                for(int j = i+1; j < predicates.size(); ++j) {
                    int literalj = predicates.getInt(j);
                    if(literali == literalj) {
                        removed = true;
                        IntArrayList originsj = origins.get(j);
                        if(originsj == null) {predicates.removeInt(j); origins.remove(j--); continue;}
                        if(originsi == null) {predicates.removeInt(i); origins.remove(i--); break;}
                        if(originsi.size() <= originsj.size()) {
                            predicates.removeInt(j);
                            origins.remove(j);}
                        else {predicates.removeInt(i); origins.remove(i--);}
                        break;}}}}
        if(removed) removeDuplicates(0,predicates,origins);}


    /** analyses a disjointness basicClause and handles the following cases:
     *  - If equivalenceClasses are given then the literals are replaced by their representatives <br>
     *  - literals which are false or true in the model are treated accordingly
     *  - Double literals p != p cause the unit handler to be called with -p<br>
     *  - If there are negative literals, then the positive literals are extracted. <br>
     *  - For all negative literals -p binary clauses are generated for all other literals q: p,-q,<br>
     *    and the binaryClauseHandler is called
     *  - The origins are collected from the basicClause itself and from the replacements by the equivalence classes.
     *
     * @param basicClause        a disjointness clause
     * @param equivalenceClasses null or the equivalence classes
     * @return                   [IntArrayList of literals, ArrayList[IntArrayList] of origins], or null
     */
    Object[] analyseClause(int[] basicClause, EquivalenceClasses equivalenceClasses) {
        int length = basicClause.length-2;
        IntArrayList literals;
        ArrayList<IntArrayList> origins = null;
        int id = basicClause[0];
        IntArrayList origin;

        if(equivalenceClasses == null) {
            int[] lits = new int[length];
            System.arraycopy(basicClause,2,lits,0,length);
            literals = IntArrayList.wrap(lits);}
        else{
            literals = new IntArrayList(length);
            origins = new ArrayList<>();
            for(int i = 2; i < basicClause.length; ++i) { // literals are replaced by their representatives
                int literal = basicClause[i];             // corresponding origins are collected
                int representative = equivalenceClasses.mapToRepresentative(literal);
                if(representative != literal) {
                    origins.add(equivalenceClasses.mapToOrigins(literal));
                    literal = representative;}
                else origins.add(null);
                literals.add(literal);}}

        for(int i = 0; i < literals.size(); ++i) {
            int literal = literals.getInt(i);
            if(model.isTrue(literal)) {                           // a true literal makes all others false
                origin = IntArrayList.wrap(new int[]{id});
                Utilities.joinIntArray(origin,origins.get(i));
                for(int j = 0; j < literals.size(); ++j) {
                    if(j == i) {continue;}
                    int lit = literals.getInt(j);
                    unaryClauseHandler.accept(-lit, Utilities.joinIntArray(origin.clone(),origins.get(j)));}
                return null;}}

        literals.removeIf((int literal)->model.isFalse(literal)); // false literals can just be removed
        if(literals.size() <= 1) {return null;}



        boolean hasNegativeLiterals = false;
        IntArrayList deleted = new IntArrayList();
        for(int i = 0; i < literals.size(); ++i) { // duplicates p,p are false : -p, and must be removed
            int literal = literals.getInt(i);
            if(deleted.contains(literal)) {literals.removeInt(i); origins.remove(i--); continue;}
            if(literal < 0) {hasNegativeLiterals = true;}
            for(int j = i+1; j < literals.size(); ++j) {
                if(literals.getInt(j) == literal) {
                    origin = IntArrayList.wrap(new int[]{id});
                    Utilities.joinIntArray(origin,origins.get(i));
                    Utilities.joinIntArray(origin,origins.get(j));
                    unaryClauseHandler.accept(-literal,origin);
                    literals.removeInt(j); origins.remove(j);
                    literals.removeInt(i); origins.remove(i--);
                    deleted.add(literal);
                    break;}}}

        if(literals.size() <= 1) {return null;}

        if(hasNegativeLiterals) { // split into positive literals and generation of binary clauses for negative literals
            int size = literals.size();
            IntArrayList positiveLiterals           = new IntArrayList(size-1);
            ArrayList<IntArrayList> positiveOrigins = new ArrayList<>(size-1);
            for(int i = 0; i < size; ++i) {
                int literal1 = literals.getInt(i);
                if(literal1 > 0) {
                    positiveLiterals.add(literal1);
                    positiveOrigins.add(Utilities.joinIntArray(IntArrayList.wrap(new int[]{id}),origins.get(i)));}
                else {
                    for(int j = 0; j < size; ++j) {
                        if(j == i) {continue;}
                        int literal2 = literals.getInt(j);
                        if(literal1 == -literal2) {continue;}  // tautology
                        origin = IntArrayList.wrap(new int[]{id});
                        Utilities.joinIntArray(origin,origins.get(i));
                        Utilities.joinIntArray(origin,origins.get(j));
                        binaryClauseHandler.accept(-literal1,-literal2,origin);}}}
            return positiveLiterals.size() > 1 ? new Object[]{positiveLiterals,positiveOrigins} : null;}
        else {
            if(origins == null) {
                origins = new ArrayList<>(literals.size());
                for(int i = 0; i < literals.size(); ++i) {origins.add(IntArrayList.wrap(new int[]{id}));}}
            else {
                for(int i = 0; i < origins.size(); ++i) {
                    origins.set(i,Utilities.joinIntArray(IntArrayList.wrap(new int[]{id}),origins.get(i)));}}
            return new Object[]{literals,origins};}}


    /** checks if two literals are disjoint
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return true if the two literals are disjoint
     */
    public boolean areDisjoint(int literal1, int literal2) {
        if(literal1 == -literal2) {return true;}
        if(disjointnesses == null || literal1 < 0 || literal2 < 0) {return false;}
        Object[] result = disjointnesses.get(literal1);
        if (result == null) {return false;}
        return ((IntArrayList)result[0]).contains(literal2);}


    /** computes the literals which must be made true if the given predicate is made true
     * Example: p != q, and p is made true then -q must be made true
     *
     * @param predicate  the predicate to be made true
     * @param truths   collects the literals to be made true.
     * @return         true if some literals must be made true.
     */
    public boolean truths(int predicate, IntArrayList truths) {
        if(disjointnesses == null || predicate < 0) {return false;}
        Object[] result = disjointnesses.get(predicate);
        if (result == null) {return false;}
        for(int literal : (IntArrayList)result[0]) {truths.add(-literal);}
        return !truths.isEmpty();}

    /** returns the origins (basicClause ids) which caused literal1 disjoint to literal2
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return null or the origins which caused the literals to be disjoint.
     */
    public IntArrayList getOrigins(int literal1, int literal2) {
        if(literal1 == literal2) {return null;}
        if(literal1 == -literal2) {return null;}
        if(disjointnesses == null) {return null;}
        Object[] result = disjointnesses.get(literal1);
        if (result == null) {return null;}
        IntArrayList literals = (IntArrayList)result[0];
        int i = 0;
        boolean found = false;
        for(;i < literals.size(); ++i) {
            if(literals.getInt(i) == literal2) {found = true; break;}}
        return found ? ((ArrayList<IntArrayList>)result[1]).get(i) : null;}

    /** checks if there are disjointenss classes
     *
     * @return true if there are no equivalence classes
     */
    public boolean isEmpty() {
        return disjointnesses == null || disjointnesses.isEmpty();}


    /** maps a literal to a string, possibly using the symboltable
     *
     * @param literal a literal
     * @return  its name
     */
    public String literalName(int literal) {
        return symboltable == null ? Integer.toString(literal) : symboltable.getLiteralName(literal);}


    /** lists all disjointness classes
     *
     * @return all disjointness classes as string
     */
    public String toString() {
        if(disjointnesses == null) {return "";}
        StringBuilder st = new StringBuilder();
        st.append("Disjointness Classes\n");
        disjointnesses.forEach((predicate, result) -> {
            st.append(literalName(predicate)).append(": ");
            IntArrayList predicates = (IntArrayList)result[0];
            ArrayList<IntArrayList> origins = (ArrayList<IntArrayList>)result[1];
            for(int i = 0; i< predicates.size(); ++i) {
                st.append(literalName(predicates.getInt(i)));
                IntArrayList origin = origins.get(i);
                if(origin != null) {st.append("@").append(origin.toString());}
                st.append(", ");}
            st.append("\n");});
        return st.toString();}
    }