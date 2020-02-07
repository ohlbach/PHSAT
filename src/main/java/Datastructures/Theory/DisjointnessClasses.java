package Datastructures.Theory;

import Datastructures.Clauses.ClauseType;
import Datastructures.Symboltable;
import Utilities.Utilities;
import Utilities.TriConsumer;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import java.util.*;
import java.util.function.BiConsumer;

/** A disjointness class is a set of literals which are pairwise contradictory.
 * Such a class may come from the input data, or be derived from binary clauses.
 * Created by Ohlbach on 21.09.2018.
 */
public class DisjointnessClasses {
    private HashMap<Integer,Object[]> disjointnesses = null;

    private Symboltable symboltable = null;

    BiConsumer<Integer,IntArrayList> contradictionHandler = null;

    TriConsumer<Integer,Integer,IntArrayList> binaryClauseHandler = null;

    BiConsumer<Integer,IntArrayList> unaryClauseHandler = null;

    /** creates empty disjointness classes
     */
    public DisjointnessClasses(Symboltable symboltable,
                               BiConsumer<Integer,IntArrayList> unaryClauseHandler,
                               TriConsumer<Integer,Integer,IntArrayList> binaryClauseHandler) {
        this.symboltable = symboltable;
        this.unaryClauseHandler = unaryClauseHandler;
        this.binaryClauseHandler = binaryClauseHandler;}


    /** checks if there are equivalence classes
    *
    * @return true if there are no equivalence classes
    */
    public boolean isEmpty() {
        return disjointnesses == null || disjointnesses.isEmpty();}

    /** turns a basicClause into a disjointness class. <br>
     * A true literal causes all other literals to become false <br>
     * A false literal is ignored <br>
     * Two true literals are a contradiction <br>
     * p &lt;=&gt; -p is ignored.<br>
     * A double literal p,p is a contradiction.<br>
     * The corresponding observers are called.
     * New disjunctions which are subsets of a new clause are deleted.
     * Literals occurring in several classes may cause joining of the classes.
     *
     * @param basicClause [clause-problemId,typenumber,literal1,...]
     * @return the result disjointness clause or null
     */
    public boolean addDisjointnessClass(int[] basicClause, EquivalenceClasses equivalenceClasses) {
        assert basicClause.length > 3;
        assert basicClause[1] == ClauseType.DISJOINT.ordinal() || basicClause[1] == ClauseType.XOR.ordinal();
        Object[] result = analyseClause(basicClause,equivalenceClasses);
        if(result == null) {return false;}
        IntArrayList literals = (IntArrayList)result[0];
        ArrayList<IntArrayList> origins  = (ArrayList<IntArrayList>)result[1];
        if(disjointnesses == null) {disjointnesses = new HashMap<>();}
        for(int literal : literals) {
            result = disjointnesses.get(literal);
            if(result == null) {disjointnesses.put(literal,new Object[]{literals,origins});}
            else {
                IntArrayList joinedLiterals = ((IntArrayList)result[0]);
                joinedLiterals.addAll(literals);
                ArrayList<IntArrayList> joinedOrigins = (ArrayList<IntArrayList>)result[1];
                joinedOrigins.addAll(origins);
                removeDuplicates(literal,joinedLiterals,joinedOrigins);
                disjointnesses.put(literal,new Object[]{joinedLiterals,joinedOrigins});}}
        return true;}

    public void addDisjointnessClass(int literal1, int literal2, IntArrayList origins, EquivalenceClasses equivalenceClasses) {
        assert literal1 > 0;
        assert literal2 > 0;
        assert literal1 != literal2;
        if(disjointnesses == null) {disjointnesses = new HashMap<>();}

        Object[] result = disjointnesses.get(literal1);
        if(result == null) {disjointnesses.put(literal1,new Object[]{IntArrayList.wrap(new int[]{literal2}),origins});}
        else {
            IntArrayList joinedLiterals = ((IntArrayList)result[0]);
            joinedLiterals.add(literal2);
            ArrayList<IntArrayList> joinedOrigins = (ArrayList<IntArrayList>)result[1];
            joinedOrigins.add(origins);
            removeDuplicates(0,joinedLiterals,joinedOrigins);
            disjointnesses.put(literal1,new Object[]{joinedLiterals,joinedOrigins});}

        result = disjointnesses.get(literal2);
        if(result == null) {disjointnesses.put(literal2,new Object[]{IntArrayList.wrap(new int[]{literal1}),origins});}
        else {
            IntArrayList joinedLiterals = ((IntArrayList)result[0]);
            joinedLiterals.add(literal1);
            ArrayList<IntArrayList> joinedOrigins = (ArrayList<IntArrayList>)result[1];
            joinedOrigins.add(origins);
            removeDuplicates(0,joinedLiterals,joinedOrigins);
            disjointnesses.put(literal2,new Object[]{joinedLiterals,joinedOrigins});}}

        void removeDuplicates(int literal, IntArrayList literals, ArrayList<IntArrayList> origins) {
        for(int i = 0; i < literals.size(); ++i) {
            int literali = literals.getInt(i);
            if(literali == literal) {
                literals.removeInt(i);
                origins.remove(i--);}
            else {
                IntArrayList originsi = origins.get(i);
                for(int j = i+1; j < literals.size(); ++j) {
                    int literalj = literals.getInt(j);
                    if(literali == literalj) {
                        IntArrayList originsj = origins.get(j);
                        if(originsi.size() < originsj.size()) {
                            literals.removeInt(j);
                            origins.remove(j);}
                        else {literals.removeInt(i); origins.remove(i--);}
                        break;}}}}}


    /** analyses a disjointness basicClause and handles the following cases:
     *  - If equivalenceClasses are given the the literals are replaced by their representatives <br>
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
        IntArrayList literals = new IntArrayList(basicClause.length-2);
        ArrayList<IntArrayList> origins = new ArrayList<>();
        int id = basicClause[0];

        for(int i = 2; i < basicClause.length; ++i) { // literals are replaced by their representatives
            int literal = basicClause[i];             // corresponding origins are collected
            if(equivalenceClasses != null) {
                int representative = equivalenceClasses.mapToRepresentative(literal);
                if(representative != literal) {
                    origins.add(equivalenceClasses.mapToOrigins(literal));
                    literal = representative;}
                else origins.add(null);}
            else {origins.add(null);}
            literals.add(literal);}

        IntArrayList origin = null;
        boolean hasNegativeLiterals = false;
        IntArrayList deleted = new IntArrayList();
        for(int i = 0; i < literals.size(); ++i) { // duplicates are detected
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
                        if(literal1 == -literal2) {continue;}
                        origin = IntArrayList.wrap(new int[]{id});
                        Utilities.joinIntArray(origin,origins.get(i));
                        Utilities.joinIntArray(origin,origins.get(j));
                        binaryClauseHandler.accept(-literal1,-literal2,origin);}}}
            return positiveLiterals.size() > 1 ? new Object[]{positiveLiterals,positiveOrigins} : null;}
        else {
            for(int i = 0; i < origins.size(); ++i) {
                origins.set(i,Utilities.joinIntArray(IntArrayList.wrap(new int[]{id}),origins.get(i)));}
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


    /** computes the literals which must be made true if the given literal is made true
     * Example: p != q, and p is made true then -q must be made true
     *
     * @param literal  the literal to be made true
     * @param truths   collects the literals to be made true.
     * @return         true if some literals must be made true.
     */
    public boolean truths(int literal, IntArrayList truths) {
        if(disjointnesses == null || literal < 0) {return false;}
        Object[] result = disjointnesses.get(literal);
        if (result == null) {return false;}
        for(int literal1 : (IntArrayList)result[0]) {truths.add(-literal1);}
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
        disjointnesses.forEach((literal, result) -> {
            st.append(literal).append(": ");
            IntArrayList literals = (IntArrayList)result[0];
            ArrayList<IntArrayList> origins = (ArrayList<IntArrayList>)result[1];
            for(int i = 0; i< literals.size(); ++i) {
                st.append(literals.getInt(i));
                IntArrayList origin = origins.get(i);
                if(origin != null) {st.append(" @ ").append(origin.toString());}
                st.append(",");}});
        return st.toString();}
    }