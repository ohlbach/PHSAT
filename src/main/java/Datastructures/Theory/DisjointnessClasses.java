package Datastructures.Theory;

import Datastructures.Clauses.ClauseType;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/** A disjointness class is a set of literals which are pairwise contradictory.
 * Such a class may come from the input data, or be derived from binary clauses.
 * Created by Ohlbach on 21.09.2018.
 */
public class DisjointnessClasses {
    /** the list of disjunctions representing disjoint literals */
    private ArrayList<IntArrayList> disjointnessClasses = null;

    private Symboltable symboltable = null;

    /** creates empty disjointness classes
     */
    public DisjointnessClasses(Symboltable symboltable) {
        this.symboltable = symboltable;}


    /** checks if there are equivalence classes
    *
    * @return true if there are no equivalence classes
    */
    public boolean isEmpty() {
        return disjointnessClasses == null || disjointnessClasses.isEmpty();}


    /** turns the disjointness classes into basicClauses
     *
     * @return a list of basicClauses representing the disjointness classes.
     */
    public ArrayList<int[]> basicClauses(int id) {
        ArrayList<int[]> clauses = new ArrayList<>();
        for(IntArrayList disjointnessClass : disjointnessClasses) {
            int[] clause = new int[disjointnessClass.size()+2];
            clause[0] = ++id;
            clause[1] = ClauseType.DISJOINT.ordinal();
            for(int i = 0; i < disjointnessClass.size(); ++i) {clause[i+2] = disjointnessClass.getInt(i);}
            clauses.add(clause);}
        return clauses;}

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
    public void addDisjointnessClass(int[] basicClause, EquivalenceClasses equivalenceClasses,
                                     Consumer<Integer> unaryClauseHandler,
                                     BiConsumer<Integer,Integer> binaryClauseHandler) {
        assert basicClause.length > 2;
        if(basicClause.length == 3) {return;}
        assert basicClause[1] == ClauseType.DISJOINT.ordinal() || basicClause[1] == ClauseType.XOR.ordinal();
        IntArrayList disjointnessClass = new IntArrayList();
        IntArrayList unaryLiterals = new IntArrayList();
        for(int i = 2; i < basicClause.length; ++i) {
            int literal = basicClause[i];
            if(equivalenceClasses != null) {literal = equivalenceClasses.mapToRepresentative(literal);}
            if(unaryLiterals.contains(literal)) {continue;}  // a literal occurs more than two times.
            if(literal < 0) {  // negative literals must be treated by ordinary binary axioms
                for(int j = 2; j < basicClause.length; ++j) {
                    int literal2 = basicClause[j];
                    if(literal2 == literal || unaryLiterals.contains(-literal2)) {continue;}
                    binaryClauseHandler.accept(-literal,-literal2);}
                continue;}
            if(disjointnessClass.contains(literal)) {  // p != p implies -p
                disjointnessClass.removeInt(literal);
                unaryClauseHandler.accept(-literal);
                unaryLiterals.add(literal);
                continue;}
            disjointnessClass.add(literal);}
        if(disjointnessClass.size() <= 1) {return;}
        if(disjointnessClasses == null) {disjointnessClasses= new ArrayList<>();}
        if(!joinClass(disjointnessClass)) {disjointnessClasses.add(disjointnessClass);}}

    public void addSimplifiedDisjointnessClass(int[] basicClause) {
        IntArrayList disjointnessClass = new IntArrayList();
        for(int i = 2; i < basicClause.length; ++i) { disjointnessClass.add(basicClause[i]);}
        if(disjointnessClasses == null) {disjointnessClasses= new ArrayList<>();}
        disjointnessClasses.add(disjointnessClass);}


    private ArrayList<IntArrayList> dummyClasses = new ArrayList<>();
    private HashSet<Integer> dummyLiterals = new HashSet<>();

    /** tries to join a new class to existing classes
     *
     * @param disjointnessClass a new class
     * @return true if the class has been joined
     */
    private boolean joinClass(IntArrayList disjointnessClass) {
        for(IntArrayList oldClass : disjointnessClasses) {
            if(oldClass.containsAll(disjointnessClass)) {return true;}}
        dummyClasses.clear();
        dummyLiterals.clear();
        for(int literal : disjointnessClass) {
            for(IntArrayList oldClass : disjointnessClasses) {
                if(oldClass.contains(literal)) {
                    dummyClasses.add(oldClass);
                    dummyLiterals.addAll(oldClass);}}}
        if(dummyClasses.isEmpty()) {return false;}
        dummyLiterals.addAll(disjointnessClass);
        dummyClasses.add(disjointnessClass);
        Integer[] literals = new Integer[dummyLiterals.size()];
        dummyLiterals.toArray(literals);
        for(int i = 0; i< literals.length; ++i) {
            int literal1 = literals[i];
            for(int j = i+1; j < literals.length; ++j) {
                if(!areDisjoint(dummyClasses,literal1,literals[j])) {return false;}}}
        for(int i = 0; i < dummyClasses.size()-1; ++i) {
            disjointnessClasses.remove(dummyClasses.get(i));}
        IntArrayList newClass = new IntArrayList(literals.length);
        for(int i : literals) {newClass.add(i);}
        disjointnessClasses.add(newClass);
        return true;}


    /** checks if two literals are disjoint
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return true if the two literals are disjoint
     */
    public boolean areDisjoint(ArrayList<IntArrayList> disjointnessClasses, int literal1, int literal2) {
        if(literal1 == literal2) {return false;}
        if(literal1 == -literal2) {return true;}
        if(disjointnessClasses == null) {return false;}
        for(IntArrayList disjointnessClass : disjointnessClasses) {
            if(disjointnessClass.contains(literal1)) {return disjointnessClass.contains(literal2);}
            if(disjointnessClass.contains(literal2)) {return disjointnessClass.contains(literal1);}}
        return false;}

    /** checks if two literals are disjoint
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return true if the two literals are disjoint
     */
    public boolean areDisjoint(int literal1, int literal2) {
        return areDisjoint(disjointnessClasses,literal1,literal2);}



    /** computes the literals which must be made true if the given literal is made true
     * Example: p != q, and p is made true then -q must be made true
     *
     * @param literal  the literal to be made true
     * @param truths   collects the literals to be made true.
     * @return         true if some literals must be made true.
     */
    public boolean truths(int literal, IntArrayList truths) {
        if(disjointnessClasses == null || literal < 0) {return false;}
        int literalp = literal;
        for(IntArrayList dissClass : disjointnessClasses) {
            if(dissClass.contains(literalp)) {
                for(int lit : dissClass) {if(lit != literalp) {truths.add(-lit);}}}}
        return !truths.isEmpty();}

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
        if (disjointnessClasses == null) {return "";}
        StringBuilder st = new StringBuilder();
        st.append("Disjointness Classes\n");
        for(IntArrayList disjointnessClass : disjointnessClasses) {
            int size = disjointnessClass.size();
            st.append("  ");
            for(int i = 0; i < size - 1; ++i) {
                st.append(literalName(disjointnessClass.getInt(i))).append(" /= ");}
            st.append(literalName(disjointnessClass.getInt(size-1))).append("\n");}
        return st.toString();}
    }




