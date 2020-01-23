package Datastructures.Theory;

import Datastructures.Clauses.ClauseType;
import Datastructures.Symboltable;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/** A disjointness class is a set of literals which are pairwise contradictory.
 * Such a class may come from the input data, or be derived from binary clauses.
 * Created by Ohlbach on 21.09.2018.
 */
public class DisjointnessClasses {
    /** the list of disjunctions representing disjoint literals */
    private ArrayList<ArrayList<Integer>> disjointnessClasses = null;

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
        ArrayList<Integer> disjointnessClass = new ArrayList<>();
        ArrayList<Integer> unaryLiterals = new ArrayList<>();
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
                disjointnessClass.remove(literal);
                unaryClauseHandler.accept(-literal);
                unaryLiterals.add(literal);
                continue;}
            disjointnessClass.add(literal);}
        if(disjointnessClass.size() <= 1) {return;}
        if(disjointnessClasses == null) {disjointnessClasses= new ArrayList<>();}
        if(!joinClass(disjointnessClass)) {disjointnessClasses.add(disjointnessClass);}}

    public void addSimplifiedDisjointnessClass(int[] basicClause) {
        ArrayList<Integer> disjointnessClass = new ArrayList<>();
        for(int i = 2; i < basicClause.length; ++i) { disjointnessClass.add(basicClause[i]);}
        if(disjointnessClasses == null) {disjointnessClasses= new ArrayList<>();}
        disjointnessClasses.add(disjointnessClass);}


    private ArrayList<ArrayList<Integer>> dummyClasses = new ArrayList<>();
    private HashSet<Integer> dummyLiterals = new HashSet<>();

    /** tries to join a new class to existing classes
     *
     * @param disjointnessClass a new class
     * @return true if the class has been joined
     */
    private boolean joinClass(ArrayList<Integer> disjointnessClass) {
        for(ArrayList<Integer> oldClass : disjointnessClasses) {
            if(oldClass.containsAll(disjointnessClass)) {return true;}}
        dummyClasses.clear();
        dummyLiterals.clear();
        for(Integer literal : disjointnessClass) {
            for(ArrayList<Integer> oldClass : disjointnessClasses) {
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
            disjointnessClass.remove(dummyClasses.get(i));}
        ArrayList<Integer> newClass = new ArrayList<>(literals.length);
        for(Integer i : literals) {newClass.add(i);}
        disjointnessClasses.add(newClass);
        return true;}


    /** checks if two literals are disjoint
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return true if the two literals are disjoint
     */
    public boolean areDisjoint(ArrayList<ArrayList<Integer>> disjointnessClasses, int literal1, int literal2) {
        if(literal1 == literal2) {return false;}
        if(literal1 == -literal2) {return true;}
        if(disjointnessClasses == null) {return false;}
        for(ArrayList<Integer> disjointnessClass : disjointnessClasses) {
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



    /** computes the flips of truth values if the truth value of the given predicate is flipped.
     * Example: p != q, and false(p) is flipped to true(p). If true(q) then q must be flipped.
     *
     * @param predicate the predicate to be flipped
     * @param model     a model for all literals
     * @param flips     collects the predicates to be flipped.
     * @return          true if some predicates must be flipped.
     */
    public boolean flips(int predicate, Model model, ArrayList<Integer> flips) {
        if(disjointnessClasses == null || model.isTrue(predicate)) {return false;} // if predicate becomes false, nothing must be flipped
        flips.clear();
        Integer literalp = predicate;  // now predicate is flipped to true. All disjoint predicates must become false.
        for(ArrayList<Integer> dissClass : disjointnessClasses) {
            if(dissClass.contains(literalp)) {
                for(Integer lit : dissClass) {
                    if(!lit.equals(literalp) && model.isTrue(lit)) {flips.add(Math.abs(lit));}};}}
        return !flips.isEmpty();}

    /** computes the literals which must be made true if the given literal is made true
     * Example: p == q, and p is made true then q must be made true
     *
     * @param literal  the literal to be made true
     * @param truths   collects the literals to be made true.
     * @return         true if some literals must be made true.
     */
    public boolean truths(int literal, Model model, ArrayList<Integer> truths) {
        if(disjointnessClasses == null) {return false;}   // überarbeiten
        truths.clear();
        Integer literalp = literal;
        for(ArrayList<Integer> dissClass : disjointnessClasses) {
            if(dissClass.contains(literalp)) {
                for(Integer lit : dissClass) {if(!lit.equals(literalp)) {truths.add(-lit);}}}}
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
        for(ArrayList<Integer> disjointnessClass : disjointnessClasses) {
            int size = disjointnessClass.size();
            st.append("  ");
            for(int i = 0; i < size - 1; ++i) {
                st.append(literalName(disjointnessClass.get(i))).append(" /= ");}
            st.append(literalName(disjointnessClass.get(size-1))).append("\n");}
        return st.toString();}
    }




