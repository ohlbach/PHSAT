package Datastructures.Theory;

import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Utilities.Utilities;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import it.unimi.dsi.fastutil.ints.IntComparator;

import java.util.*;
import java.util.function.BiConsumer;

/** This class manages equivalence classes of literals.
 *
 * Created by ohlbach on 20.09.2018.
 */
public class EquivalenceClasses {
    /** the list of equivalence classes */
    private ArrayList<IntArrayList[]> equivalenceClasses = null;

    /** maps literals to their representatives in the equivalence class */
    private HashMap<Integer,Integer> representativesMap = null;

    /** maps literals to the basicClause ids causing the equivalence which contains the literal. */
    private HashMap<Integer,IntArrayList> originsMap = null;

    /**  a function to be called when adding the new equivalence class causes p equiv -p,*/
    private BiConsumer<Integer,IntArrayList> contradictionHandler = null;

    /**  a function to be called when adding the new equivalence class causes p equiv p, which implies p */
    private BiConsumer<Integer,IntArrayList> unitHandler = null;

    private Symboltable symboltable;

    /** creates empty equivalence classes
     *
     * @param symboltable          maps integers to names
     * @param contradictionHandler for reporting contradictions
     * @param unitHandler          for reporting unit clauses
     */
    public EquivalenceClasses(Symboltable symboltable,
                              BiConsumer<Integer,IntArrayList> contradictionHandler,
                              BiConsumer<Integer,IntArrayList> unitHandler) {
        this.symboltable          = symboltable;
        this.contradictionHandler = contradictionHandler;
        this.unitHandler          = unitHandler;}

    /** This constructor clones the equivalence classes of another instance
     *
     * @param eqClasses  another instance of EquivalenceClasses, typically coming from the Preparer
     * @param contradictionHandler  for dealing with contradictions in new equivalence classes added later.
     */
    public EquivalenceClasses(EquivalenceClasses eqClasses,
                              BiConsumer<Integer,IntArrayList> contradictionHandler,
                              BiConsumer<Integer,IntArrayList> unitHandler) {
        this.symboltable          = eqClasses.symboltable;
        this.contradictionHandler = contradictionHandler;
        this.unitHandler          = unitHandler;
        initialize();
        if(!eqClasses.equivalenceClasses.isEmpty()) {
            for(IntArrayList[] eqClass : eqClasses.equivalenceClasses) {
                IntArrayList eClass = eqClass[0].clone();
                IntArrayList origin = (eqClass[1] != null) ? eqClass[1].clone() : null;
                equivalenceClasses.add(new IntArrayList[]{eClass,origin});}
            representativesMap = (HashMap)eqClasses.representativesMap.clone();
            originsMap         = (HashMap)eqClasses.representativesMap.clone(); }}

    /** initializes the lists amd maps
     */
    private void initialize() {
        equivalenceClasses    = new ArrayList<>();
        representativesMap    = new HashMap<>();
        originsMap            = new HashMap<>();}

    /** checks if there are equivalence classes
     *
     * @return true if there are no equivalence classes
     */
    public boolean isEmpty() {
        return equivalenceClasses == null || equivalenceClasses.isEmpty();}



    /** is used to sort literals according to their absolute value */
    private static IntComparator absComparator = (i,j) -> Integer.compare(Math.abs(i),Math.abs(j));


    /** turns a basicClause into an equivalence class.
     * Overlapping classes are joined.<br>
     * The classes are sorted according to the absolute value of the literals.<br>
     * The smallest predicate number becomes the representative of the class.<br>
     * If a contradiction p = -p is found then the contradictionHandler is called. <br>
     * If the class shrinks to a unit class then the unitHandler is called.
     *
     *
     * @param basicClause [clause-problemId,typenumber,literal1,...]
     * @return false if a contradiction was found, otherwise true.
     */
    public boolean addEquivalenceClass(int[] basicClause) {
        assert basicClause.length > 2;
        if(basicClause.length == 3) {return true;}
        if(equivalenceClasses == null) {initialize();}
        findExistingClasses(basicClause);
        if(existingClasses.isEmpty()) {return addSeparateEquivalenceClass(basicClause);}
        else {return joinEquivalenceClasses(basicClause);}}

    /** turns the two literals into an equivalence class.
     * Overlapping classes are joined.<br>
     * The classes are sorted according to the absolute value of the literals.<br>
     * The smallest predicate number becomes the representative of the class.<br>
     * If a contradiction p = -p is found then the contradictionHandler is called. <br>
     * If the class shrinks to a unit class then the unitHandler is called.
     *
     * @param literal1  a literal
     * @param literal2  a literal
     * @param origins    the list of basicClause ids which caused this equivalence
     * @return false if a contradiction was found.
     */
    public boolean addEquivalenceClass(int literal1, int literal2, IntArrayList origins) {
        if(literal1 == literal2) {unitHandler.accept(literal1,origins); return true;}
        if(literal1 == -literal2) {contradictionHandler.accept(Math.abs(literal1),origins); return false;}
        if(equivalenceClasses == null) {initialize();}
        findExistingClasses(literal1,literal2);
        if(existingClasses.isEmpty()) {addSeparateEquivalenceClass(literal1,literal2,origins); return true;}
        else {return joinEquivalenceClasses(literal1,literal2,origins);}}

    /** takes tuples [eqClass,sign]*/
    private ArrayList<Object[]> existingClasses = new ArrayList<>();

    /** searches the exiting equivalence classes for those overlapping with the basicClause
     * These classes are collected in existingClasses as tuples [eqClass,sign] <br>
     * The sign is +1 if the literals are contained in the class. <br>
     * The sign is -1 if the negated literals are contains in the class.
     *
     * @param basicClause describing equivalent literals.
     */
    private void findExistingClasses(int[] basicClause) {
        existingClasses.clear();
        for(int i = 2; i < basicClause.length; ++i) {
            int literal1 = basicClause[i];
            for(IntArrayList[] eqClass : equivalenceClasses) {
                for(int literal : eqClass[0]) {
                    if(literal == literal1)   {existingClasses.add(new Object[]{eqClass,1}); continue;}
                    if(literal == -literal1)  {existingClasses.add(new Object[]{eqClass,-1});}}}}}

    /** searches the exiting equivalence classes for those containing literal1 or literal2 or their negations.
     * These classes are collected in existingClasses as tuples [eqClass,sign] <br>
     * The sign is +1 if the literals are contained in the class. <br>
     * The sign is -1 if the negated literals are contains in the class.
     *
     * @param literal1 a literal
     * @param literal2 a literal
     */
    private void findExistingClasses(int literal1, int literal2) {
        existingClasses.clear();
        for(IntArrayList[] eqClass : equivalenceClasses) {
            for(int literal : eqClass[0]) {
                if(literal == literal1 || literal == literal2)   {existingClasses.add(new Object[]{eqClass,1}); continue;}
                if(literal == -literal1 || literal == -literal2) {existingClasses.add(new Object[]{eqClass,-1});}}}}


    /** joins the new equivalence class with the overlapping classes detected by findExistingClasses
     *
     * @param basicClause a new equivalence class
     * @return false if a contradiction was found, otherwise true.
     */
    private boolean joinEquivalenceClasses(int[] basicClause) {
        IntArrayList literals = new IntArrayList();
        IntArrayList origins  = new IntArrayList();
        joinExistingClasses(literals,origins);
        for(int i = 2; i < basicClause.length; ++i) {literals.add(basicClause[i]);}
        origins.add(basicClause[0]);
        return integrateNewClass(literals,origins);}

    /** joins the new class literal1 == literal2 with the overlapping classes detected by findExistingClasses
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @param newOrigins a list of basicClause ids which implied this equivalence
     * @return false if a contradiction was found, otherwise true
     */
    private boolean joinEquivalenceClasses(int literal1, int literal2, IntArrayList newOrigins) {
        IntArrayList literals = new IntArrayList();
        IntArrayList origins  = new IntArrayList();
        joinExistingClasses(literals,origins);
        literals.add(literal1); literals.add(literal2);
        if(newOrigins != null) {origins.addAll(newOrigins);}
        return integrateNewClass(literals,origins);}

    /** joins the overlapping classes found by findExistingClasses into a new class
     *
     * @param literals a list of equivalent literals
     * @param origins  the basicClause ids which cause this equivalence.
     */
    private void joinExistingClasses(IntArrayList literals, IntArrayList origins) {
        for(Object[] existingClass : existingClasses) {
            IntArrayList[] cl        = (IntArrayList[])existingClass[0];
            IntArrayList oldLiterals = cl[0];
            IntArrayList oldOrigins  = cl[1];
            int          sign        = (Integer)existingClass[1];
            for(int literal : oldLiterals) {literals.add(sign*literal);}
            if(oldOrigins != null) {Utilities.joinIntArray(origins,oldOrigins);}
            equivalenceClasses.remove(cl);}}

    /** integrates the new class into the internal data structures
     *
     * @param literals a list of equivalent literals
     * @param origins the basicClause ids which cause this equivalence.
     * @return false if a contradiction was detected, otherwise false.
     */
    private boolean integrateNewClass(IntArrayList literals, IntArrayList origins) {
        Utilities.removeDuplicates(literals);
        Utilities.removeDuplicates(origins);
        literals.sort(absComparator);
        int representative = literals.getInt(0);
        for(int i = 1; i < literals.size(); ++i) {
            int literal = literals.getInt(i);
            representativesMap.put(literal,representative);
            representativesMap.put(-literal,-representative);
            originsMap.put(literal,origins);
            originsMap.put(-literal,origins);}
        if(containsContradictions(literals,origins)) {return false;}
        equivalenceClasses.add(new IntArrayList[]{literals,origins});
        return true;}

    /** checks if the equivalent literals contain literals p equiv -p, which causes a contradiction.
     * If a contradiction p equiv -p is discovered then the contradiction handler is called with (p,origins)
     *
     * @param literals a list of equivalent literals
     * @param origins  a list of basicClause ids
     * @return true if a contradiction was discovered.
     */
    private boolean containsContradictions(IntArrayList literals, IntArrayList origins) {
        int size = literals.size();
        for(int i = 0; i < size; ++i) {
            int literal = literals.getInt(i);
            for(int j = i+1; j < size; ++j) {
                if(literal == -literals.getInt(j)) {
                    contradictionHandler.accept(Math.abs(literal),origins);
                    return true;}}}
        return false;}

    /** adds an equivalence class, which has no intersection with other classes.
     * If a contradiction was detected the contradictionHandler is called. <br>
     * It the literals shrink to a single literal, the unitHandler is called.
     *
     * @param basicClause as equivalence class.
     * @return false if a contradiction was detected, otherwise true
     */
    private boolean addSeparateEquivalenceClass(int[] basicClause) {
        IntArrayList literals = new IntArrayList(basicClause.length-2);
        for(int i = 2; i < basicClause.length; ++i) {literals.add(basicClause[i]);}
        Utilities.removeDuplicates(literals);
        IntArrayList origins = IntArrayList.wrap(new int[]{basicClause[0]});
        if(literals.size() == 1) {
            unitHandler.accept(literals.getInt(0),origins);
            return true;}
        literals.sort(absComparator);
        int representative = literals.getInt(0);
        for(int i = 1; i < literals.size(); ++i) {
            int literal = literals.getInt(i);
            representativesMap.put(literal,representative);
            representativesMap.put(-literal,-representative);
            originsMap.put(literal,origins);
            originsMap.put(-literal,origins);}
        if(containsContradictions(literals,origins)) {return false;}
        equivalenceClasses.add(new IntArrayList[]{literals,origins});
        return true;}

    /** adds an equivalence literal1 == literal2 which has no intersection with other classes
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @param origin   null or a list of basicClause-ids which imply the equivalence
     */
    private void addSeparateEquivalenceClass(int literal1, int literal2, IntArrayList origin) {
        IntArrayList eqClass = IntArrayList.wrap(new int[]{literal1,literal2});
        eqClass.sort(absComparator);
        int representative = eqClass.getInt(0);
        int literal = eqClass.getInt(1);
        representativesMap.put(literal,representative);
        representativesMap.put(-literal,-representative);
        originsMap.put(literal,origin);
        originsMap.put(-literal,origin);
        equivalenceClasses.add(new IntArrayList[]{eqClass,origin});}


    /** maps literals to their representative in the equivalence class.
     *
     * @param literal the literal
     * @return either the literal itself, or its representative.
     */
    public int mapToRepresentative(int literal) {
        if(representativesMap == null) {return literal;}
        Integer replaced = representativesMap.get(literal);
        return (replaced == null) ? literal : replaced;}

    /** yields the original basicClause ids for an equivalence which causes the representativesMap
     *
     * @param literal a literal
     * @return null or the  basicClause ids for an equivalence which causes the representativesMap
     */
    public IntArrayList mapToOrigins(int literal) {
        return (originsMap == null) ? null : originsMap.get(literal);}


    /** completes a model by the equivalence classes.
     * If one of the literals in an equivalence class has a truth value, then all others get the same value.<br>
     * If none of the literals in an equivalence class has a truth value, then nothing is changed.
     *
     * @returns Unsatisfiable if two different literals in a class have already different truth values, otherwise null
     */
    public Result completeModel(Model model) {
        for(IntArrayList[] eqClass : equivalenceClasses) {
            IntArrayList literals = eqClass[0];
            int status = 0;
            int literal1 = 0;
            for(int literal : literals) {
                int stat = model.status(literal);
                if(stat != 0) {status = stat; literal1 = literal; break;}}
            if(status == 0) {continue;}
            for(int literal : literals) {
                int stat = model.status(literal);
                if(stat != 0 && stat != status) {
                    return new Unsatisfiable("Equivalent literal " + toStringSt(literal1) +
                            " is " + model.toString(status) + ", but " + toStringSt(literal) +
                            " is " + model.toString(stat));}
                model.setStatus(literal,status);}}
        return null;}

    /** maps a literal to a string, possibly via a symboltable
     *
     * @param literal     a literal
     * @return the literal as string
     */
    private String toStringSt(int literal) {
        if(symboltable == null) {return Integer.toString(literal);}
        return symboltable.getLiteralName(literal);}


    /** computes the literals which must be made true if the given literal is made true
     * Example: p == q, and p is made true then q must be made true
     *
     * @param literalp  the literal to be made true
     * @param truths   collects the literals to be made true.
     * @return         true if some literals must be made true.
     */
    public boolean truths(int literalp, IntArrayList truths) {
        if(representativesMap == null || representativesMap.get(literalp) == null) {return false;}
        int literaln = -literalp;
        boolean found = false;
        for(IntArrayList[] eqClass : equivalenceClasses) {
            if(eqClass[0].contains(literalp)) {
                found = true;
                for(int lit : eqClass[0]) {if(lit != literalp) {truths.add(lit);}}}
            else {
                if(eqClass[0].contains(literaln)) {
                    found = true;
                    for(int lit : eqClass[0]) {if(lit != literaln) {truths.add(-lit);}}}}
            if(found) break;}
        return !truths.isEmpty();}



    /** lists all equivalence classes and the representativesMap
     *
     * @return all equivalence classes as string
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        if(equivalenceClasses.isEmpty()) {return "";}
        st.append("Equivalence Classes:\n");
        for(IntArrayList[] eqClass : equivalenceClasses) {
            IntArrayList literals = eqClass[0];
            IntArrayList origins = eqClass[1];
            st.append(toStringSt(literals.getInt(0))).append (" = ");
            for(int i = 1; i < literals.size()-1; ++i) {st.append(toStringSt(literals.getInt(i))).append(" = ");}
            st.append(toStringSt(literals.getInt(literals.size()-1)));
            if(origins != null) {st.append(" from ").append(origins.toString());}
            st.append("\n");}
        st.append("Replacements:\n");
            for(Map.Entry entry : representativesMap.entrySet()) {
                st.append(toStringSt((Integer)entry.getKey())).
                        append(" -> ").
                        append(toStringSt((int)entry.getValue())).
                        append("\n");}
        return st.toString();}

}
