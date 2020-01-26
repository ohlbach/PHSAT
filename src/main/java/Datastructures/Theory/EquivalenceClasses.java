package Datastructures.Theory;

import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Utilities.IntegerQueue;

import java.util.*;
import java.util.function.Consumer;

/** This class manages equivalence classes of literals.
 *
 * Created by ohlbach on 20.09.2018.
 */
public class EquivalenceClasses {
    /** the list of equivalence classes */
    private ArrayList<ArrayList<Integer>> equivalenceClasses = new ArrayList<>();
    /** maps literals to their representatives in an equivalence class */
    private HashMap<Integer,Integer> replacements = new HashMap<>();
    /**  a function to be called when adding the new equivalence class causes a contradiction p equiv -p */
    private Consumer<String> contradictionHandler = null;

    private Symboltable symboltable;

    /** creates empty equivalence classes
     *
     * @param symboltable          maps integers to names
     * @param contradictionHandler for reporting contradictions
     */
    public EquivalenceClasses(Symboltable symboltable,Consumer<String> contradictionHandler) {
        this.contradictionHandler = contradictionHandler;
        this.symboltable = symboltable;}

    /** checks if there are equivalence classes
     *
     * @return true if there are no equivalence classes
     */
    public boolean isEmpty() {
        return equivalenceClasses == null;}

    /** turns the equivalence classes into basicClauses
     *
     * @return a list of basicClauses representing the equivalence classes.
     */
    public ArrayList<int[]> basicClauses(int id) {
        ArrayList<int[]> clauses = new ArrayList<>();
        for(ArrayList<Integer> equivalenceClass : equivalenceClasses) {
            int[] clause = new int[equivalenceClass.size()+2];
            clause[0] = ++id;
            clause[1] = ClauseType.EQUIV.ordinal();
            for(int i = 0; i < equivalenceClass.size(); ++i) {clause[i+2] = equivalenceClass.get(i);}
            clauses.add(clause);}
        return clauses;}


    /** is used to sort literals according to their absolute value */
    private static Comparator<Integer> absComparator = Comparator.comparingInt(literal -> Math.abs(literal));

    /** turns a basicClause into an equivalence class.
     * The basicClause must be already a simplified and normalized equivalence class.
     *
     * @param basicClause [clause-problemId,typenumber,literal1,...]
     */
    public boolean addSimplifiedEquivalenceClass(int[] basicClause) {
        assert basicClause.length > 2;
        if(basicClause.length == 3) {return true;}
        if(equivalenceClasses == null) {
            equivalenceClasses = new ArrayList<ArrayList<Integer>>();
            replacements = new HashMap<>();}

        ArrayList<Integer> equivalenceClass = new ArrayList<>();
        int representative = basicClause[2];
        equivalenceClass.add(representative);
        for(int i = 3; i < basicClause.length; ++i) {
            int literal = basicClause[i];
            equivalenceClass.add(literal);
            replacements.put(literal,representative);
            replacements.put(-literal,-representative);}
        equivalenceClasses.add(equivalenceClass);
        return true;}


    /** turns a basicClause into an equivalence class.
     * Overlapping classes are joined.
     * The classes are sorted according to the absolute value of the literals.
     * The smallest predicate number becomes the representative of the class.
     *
     * @param basicClause [clause-problemId,typenumber,literal1,...]
     */
    public boolean addEquivalenceClass(int[] basicClause) {
        assert basicClause.length > 2;
        if(basicClause.length == 3) {return true;}
        if(equivalenceClasses == null) {
            equivalenceClasses = new ArrayList<ArrayList<Integer>>();
            replacements = new HashMap<>();}

        int literal = basicClause[2];
        for(int i = 3; i < basicClause.length; ++i) {
            addEquivalence(literal,basicClause[i]);}
        return true;}

    /** turns the two literals into an equivalence class.
     * Overlapping classes are joined.
     * The classes are sorted according to the absolute value of the literals.
     * The smallest predicate number becomes the representative of the class.
     *
     * @param literal1 the first literal
     * @param literal2 the second literal
     */
    public boolean addEquivalence(int literal1, int literal2) {
        if(equivalenceClasses == null) {
            equivalenceClasses = new ArrayList<ArrayList<Integer>>();
            replacements = new HashMap<>();}
        ArrayList<Integer> eClass = null;
        for(ArrayList<Integer> eqClass : equivalenceClasses) {
            if(eqClass.contains(literal1) && eqClass.contains(literal2)) {return false;}
            eClass = joinIfPossible(eqClass,literal1,literal2);
            if(eClass != null) {break;}
            eClass = joinIfPossible(eqClass,literal2,literal1);
            if(eClass != null) {break;}}
        if(eClass == null) {
            eClass = new ArrayList<Integer>(2);
            equivalenceClasses.add(eClass);
            eClass.add(literal1); eClass.add(literal2);}
        eClass.sort(absComparator);
        int representative = eClass.get(0);
        if(literal1 != representative) {
            replacements.put(literal1,representative);
            replacements.put(-literal1,-representative);}
        if(literal2 != representative) {
            replacements.put(literal2,representative);
            replacements.put(-literal2,-representative);}
        return true;}

    /** tries to join the equivalence literal1 == literal2 into the existing equivalence class.
     *  It must be called for (literal1,literal2) and (literal2,literal1)
     *  If the equivalence contradicts an existing equivalence, the contradiction handler is called.
     *
     * @param eqClass   an existing equivalence class
     * @param literal1  the first literal
     * @param literal2  the second literal
     * @return  either eqClass itself, if the literals were inserted, or null
     */
    private ArrayList<Integer> joinIfPossible(ArrayList<Integer> eqClass, int literal1, int literal2) {
        if(eqClass.contains(literal1)) {
            if(eqClass.contains(literal2)) {return eqClass;}
            if(eqClass.contains(-literal2)) {
                String reason = "Equivalence " + toStringSt(literal1) + " = " +
                        toStringSt(literal2) + " contradicts existing equivalences: \n";
                for(int i = 0; i < eqClass.size()-1; ++i) {reason += toStringSt(eqClass.get(i))+ " = ";}
                reason += toStringSt(eqClass.get(eqClass.size()-1));
                contradictionHandler.accept(reason); return eqClass;}
            eqClass.add(literal2);
            return eqClass;}
        return null;}



    /** maps literals to their representative in the equivalence class.
     *
     * @param literal the literal
     * @return either the literal itself, or its representative.
     */
    public int mapToRepresentative(int literal) {
        if(replacements == null) {return literal;}
        Integer replaced = replacements.get(literal);
        return (replaced == null) ? literal : replaced;}


    /** completes a model by the equivalence classes.
     * If one of the literals in an equivalence class has a truth value, then all others get the same value.<br>
     * If none of the literals in an equivalence class has a truth value, then nothing is changed.
     *
     * @returns Unsatisfiable if two different literals in a class have already different truth values, otherwise null
     */
    public Result completeModel(Model model) {
        for(ArrayList<Integer> eqClass : equivalenceClasses) {
            int status = 0;
            int literal1 = 0;
            for(int literal : eqClass) {
                int stat = model.status(literal);
                if(stat != 0) {status = stat; literal1 = literal;break;}}
            if(status == 0) {continue;}
            for(int literal : eqClass) {
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
     * @param literal  the literal to be made true
     * @param truths   collects the literals to be made true.
     * @return         true if some literals must be made true.
     */
    public boolean truths(int literal, ArrayList<Integer> truths) {
        if(replacements == null || replacements.get(literal) == null) {return false;}
        Integer literalp = literal;
        Integer literaln = -literal;
        boolean found = false;
        for(ArrayList<Integer> eqClass : equivalenceClasses) {
            if(eqClass.contains(literalp)) {
                found = true;
                for(Integer lit : eqClass) {if(!lit.equals(literalp)) {truths.add(lit);}}}
            else {
                if(eqClass.contains(literaln)) {
                    found = true;
                    for(Integer lit : eqClass) {if(!lit.equals(literaln)) {truths.add(-lit);}}}}
            if(found) break;}
        return !truths.isEmpty();}



    /** lists all equivalence classes and the replacements
     *
     * @return all equivalence classes as string
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        if(equivalenceClasses.isEmpty()) {return "";}
        st.append("Equivalence Classes:\n");
        for(ArrayList<Integer> eqClass : equivalenceClasses) {
            st.append(toStringSt(eqClass.get(0))).append (" = ");
            for(int i = 1; i < eqClass.size()-1; ++i) {st.append(toStringSt(eqClass.get(i))).append(" = ");}
            st.append(toStringSt(eqClass.get(eqClass.size()-1))).append("\n");}
        st.append("Replacements:\n");
            for(Map.Entry entry : replacements.entrySet()) {
                st.append(toStringSt((Integer)entry.getKey())).
                        append(" -> ").
                        append(toStringSt((int)entry.getValue())).
                        append("\n");}
        return st.toString();}

}
