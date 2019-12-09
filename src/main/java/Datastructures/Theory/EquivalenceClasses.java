package Datastructures.Theory;

import Datastructures.Symboltable;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/** This class manages equivalence classes of literals.
 *
 * Created by ohlbach on 20.09.2018.
 */
public class EquivalenceClasses {
    /** the list of equivalence classes */
    private ArrayList<ArrayList<Integer>> equivalenceClasses = null;
    /** maps literals to their representatives in an equivalence class */
    private HashMap<Integer,Integer> replacements = null;
    /**  a function to be called when adding the new equivalence class causes a contradiction p equiv -p */
    private BiConsumer<int[],Integer> contradictionHandler = null;

    public EquivalenceClasses(BiConsumer<int[],Integer> contradictionHandler) {
        this.contradictionHandler = contradictionHandler;}

    /** checks if there are equivalence classes
     *
     * @return true if there are no equivalence classes
     */
    public boolean isEmpty() {
        return equivalenceClasses == null;}


    /** is used to sort literals according to their absolute value */
    private static Comparator<Integer> absComparator = (lit1,lit2)-> Integer.compare(Math.abs(lit1),Math.abs(lit2));

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
        ArrayList<Integer> eqClass  = new ArrayList<Integer>(basicClause.length-2);
        for(int i = 2; i < basicClause.length; ++i) {eqClass.add(basicClause[i]);}
        eqClass.sort(absComparator);
        ArrayList<Integer> oldClass = eqClass;
        eqClass = joined(eqClass,basicClause);
        if(eqClass == null) {return false;}
        if(eqClass == oldClass) {equivalenceClasses.add(eqClass);}
        int representative = eqClass.get(0);
        for(int i = 1; i < eqClass.size(); ++i) {
            replacements.put(eqClass.get(i),representative);
            replacements.put(-eqClass.get(i),-representative);}
        return true;}

    /** joins overlapping classes if necessary
     *
     * @param eqClass a new equivalence class
     * @return either eqClass, or the class to which the class has been joined.
     */
    private ArrayList<Integer> joined(ArrayList<Integer> eqClass,int[] basicClause) {
        for(int literal : eqClass) {
            for(ArrayList<Integer> otherClass : equivalenceClasses) {
                for(int otherLiteral : otherClass) {
                    if(literal == otherLiteral) {
                        int contradicts = joinClass(otherClass,eqClass,true);
                        if(contradicts != 0) {
                            contradictionHandler.accept(basicClause,contradicts);
                            return null;}
                        return otherClass;}
                    if(literal == -otherLiteral) {
                        int contradicts = joinClass(otherClass,eqClass,false);
                        if(contradicts != 0) {
                            contradictionHandler.accept(basicClause,contradicts);
                            return null;}
                        return otherClass;}}}}
        return eqClass;}

    /** joins two overlapping classes
     *
     * @param otherClass  a previously inserted class
     * @param eqClass     the new class
     * @param positive    if true then the two classes joined a literal directly, otherwise they joined complementary literals
     * @return            0 or a predicate p with p equivalent -p
     */
    private int joinClass(ArrayList<Integer> otherClass, ArrayList<Integer> eqClass, boolean positive) {
        int oldRepresentative = otherClass.get(0);
        if(positive) {
            for(Integer literal : eqClass) {
                if(!otherClass.contains(literal)) {otherClass.add(literal);}}}
        else  {
            for(Integer literal : eqClass) {
                if(!otherClass.contains(-literal)) {otherClass.add(-literal);}}}
        otherClass.sort(absComparator);
        for(int i = 0; i < otherClass.size()-1; ++i) {
            if((int)otherClass.get(i) == -(int)otherClass.get(i+1)) {return Math.abs(otherClass.get(i));}}
        return 0;}



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
     * If the representative of a class has a truth value then all other literals get the same value.
     */
    public void completeModel(Model model) {
        for(ArrayList<Integer> eqClass : equivalenceClasses) {
            int representative = eqClass.get(0);
            int status = model.status(representative);
            if(status == 0) {continue;}
            for(int literal : eqClass) {model.setStatus(literal,status);}}}

    /** maps a literal to a string, possibly via a symboltable
     *
     * @param literal     a literal
     * @param symboltable a symboltable or null
     * @return the literal as string
     */
    private String toStringSt(Integer literal, Symboltable symboltable) {
        if(symboltable == null) {return literal.toString();}
        return symboltable.getLiteralName(literal);}

    public String toString() {
        return toString(null);}

    /** lists all equivalence classes and the replacements
     *
     * @param symboltable a symboltable
     * @return all equivalence classes as string
     */
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        if(equivalenceClasses.isEmpty()) {return "";}
        st.append("Equivalence Classes:\n");
        for(ArrayList<Integer> eqClass : equivalenceClasses) {
            st.append(toStringSt(eqClass.get(0),symboltable)).append (" = ");
            for(int i = 1; i < eqClass.size()-1; ++i) {st.append(toStringSt(eqClass.get(i),symboltable)).append(" = ");}
            st.append(toStringSt(eqClass.get(eqClass.size()-1),symboltable)).append("\n");}
        st.append("Replacements:\n");
            for(Map.Entry entry : replacements.entrySet()) {
                st.append(toStringSt((Integer)entry.getKey(),symboltable)).
                        append(" -> ").
                        append(toStringSt((Integer)entry.getValue(),symboltable)).
                        append("\n");}
        return st.toString();}

}
