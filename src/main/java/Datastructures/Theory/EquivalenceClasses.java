package Datastructures.Theory;

import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Utilities.IntegerQueue;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import it.unimi.dsi.fastutil.ints.IntComparator;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/** This class manages equivalence classes of literals.
 *
 * Created by ohlbach on 20.09.2018.
 */
public class EquivalenceClasses {
    /** the list of equivalence classes */
    private ArrayList<IntArrayList> equivalenceClasses = new ArrayList<>();
    /** For each equivalence class: the list of original basic clause ids which caused the equivelence */
    private ArrayList<IntArrayList> origins = null;
    /** maps literals to their representatives in an equivalence class */
    private HashMap<Integer,Integer> replacements = new HashMap<>();
    /** maps replacements to their origins */
    private HashMap<Integer,IntArrayList> originMap = null;
    /**  a function to be called when adding the new equivalence class causes a contradiction p equiv -p */
    private BiConsumer<IntArrayList,IntArrayList> contradictionHandler = null;

    private Symboltable symboltable;
    /** If true then origins must be filled */
    private boolean trackReasoning;

    /** creates empty equivalence classes
     *
     * @param symboltable          maps integers to names
     * @param contradictionHandler for reporting contradictions
     */
    public EquivalenceClasses(boolean trackReasoning,Symboltable symboltable,BiConsumer<IntArrayList,IntArrayList> contradictionHandler) {
        this.trackReasoning = trackReasoning;
        this.symboltable = symboltable;
        this.contradictionHandler = contradictionHandler;}

    /** This constructor clones the equivalence classes of another instance
     *
     * @param eqClasses  another instance of EquivalenceClasses, typically coming from the Preparer
     * @param contradictionHandler  for dealing with contradictions in new equivalence classes added later.
     */
    public EquivalenceClasses(EquivalenceClasses eqClasses, BiConsumer<IntArrayList,IntArrayList> contradictionHandler) {
        this.trackReasoning = eqClasses.trackReasoning;
        this.symboltable = eqClasses.symboltable;
        this.contradictionHandler = contradictionHandler;
        if(!eqClasses.equivalenceClasses.isEmpty()) {
            for(IntArrayList eqClass : eqClasses.equivalenceClasses) {
                equivalenceClasses.add(eqClass.clone());}
            replacements = (HashMap)eqClasses.replacements.clone();}
        if(eqClasses.origins != null) {
            for(IntArrayList origin : eqClasses.origins) {
                equivalenceClasses.add(origin.clone());}}}


    /** checks if there are equivalence classes
     *
     * @return true if there are no equivalence classes
     */
    public boolean isEmpty() {
        return equivalenceClasses == null;}



    /** is used to sort literals according to their absolute value */
    private static IntComparator absComparator = (i,j) -> Integer.compare(Math.abs(i),Math.abs(j));




    /** turns a basicClause into an equivalence class.
     * Overlapping classes are joined.
     * The classes are sorted according to the absolute value of the literals.
     * The smallest predicate number becomes the representative of the class.
     *
     * @param basicClause [clause-problemId,typenumber,literal1,...]
     */
    public boolean addEquivalenceClass(int[] basicClause, IntArrayList origin) {
        assert basicClause.length > 2;
        if(basicClause.length == 3) {return true;}
        if(equivalenceClasses == null) {
            equivalenceClasses = new ArrayList<>();
            replacements = new HashMap<>();}

        int literal = basicClause[2];
        for(int i = 3; i < basicClause.length; ++i) {
            if(!addEquivalence(literal,basicClause[i],origin)) {return false;}}
        return true;}

    /** turns the two literals into an equivalence class.
     * Overlapping classes are joined.
     * The classes are sorted according to the absolute value of the literals.
     * The smallest predicate number becomes the representative of the class.
     *
     * @param literal1 the first literal
     * @param literal2 the second literal
     * @param origin   null or the list of basicClause ids which caused the equivalence.
     * @return true if the equivalences was added, false if a contradiction was found
     */
    public boolean addEquivalence(int literal1, int literal2, IntArrayList origin) {
        if(Math.abs(literal2) < Math.abs(literal1)) {
            int dummy = literal1; literal1 = literal2; literal2=dummy;}
        if(literal1 < 0) {literal1 = -literal1; literal2 = -literal2;}

        if(equivalenceClasses == null) {
            equivalenceClasses = new ArrayList<>();
            replacements = new HashMap<>();
            if(trackReasoning) {
                origins = new ArrayList<>();
                originMap = new HashMap<>();}}

        for(int i = 0; i < equivalenceClasses.size(); ++i) {
            IntArrayList eqClass = equivalenceClasses.get(i);
            IntArrayList orig = (trackReasoning) ? origins.get(i) : null;
             switch(joinIfPossible(eqClass,orig,literal1,literal2, origin)) {
                 case -1: return false;
                 case +1: return true;}}
        IntArrayList eClass = new IntArrayList(2);
        equivalenceClasses.add(eClass);
        eClass.add(literal1); eClass.add(literal2);
        replacements.put(literal2,literal1);
        replacements.put(-literal2,-literal1);
        if(trackReasoning) {
            IntArrayList orig = origin.clone();
            origins.add(orig);
            originMap.put(literal2,orig);
            originMap.put(-literal2,orig);}
        return true;}


    /** tries to join the equivalence literal1 == literal2 into the existing equivalence class.
     *  If the equivalence contradicts an existing equivalence, the contradiction handler is called.
     *
     * @param eqClass   an existing equivalence class
     * @param origins  either null or the list of basicClause ids which caused the old equivalence
     * @param literal1  the first literal
     * @param literal2  the second literal
     * @param origin  either null or the list of basicClause ids which caused the new equivalence
     * @return  +1 if the literals were joined, -1 if a contradiction was found, and 0 otherwise
     */
    int joinIfPossible(IntArrayList eqClass, IntArrayList origins, int literal1, int literal2, IntArrayList origin) {
        int found1 = 0; int found2 = 0;
        for(int literal : eqClass) {
            if(literal == literal1) {
                found1 = +1;
                if(found2 == +1) {return 1;}
                if(found2 == -1) {eqClass.add(literal2); break;}}
            if(literal == -literal1) {
                found1 = -1;
                if(found2 == -1) {return 1;}
                if(found2 == +1) {eqClass.add(literal2); break;}}
            if(literal == literal2) {
                found2 = +1;
                if(found1 == +1) {return 1;}
                if(found1 == -1) {eqClass.add(literal1); break;}}
            if(literal == -literal2) {
                found2 = -1;
                if(found1 == -1) {return 1;}
                if(found1 == +1) {eqClass.add(literal2); break;}}}

        if(found1 == 0 && found2 == 0) {return 0;}

        if(trackReasoning) {origins.addAll(0, origin);}

        if(found1 != 0 && found2 != 0) { // both must be different
            contradictionHandler.accept(eqClass,origins);
            return -1;}

        switch(found1) {
            case -1: eqClass.add(-literal2); break;
            case 0 : break;
            case +1: eqClass.add(literal2);  break;}
        switch(found2) {
            case -1: eqClass.add(-literal1); break;
            case 0 : break;
            case +1: eqClass.add(literal1);  break;}

        eqClass.sort(absComparator);
        int representative = eqClass.getInt(0);
        for(int i = 1; i < eqClass.size(); ++i) {
            int literal = eqClass.getInt(i);
            replacements.put(literal,representative);
            replacements.put(-literal,-representative);
            if(trackReasoning) {
                originMap.put(literal,origins);
                originMap.put(-literal,origins);}}
        return 1;}




    /** maps literals to their representative in the equivalence class.
     *
     * @param literal the literal
     * @return either the literal itself, or its representative.
     */
    public int mapToRepresentative(int literal) {
        if(replacements == null) {return literal;}
        Integer replaced = replacements.get(literal);
        return (replaced == null) ? literal : replaced;}

    /** yields the original basicClause ids for an equivalence which causes the replacements
     *
     * @param literal a literal
     * @return null or the  basicClause ids for an equivalence which causes the replacements
     */
    public IntArrayList mapOrigins(int literal) {
        return (originMap == null) ? null : originMap.get(literal);}


    /** completes a model by the equivalence classes.
     * If one of the literals in an equivalence class has a truth value, then all others get the same value.<br>
     * If none of the literals in an equivalence class has a truth value, then nothing is changed.
     *
     * @returns Unsatisfiable if two different literals in a class have already different truth values, otherwise null
     */
    public Result completeModel(Model model) {
        for(IntArrayList eqClass : equivalenceClasses) {
            int status = 0;
            int literal1 = 0;
            for(int literal : eqClass) {
                int stat = model.status(literal);
                if(stat != 0) {status = stat; literal1 = literal; break;}}
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
     * @param literalp  the literal to be made true
     * @param truths   collects the literals to be made true.
     * @return         true if some literals must be made true.
     */
    public boolean truths(int literalp, IntArrayList truths) {
        if(replacements == null || replacements.get(literalp) == null) {return false;}
        int literaln = -literalp;
        boolean found = false;
        for(IntArrayList eqClass : equivalenceClasses) {
            if(eqClass.contains(literalp)) {
                found = true;
                for(int lit : eqClass) {if(lit !=literalp) {truths.add(lit);}}}
            else {
                if(eqClass.contains(literaln)) {
                    found = true;
                    for(int lit : eqClass) {if(lit != literaln) {truths.add(-lit);}}}}
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
        for(IntArrayList eqClass : equivalenceClasses) {
            st.append(toStringSt(eqClass.getInt(0))).append (" = ");
            for(int i = 1; i < eqClass.size()-1; ++i) {st.append(toStringSt(eqClass.getInt(i))).append(" = ");}
            st.append(toStringSt(eqClass.getInt(eqClass.size()-1))).append("\n");}
        st.append("Replacements:\n");
            for(Map.Entry entry : replacements.entrySet()) {
                st.append(toStringSt((Integer)entry.getKey())).
                        append(" -> ").
                        append(toStringSt((int)entry.getValue())).
                        append("\n");}
        return st.toString();}

}
