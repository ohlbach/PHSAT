package Datastructures.Theory;

import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.function.Consumer;

/**
 * Created by ohlbach on 20.11.2019.
 */
public class Transformers {

    /** turns all equivalences in the basicClauseList into equivalence classes
     *
     * @return either the new EquivalenceClasses, or Unsatisfiable if p == -p was encountered
     */
    public static Object prepareEquivalences(ArrayList<int[]> basicClauses, Symboltable symboltable) {
        Result[] result = new Result[]{null};
        EquivalenceClassesOld equivalenceClasses = new EquivalenceClassesOld(symboltable,
                ((literal, origin) -> {
                    result[0] = new Unsatisfiable(""+literal+" == "+-literal + " in equivalence clause " + origin.getInt(0));}));
        for(int[] clause : basicClauses) {
            equivalenceClasses.addEquivalenceClass(clause);
            if(result[0] != null) {return result[0];}}
        return equivalenceClasses;}

    /** transforms all disjunctions in the basic clause list into clauses and applies the handler to the new clauses.
     * If there are equivalence classes then the literals are mapped to the representative of the equivalence class.
     * Double literals are removed.<br>
     * Tautologies are ignored. <br>
     * A resulting clause may be a unit clause.
     *
     * @param disjunctions       the list of input clauses.
     * @param equivalenceClasses null or a set of equivalence classes of literals
     * @param handler            for dealing with the new clause.
     * @return Unsatisfiable if the clause is empty, otherwise null
     */
    public static Unsatisfiable prepareDisjunctions(ArrayList<int[]> disjunctions, int[] id, Symboltable symboltable,
                                                    boolean trackReasoning,
                                                    Model model, EquivalenceClassesOld equivalenceClasses,
                                                    Consumer<Clause> handler) {
        for(int[] basicClause : disjunctions) {
            Unsatisfiable result = prepareDisjunction(basicClause,id,symboltable, trackReasoning, model, equivalenceClasses,handler);
            if(result != null) return result;}
        return null;}

    /** turns a single basic clause into a Clause datastructure and applies the handler to it.
     * Literals are replaced by their representative in the equivalence classes.<br>
     * Double literals are removed.<br>
     * Tautologies are ignored.
     *
     * @param basicClause [id,type,lit1,...] (the type is ignored)
     * @param id      for specifying the id of the new clause
     * @param equivalenceClasses  null or equivalence classes of literals
     * @param handler     for treating the new clause
     * @return Unsatisfiable if the clause is empty, otherwise null
     */
    private static Unsatisfiable prepareDisjunction(int[] basicClause, int[] id, Symboltable symboltable, boolean trackReasoning,
                                           Model model, EquivalenceClassesOld equivalenceClasses,
                                           Consumer<Clause> handler) {
        Clause clause = new Clause(++id[0],basicClause.length-2);
        IntArrayList origins = trackReasoning ? IntArrayList.wrap(new int[]{basicClause[0]}) : null;
        for(int i = 2; i < basicClause.length;++i) {
            int originalLiteral =  basicClause[i];
            int literal = originalLiteral;
            if(equivalenceClasses != null) {
                literal = equivalenceClasses.mapToRepresentative(originalLiteral);
                if(trackReasoning && literal != originalLiteral) {
                    IntArrayList origin = equivalenceClasses.mapToOrigins(originalLiteral);
                    if(origin != null) {origins.addAll(origin);}}}
            if(model.isFalse(literal)) {
                if(trackReasoning) {origins.addAll(model.getOrigin(literal));}
                continue;}
            clause.add(new CLiteral(literal,clause,i-2));}
        clause.origins = origins;
        if(clause.isEmpty()) return new Unsatisfiable(model,basicClause,symboltable,origins);
        if(!clause.hasComplementaries()) {
            clause.removeDoubles();
            clause.setStructure();
            handler.accept(clause);}
        return null;}

    /** The handler is applied to all conjuncts in the basicClauseList.
     * If there are equivalence classes then the literals are mapped to the representative of the equivalence class.
     *
     * @param conjunctions         the list of input conjunctions.
     * @param symboltable          null or a symboltable
     * @param equivalenceClasses   null or a set of equivalence classes of literals
     * @param model                for inserting the conjuncts
     * @return                     null or an Unsatisfiable object.
     */
    public static Unsatisfiable prepareConjunctions(ArrayList<int[]> conjunctions, Symboltable symboltable, EquivalenceClassesOld equivalenceClasses, Model model) {
        for(int[] basicClause : conjunctions) {
            for(int i = 2; i < basicClause.length; ++i) {
                int originalLiteral =  basicClause[i];
                int literal = originalLiteral;
                IntArrayList origin = null;
                if(equivalenceClasses != null) {
                    literal = equivalenceClasses.mapToRepresentative(originalLiteral);
                    if(literal != originalLiteral) origin = equivalenceClasses.mapToOrigins(originalLiteral);}
                IntArrayList origins = IntArrayList.wrap(new int[]{basicClause[0]});
                if(origin != null) {origins.addAll(origin);}
                if(model.isFalse(literal)) {
                    return new Unsatisfiable(model,literal,symboltable,origins);}
                else {model.add(literal,origins);}}}
        return null;}



    /** transforms all exclusive-or clauses in the xors into normal clauses and applies the handler to them.
     *  Example: p xor q xor r yields the clauses: <br>
     *  1. p,q,r   (one of the literals must be true)<br>
     *  2. -p,-q   (if p ist true then q must be false and vice versa)<br>
     *  3. -p,-r   (if p ist true then r must be false and vice versa)<br>
     *  4. -q,-r   (if q ist true then r must be false and vice versa)
     *
     * @param xors    the input clauses
     * @param equivalenceClasses null or some equivalence classes of literals
     * @param handler            for dealing with the resulting clauses
     */
    public static void prepareXors(ArrayList<int[]> xors, int[] id, Symboltable symboltable, boolean trackReasoning,
                                   Model model,
                                   DisjointnessClasses disjointnessClasses,
                                   EquivalenceClassesOld equivalenceClasses, Consumer<Clause> handler) {
        for(int[] basicClause : xors) {
            prepareDisjunction(basicClause,id,symboltable,trackReasoning,model, equivalenceClasses ,handler);
            disjointnessClasses.addDisjointnessClass(basicClause,equivalenceClasses);}}

    /** transforms all disjoint clauses in the basicClauseList into normal clauses and applies the handler to them.
     * Disjoints are like XORs, except that all literals may be false.
     *
     * @param disjoints    the input clauses
     * @param equivalenceClasses null or some equivalence classes of literals
     */
    public static void prepareDisjoints(ArrayList<int[]> disjoints,
                                        DisjointnessClasses disjointnessClasses,
                                        EquivalenceClassesOld equivalenceClasses) {
        for(int[] basicClause : disjoints) {
            disjointnessClasses.addDisjointnessClass(basicClause,equivalenceClasses);}}


}


