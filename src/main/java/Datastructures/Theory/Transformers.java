package Datastructures.Theory;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;

import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Created by ohlbach on 20.11.2019.
 */
public class Transformers {

    /** transforms all disjunctions in the basic clause list into clauses and applies the handler to the new clauses.
     * If there are equivalence classes then the literals are mapped to the representative of the equivalence class.
     * Double literals are removed.<br>
     * Tautologies are ignored. <br>
     * A resulting clause may be a unit clause.
     *
     * @param basicClauseList    the list of input clauses.
     * @param equivalenceClasses null or a set of equivalence classes of literals
     * @param handler            for dealing with the new clause.
     */
    public static void prepareDisjunctions(BasicClauseList basicClauseList, EquivalenceClasses equivalenceClasses,
                                           Consumer<Clause> handler) {
        for(int[] basicClause : basicClauseList.disjunctions) {
            prepareDisjunction(basicClause,"",equivalenceClasses,handler);}}

    /** turns a single basic clause into a Clause datastructure and applies the handler to it.
     * Literals are replaced by their representative in the equivalence classes.<br>
     * Double literals are removed.<br>
     * Tautologies are ignored.
     *
     * @param basicClause [id,type,lit1,...] (the type is ignored)
     * @param prefix      for forming the name of the new clause
     * @param equivalenceClasses  null or equivalence classes of literals
     * @param handler     for treating the new clause
     */
    private static void prepareDisjunction(int[] basicClause, String prefix, EquivalenceClasses equivalenceClasses,
                                           Consumer<Clause> handler) {
        Clause clause = new Clause(prefix+basicClause[0],basicClause.length-2);
        for(int i = 2; i < basicClause.length;++i) {
            int literal =  basicClause[i];
            if(equivalenceClasses != null) literal = equivalenceClasses.mapToRepresentative(literal);
            clause.add(new CLiteral(literal,clause,i-2));}
        clause.removeDoubles();
        clause.setStructure();
        if(!clause.hasComplementaries()) {handler.accept(clause);}}

    /** The handler is applied to all conjuncts in the basicClauseList.
     * If there are equivalence classes then the literals are mapped to the representative of the equivalence class.
     *
     * @param basicClauseList      the list of input clauses.
     * @param equivalenceClasses   null or a set of equivalence classes of literals
     * @param handler              for dealing with the conjuncts
     */
    public static void prepareConjunctions(BasicClauseList basicClauseList, EquivalenceClasses equivalenceClasses,
                                           Consumer<Integer> handler) {
        for(int[] basicClause : basicClauseList.conjunctions) {
            for(int i = 2; i < basicClause.length; ++i) {
                int literal =  basicClause[i];
                if(equivalenceClasses != null) literal = equivalenceClasses.mapToRepresentative(literal);
                handler.accept(literal);}}}

    /** transforms all exclusive-or clauses in the basicClauseList into normal clauses and applies the handler to them.
     *  Example: p xor q xor r yields the clauses: <br>
     *  1. p,q,r   (one of the literals must be true)<br>
     *  2. -p,-q   (if p ist true then q must be false and vice versa)<br>
     *  3. -p,-r   (if p ist true then r must be false and vice versa)<br>
     *  4. -q,-r   (if q ist true then r must be false and vice versa)
     *
     * @param basicClauseList    the input clauses
     * @param equivalenceClasses null or some equivalence classes of literals
     * @param handler            for dealing with the resulting clauses
     */
    public static void prepareXors(BasicClauseList basicClauseList, EquivalenceClasses equivalenceClasses, Consumer<Clause> handler) {
        for(int[] basicClause : basicClauseList.xors) {
            prepareDisjunction(basicClause,"X",equivalenceClasses,handler);
            prepareDisjoint(basicClause,equivalenceClasses,handler);}}

    /** transforms all disjoint clauses in the basicClauseList into normal clauses and applies the handler to them.
     * Disjoints are like XORs, except that all literals may be false.
     *
     * @param basicClauseList    the input clauses
     * @param equivalenceClasses null or some equivalence classes of literals
     * @param handler            for dealing with the resulting clauses
     */
    public static void prepareDisjoints(BasicClauseList basicClauseList, EquivalenceClasses equivalenceClasses, Consumer<Clause> handler) {
        for(int[] basicClause : basicClauseList.disjoints) {
            prepareDisjoint(basicClause, equivalenceClasses, handler);}}

    /** This method generates the disjointness clauses for XORs and Disjoints.
     * Example: p disjoint q disjoint r <br>
     * 1. -p,-q  (if p is true then q must be false and vice versa)<br>
     * 1. -p,-r  (if p is true then r must be false and vice versa)<br>
     * 1. -q,-r  (if q is true then r must be false and vice versa)<br>
     *
     * Literals are replaced by their representatives in equivalence classes.
     * The resulting clauses may become unit clauses, due to merging
     *
     * @param basicClause        [number,type,lit1,...]
     * @param equivalenceClasses  null or some equivalence classes
     * @param handler            for dealing with the resulting clauses.
     */
    private static void prepareDisjoint(int[] basicClause, EquivalenceClasses equivalenceClasses, Consumer<Clause> handler) {
        int size = basicClause.length;
        int counter = 0;
        for(int i = 2; i < size-1;++i) {
            int literal1 = -basicClause[i];
            if(equivalenceClasses != null) {literal1 = equivalenceClasses.mapToRepresentative(literal1);}
            for(int j = i+1; j < size; ++j) {
                int literal2 = -basicClause[j];
                if(equivalenceClasses != null) {literal2 = equivalenceClasses.mapToRepresentative(literal2);}
                if(literal1 == -literal2) {continue;}
                Clause clause = new Clause("D"+basicClause[0] + "_" + ++counter, 2);
                clause.add(new CLiteral<Clause>(literal1,clause,0));
                if(literal1 != literal2) {clause.add(new CLiteral<Clause>(literal2,clause,1));}
                clause.setStructure();
                handler.accept(clause);}}}


    /** This method turns equivalences in the input into an EquivalenceClasses object.
     * A contradiction may arise like in the following example:<br>
     * 1. p equiv q <br>
     * 2. q equiv -p <br>
     * This causes p equiv -p which is a contradiction.
     *
     * @param basicClauseList       the input clauses with equivalences
     * @param contradictionHandler  for dealing with contradictions
     * @return                      the new equivalence classes object or null, if a contradiction was found.
     */
    public static EquivalenceClasses prepareEquivalences(BasicClauseList basicClauseList, BiConsumer<int[],Integer> contradictionHandler) {
         EquivalenceClasses equivalenceClasses = new EquivalenceClasses(contradictionHandler);
        for(int[] basicClause : basicClauseList.equivalences) {
            if(!equivalenceClasses.addEquivalenceClass(basicClause)) {return null;}}
        return equivalenceClasses;}
    }

