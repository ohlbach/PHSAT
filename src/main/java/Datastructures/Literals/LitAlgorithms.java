package Datastructures.Literals;

import Algorithms.Algorithms;
import Datastructures.Clauses.Clause;
import Datastructures.Statistics.PreProcessorStatistics;
import Datastructures.Theory.ImplicationDAG;
import Datastructures.Theory.Model;
import Management.Monitor;
import Utilities.Applier;

import java.util.ArrayList;
import java.util.function.Function;
import java.util.function.IntConsumer;

/**
 * Created by ohlbach on 03.07.2019.
 */
public class LitAlgorithms {

    /** adds a new literal to the list of literals.
     * Double literals are avoided.
     * Tautologies are detected.
     * A true literal indicates a tautology
     * A false literal is ignored.
     *
     * @param literals a list of literals
     * @param literal  a new literal
     * @param model a model
     * @param constructor for constructing a new literal
     * @return null or the list of literals.
     */
    public static ArrayList<CLiteral<Clause>> addLiteral(ArrayList<CLiteral<Clause>> literals, int literal, Model model, Function<Integer,CLiteral<Clause>> constructor) {
        if(model != null) {
            if(model.isTrue(literal)) {return null;}
            if(model.isFalse(literal)) {return literals;}}
        for(CLiteral clit : literals) {
            int lit = clit.literal;
            if(lit == literal) {continue;}
            if(lit == -literal) {return null;}}
        literals.add(constructor.apply(literal));
        return literals;}


    /** This method simplifies a new clause, which is just given by a list of literals.
     *
     * The following simplifications are tried <br>
     *     - Subsumption by the implication DAG <br>
     *     - Replacement Resolution with the implication DAG <br>
     *     - Subsumption by other clauses <br>
     *     - Replacement resolution using other clauses.
     *
     * @param literals          the clause to be simplified
     * @param literalIndex      the literal index of the other clauses
     * @param implicationDAG    the implication DAG (or null)
     * @param monitor           for printing information
     * @param processId         id of the process which performed the simplification
     * @param clauseId          the clause's id
     * @param clauseStatistics  for incrementing removed clauses
     * @param replStatisticsDAG for incrementing removed literals by the implication DAG
     * @param replStatisticsClauses    for incrementing removed literals be other clauses
     * @return
     */
    public static ArrayList<CLiteral<Clause>> simplifyClause(ArrayList<CLiteral<Clause>> literals, LiteralIndex<Clause> literalIndex,
                                                             ImplicationDAG implicationDAG,
                                                             Monitor monitor, String processId, String clauseId,
                                                             Applier clauseStatistics, IntConsumer replStatisticsDAG,
                                                             Applier replStatisticsClauses) {
        int size = literals.size();
        if(size == 1) {return literals;}
        if(implicationDAG != null && !implicationDAG.isEmpty()) {
            if(Algorithms.subsumedByID(literals,implicationDAG)) { // subsumption by the implication DAG
                if(monitor != null) {
                    monitor.print(processId, "Clause " + clauseId + " is subsumed by the implication DAG.");}
                    clauseStatistics.apply();
                return null;}
            int removals = Algorithms.replacementResolutionWithID(literals,implicationDAG); // replacement resolution with the DAG
            if(removals != 0) {
                replStatisticsDAG.accept(removals);
                if(monitor != null) {
                    monitor.print(processId,"Replacement Resolution with Implication DAG removed " + removals + " literals from clause " + clauseId
                         + ".\n  clause: " + clauseId + ": " +literals.toString());}
                if(literals.size() == 1) {return literals;}}}

        if(literals.size() > 2 || implicationDAG == null) {
            Clause subsumer = Algorithms.subsumed(literals,literalIndex,implicationDAG); // subsumption by other clauses
            if(subsumer != null) {
                if(monitor != null) {
                    monitor.print(processId,"Clause " + clauseId + " " + literals.toString() + " subsumed by clause " +
                    subsumer.toString());}
                clauseStatistics.apply();
                return null;}}

        size = literals.size();
        literals = Algorithms.resolved(literals,literalIndex,implicationDAG); // replacement resolution with other clauses
        if(literals.size() < size) {
            if(monitor != null) {
                monitor.print(processId,"Replacement Resolution shortened clause " +
                            clauseId + " " + literals.toString() + " to " +
                            literals.toString());}
            replStatisticsClauses.apply();}
        return literals;}

}
