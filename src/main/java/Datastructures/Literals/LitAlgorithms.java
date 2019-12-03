package Datastructures.Literals;

import Algorithms.Algorithms;
import Datastructures.Clauses.Clause;
import Datastructures.Theory.ImplicationDAG;
import Datastructures.Theory.Model;
import Management.Monitor;
import Utilities.Applier;

import java.util.ArrayList;
import java.util.Iterator;
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


    /** This method checks if the given clause is subsumed by some other clause in the literal index
     *
     * @param clause        the clause to be checked
     * @param literalIndex  the index mapping literals to occurrences in clauses
     * @param timestamp     an incremented timestamp
     * @return              either a subsumer, or null
     */
    public static Clause isSubsumed(Clause clause, LiteralIndexSorted literalIndex, int timestamp) {
        int size = clause.size();
        for(CLiteral cliteral : clause) {
            Iterator<CLiteral<Clause>> iterator = literalIndex.iteratorTo(cliteral.literal,size);
            while(iterator.hasNext()) {
                CLiteral<Clause> otherLiteral = iterator.next();
                Clause otherClause = otherLiteral.clause;
                if(clause == otherClause) {continue;}
                if(otherClause.timestamp < timestamp) {otherClause.timestamp = timestamp; continue;}
                if(otherClause.timestamp - timestamp == otherClause.size()-2) {return otherClause;}
                ++otherClause.timestamp;}}
        return null;}

    /** This method searches all clauses in the literal index which are subsumed by the given clause
     *
     * @param clause       the subsumer clause
     * @param literalIndex an index mapping literals to occurrences in clauses
     * @param timestamp    an incremented timestamp
     * @param subsumed     collects all subsumed clauses
     */
    public static void subsumes(Clause clause, LiteralIndexSorted literalIndex, int timestamp, ArrayList<Clause> subsumed) {
        int size = clause.size();
        int difference = size-2;
        for(CLiteral cliteral : clause) {
            Iterator<CLiteral<Clause>> iterator = literalIndex.iteratorFrom(cliteral.literal,size);
            while(iterator.hasNext()) {
                CLiteral<Clause> otherLiteral = iterator.next();
                Clause otherClause = otherLiteral.clause;
                if(clause == otherClause) {continue;}
                if(otherClause.timestamp < timestamp) {otherClause.timestamp = timestamp; continue;}
                if(otherClause.timestamp - timestamp == difference) {
                    subsumed.add(otherClause);
                    otherClause.timestamp = 0;}
                ++otherClause.timestamp;}}}


    /** This method checks if a literal in the given clause can be removed by replacement resolution with another clause in the literal index.
     *
     * @param clause        the clause to be checked
     * @param literalIndex  the index mapping literals to occurrences in clauses
     * @param timestamp     an incremented timestamp
     * @return              [cLiteral,otherClause], or null
     */
    public static Object[] replacementResolutionBackwards(Clause clause, LiteralIndexSorted literalIndex, int timestamp) {
        int size = clause.size();
        for(CLiteral cliteral : clause) {
            Iterator<CLiteral<Clause>> iterator = literalIndex.iteratorTo(cliteral.literal,size);
            while(iterator.hasNext()) {
                CLiteral<Clause> otherLiteral = iterator.next();
                Clause otherClause = otherLiteral.clause;
                if(clause == otherClause) {continue;}
                if(otherClause.timestamp < timestamp) {otherClause.timestamp = timestamp;}
                else {++otherClause.timestamp;}}}
        for(CLiteral cliteral : clause) {
            Iterator<CLiteral<Clause>> iterator = literalIndex.iteratorTo(-cliteral.literal,size);
            while(iterator.hasNext()) {
                Clause otherClause = iterator.next().clause;
                int otherTimestamp = otherClause.timestamp;
                if(otherTimestamp >= timestamp && otherTimestamp - timestamp == otherClause.size()-2) {
                    return new Object[]{cliteral,otherClause};}}}
        return null;}

    /** This method checks if a literal in the given clause can be removed vy replacement resolution with another clause in the literal index.
     *
     * @param clause        the clause to be checked
     * @param literalIndex  the index mapping literals to occurrences in clauses
     * @param timestamp     an incremented timestamp
     * @param resolvents    a list of cLiterals to be removed.
     */
    public static void replacementResolutionForward(Clause clause, LiteralIndexSorted literalIndex, int timestamp,
                                                        ArrayList<CLiteral<Clause>> resolvents) {
        int size = clause.size();
        int difference = size-2;
        for(CLiteral cliteral : clause) {
            Iterator<CLiteral<Clause>> iterator = literalIndex.iteratorFrom(cliteral.literal,size);
            while(iterator.hasNext()) {
                CLiteral<Clause> otherLiteral = iterator.next();
                Clause otherClause = otherLiteral.clause;
                if(clause == otherClause) {continue;}
                if(otherClause.timestamp < timestamp) {otherClause.timestamp = timestamp;}
                else {++otherClause.timestamp;}}}
        for(CLiteral cliteral : clause) {
            Iterator<CLiteral<Clause>> iterator = literalIndex.iteratorTo(-cliteral.literal,size);
            while(iterator.hasNext()) {
                CLiteral<Clause> otherLiteral = iterator.next();
                Clause otherClause = otherLiteral.clause;
                int otherTimestamp = otherClause.timestamp;
                if(otherTimestamp >= timestamp && otherTimestamp - timestamp == difference) {
                    resolvents.add(otherLiteral);
                    otherClause.timestamp = 0;}}}}

    /** The method checks if the given literal or its negation are in the litrals
     *
     * @param cLiterals a list of CLiterals
     * @param literal   a literal
     * @return +1 if cLiterals contains literal, -1 if cLiterals contains -literal, otherwise 0
     */
    public static int contains(ArrayList<CLiteral<Clause>> cLiterals, int literal) {
        for(CLiteral<Clause> cLit : cLiterals) {
            int lit = cLit.literal;
            if(lit ==  literal) {return +1;}
            if(lit == -literal) {return -1;}}
        return 0;}

    /** This method generates a resolvent with the given resolution literals.
     *  Double literals are avoided. A tautology is not generated
     *
     * @param literal1 the first parent literal
     * @param literal2 the second parent literal
     * @return the new resolvent, or null if it would be a tautology
     */
    public static Clause resolve(CLiteral<Clause> literal1, CLiteral<Clause> literal2) {
        Clause parent1 = literal1.clause; Clause parent2 = literal2.clause;
        ArrayList<CLiteral<Clause>> literals = new ArrayList<>(parent1.size()+parent2.size()-2);
        for(CLiteral<Clause> lit1 : parent1) {if(lit1 != literal1) {literals.add(new CLiteral<Clause>(lit1.literal));}}
        for(CLiteral<Clause> lit2 : parent2) {
            if(lit2 != literal2) {
                switch(contains(literals,lit2.literal)) {
                    case -1: return null; // tautology
                    case  0: literals.add(new CLiteral<Clause>(lit2.literal));}}}
        return new Clause(parent1.id+"+"+parent2.id,literals);}
}
