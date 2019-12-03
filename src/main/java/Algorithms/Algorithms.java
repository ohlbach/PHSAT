package Algorithms;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Theory.ImplicationDAG;
import org.omg.CORBA.TIMEOUT;

import java.util.ArrayList;
import java.util.TreeSet;
import java.util.function.BiFunction;
import java.util.stream.Stream;

/**
 * Created by ohlbach on 18.09.2018.
 */
public class Algorithms {

/*
    public static ArrayList<CLiteral<Clause>> subsumedAndResolved(ArrayList<CLiteral<Clause>> clause, LiteralIndex<Clause> literalIndex, ImplicationDAG implicationDAG) {
        if(subsumed(clause,literalIndex,implicationDAG) != null) {return null;}
        return resolved(clause,literalIndex,implicationDAG);
    }
*/
    /** checks if the clause can be subsumed.
     *
     * @param literals       the clause to be checked
     * @param literalIndex   the literal index
     * @param implicationDAG the implication DAG
     * @return               a subsumer clause or null
     */
  /*  public static Clause subsumed(ArrayList<CLiteral<Clause>> literals, LiteralIndex<Clause> literalIndex, ImplicationDAG implicationDAG) {
        int size = literals.size();
        int timestamp = literalIndex.timestamp + 1;
        literalIndex.timestamp += size+2;
        for(CLiteral<Clause> cliteral : literals) {
            Clause subsumer = (Clause)
                implicationDAG.find(cliteral.literal,false,
                    (literal -> {
                        for(CLiteral<Clause> clit : literalIndex.getLiterals(literal)) {
                            Clause otherClause = clit.clause;
                            if(otherClause.size() > size) {continue;}
                            if(clit.timestamp < timestamp) {
                                clit.timestamp = timestamp;
                                if(otherClause.timestamp < timestamp) {otherClause.timestamp = timestamp;}
                                else {++otherClause.timestamp;}}
                            if(otherClause.timestamp - timestamp == otherClause.size()-1) {
                                return otherClause;}}
                            return null;}));
            if(subsumer != null) {return subsumer;}}
        return null;} */

    private static int[] dummy = new int[]{0};

    /** performs replacement resolution at the clause, by using the implicationDAG
     * All literals which can be resolved away by replacement resolution are removed from the clause.
     * Therefore the clause is assumed not yet to be part of a clause list and the literal index.
     *
     * @param clause         the clause to be checked
     * @param literalIndex   the literal index
     * @param implicationDAG the implication DAG
     * @return               the possibly shortened clause
     */
   /* public static ArrayList<CLiteral<Clause>> resolved(ArrayList<CLiteral<Clause>> clause, LiteralIndex<Clause> literalIndex,  ImplicationDAG implicationDAG) {
        int timestamp = literalIndex.timestamp+1;
        int[] maxTimestamp = new int[]{timestamp};
        for(CLiteral<Clause> clit1 : clause) {
            implicationDAG.apply(clit1.literal,false,(literal -> {
                for(CLiteral<Clause> clit2 :  literalIndex.getLiterals(literal)) {
                    if(clit2.timestamp < timestamp) {
                        clit2.timestamp = timestamp;
                        Clause c2 = clit2.clause;
                        if(c2.timestamp < timestamp) {c2.timestamp = timestamp;}
                        else {++c2.timestamp;
                              maxTimestamp[0] = Math.max(maxTimestamp[0],c2.timestamp);}}}}));}

        for(CLiteral<Clause> clit1 : clause) {  // new we look for resolution partners
            if((Boolean)
                implicationDAG.find(clit1.literal,true,(literal -> {
                    for(CLiteral<Clause> clit2 :  literalIndex.getLiterals(-literal)) {
                        if(clit2.timestamp - timestamp >= clit2.clause.size()-2) {return true;}}
                    return false;}))) {
                clause.remove(clit1);
                literalIndex.timestamp = maxTimestamp[0]+1;
                return clause.size() > 1 ? resolved(clause,literalIndex,implicationDAG) : clause;}}
        return clause;}*/

    /** finds all clauses which are subsumed by the given clause(with the implication graph).<br>
     *
     * @param clause           the clause which operates on the other clauses
     * @param literalIndex     the literal index
     * @param implicationDAG   the implication graph
     * @return the list of subsumed clauses, or null, if there are none of them.
     */
  /*  public static ArrayList<Clause> subsume(Clause clause, LiteralIndex<Clause> literalIndex, ImplicationDAG implicationDAG) {
        int size = clause.size();
        int timestamp = literalIndex.timestamp+1;
        literalIndex.timestamp += size+1;
        ArrayList<Clause> subsumed = new ArrayList<>();
        for(CLiteral<Clause> clit1 : clause) {
            implicationDAG.apply(clit1.literal,true, (literal2 -> {
                for(CLiteral<Clause> clit2 : literalIndex.getLiterals(literal2)) {
                    Clause otherClause = clit2.clause;
                    if(otherClause == clause || otherClause.size() < size) {continue;}
                    if(clit2.timestamp < timestamp) {clit2.timestamp = timestamp; continue;}
                    int otherTimestamp = otherClause.timestamp;
                    if(otherTimestamp < timestamp) {otherClause.timestamp = timestamp; continue;}
                    if(otherTimestamp - timestamp == otherClause.size() - 2) {
                        subsumed.add(otherClause);
                        otherClause.timestamp = 0;}
                    else {++otherClause.timestamp;}}}));}
        return subsumed.isEmpty() ? null : subsumed;}*/


    /** finds all literals for replacement resolution with the given clause(with the implication graph).<br>
     *
     * @param clause           the clause which operates on the other clauses
     * @param literalIndex     the literal index
     * @param implicationDAG the implication graph
     * @return the literals to be resolved, or null if there are none.
     */
    /*public static ArrayList<CLiteral<Clause>> resolve(Clause clause, LiteralIndex<Clause> literalIndex, ImplicationDAG implicationDAG) {
        int size = clause.size();
        int size1 = size-1;
        int timestamp = literalIndex.timestamp+1;
        literalIndex.timestamp += size+1;
        ArrayList<CLiteral<Clause>> toBeResolved = new ArrayList<>();
        for(CLiteral<Clause> clit1 : clause) {
            implicationDAG.apply(clit1.literal,true, (literal2 -> {
                for(CLiteral<Clause> clit2 : literalIndex.getLiterals(literal2)) {
                    if(clit2.timestamp < timestamp) {
                        clit2.timestamp = timestamp;
                        if(clit2.clause.timestamp < timestamp) {clit2.clause.timestamp = timestamp;}
                        else {++clit2.clause.timestamp;}}}}));}
        for(CLiteral<Clause> clit1 : clause) {
            implicationDAG.apply(clit1.literal,true, (literal2 -> {
                for(CLiteral<Clause> clit2 : literalIndex.getLiterals(-literal2)) {
                    if(clit2.timestamp < timestamp && clit2.clause.timestamp -timestamp == size - 2) {
                        toBeResolved.add(clit2);}}}));}
        return toBeResolved.isEmpty() ? null : toBeResolved;}*/


    /** checks if the clause is subsumed by the implication DAG
     * Example: p,q,r  and -p -&gt; r: subsumed
     *
     * @param clause         the clause to be simplified
     * @param implicationDAG the implication DAG
     * @return               true if the clause is subsumed by the implication DAG
     */
    public static boolean subsumedByID(ArrayList<CLiteral<Clause>> clause, ImplicationDAG implicationDAG) {
        for(CLiteral<Clause> clit1 : clause) {
            for(CLiteral<Clause> clit2 : clause) {
                return clit1 != clit2 && implicationDAG.implies(-clit1.literal,clit2.literal);}}
        return false;}

    /** simplifies a clause by means of the implication DAG.<br>
     * Example: p,q,r  and p -&gt; r: remove p
     * This is essentially a replacement resolution with the two-literal clauses in the implication DAG.
     * The clause must not be in a clause list and the the literal index.
     *
     * @param clause         the clause to be simplified
     * @param implicationDAG the implication DAG
     * @return               the number of literal removals.
     */
    public static int replacementResolutionWithID(ArrayList<CLiteral<Clause>> clause, ImplicationDAG implicationDAG) {
        int removals = 0;
        boolean again = true;
        while(again) {
            again = false;
            for(CLiteral<Clause> clit1 : clause) {
                for(CLiteral<Clause> clit2 : clause) {
                    if(clit1 != clit2 && implicationDAG.implies(clit1.literal,clit2.literal)) {
                        clause.remove(clit1);
                        ++removals;
                        again = true;
                        break;}}
                if(again) {break;}}}
        return removals;}


    /** performs all subsumptions and resolutions with an implication p -&gt; g, and its consequences in the implicationDAG
     *
     * @param from       the antecedent of the implication
     * @param to         the succedent or the implication
     * @param literalIndex     the literal index
     * @param implicationDAG  the implication DAG
     * @return  null or [subsumption clauses (ArrayList, resolution literals (TreeSet)}
     */
    /*public static Object[] simplifyWithImplication(int from, int to, LiteralIndex<Clause> literalIndex, ImplicationDAG implicationDAG) {
        ArrayList<Clause> subsumed = new ArrayList<>();
        int timestamp = literalIndex.timestamp;
        ++literalIndex.timestamp;
        implicationDAG.apply(-from,true, (q -> {
            for(CLiteral<Clause> lit : literalIndex.getLiterals(q)) {
                lit.clause.timestamp = timestamp;}}));
        implicationDAG.apply(to,true, (q -> {
            for(CLiteral<Clause> lit : literalIndex.getLiterals(q)) {
                Clause clause = lit.clause;
                if(clause.timestamp == timestamp) {subsumed.add(clause); clause.timestamp = 0;}
                else {clause.timestamp = timestamp;}}}));

        TreeSet<CLiteral> toBeResolved = new TreeSet<>();
        implicationDAG.apply(from,false, (q -> {
            for(CLiteral<Clause> lit : literalIndex.getLiterals(q)) {
                if(lit.clause.timestamp == timestamp) {toBeResolved.add(lit);}}}));
        implicationDAG.apply(-to,false, (q -> {
            for(CLiteral<Clause> lit : literalIndex.getLiterals(q)) {
                if(lit.clause.timestamp == timestamp) {toBeResolved.add(lit);}}}));
        return(!subsumed.isEmpty() || !toBeResolved.isEmpty()) ? new Object[]{subsumed,toBeResolved} : null;}

*/


}



