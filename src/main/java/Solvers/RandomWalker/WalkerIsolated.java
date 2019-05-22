package Solvers.RandomWalker;

import Coordinator.CentralProcessor;
import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.TreeSet;

/**
 * Created by ohlbach on 16.05.2019.
 */
public class WalkerIsolated extends Walker{

    /** parses a HashMap with key-value pairs<br>
     *
     * @param parameters  the parameters with the keys "seed", "flips", "jumpFrequency"
     * @param errors      for error messages
     * @param warnings    for warnings
     * @return            a list of HashMaps with these keys.
     */
    public static ArrayList<HashMap<String,Object>> parseParameters(HashMap<String,String> parameters, StringBuffer errors, StringBuffer warnings){
        return Walker.parseParameters(parameters,errors,warnings);}

    public static String help() {
        return Walker.help();}

    public WalkerIsolated(HashMap<String,Object> applicationParameters, CentralProcessor centralProcessor) {
        super(applicationParameters,centralProcessor);
        addImplications(); // now centralProcessor may change its clauses
    }


        /** turns the implications in the implication dag back to two-literal clauses.
         * From now on WalkerIsolated works without further communication with the rest.
         */

    void addImplications() {
        TreeSet<Pair<Integer,Integer>> pairs = new TreeSet<Pair<Integer,Integer>>();
        implicationDAG.applyToRoots((literal1 -> {
            implicationDAG.apply(literal1,true,
                    (literal2-> {
                        if(literal1 == literal2) {return;}
                        int lit1 = -literal1;
                        pairs.add(lit1 < literal2 ?
                                new ImmutablePair<Integer, Integer>(lit1,literal2):
                                new ImmutablePair<Integer, Integer>(literal2,lit1));}));}));
        int counter = 0;
        for(Pair<Integer,Integer> pair : pairs) {
            Clause clause = new Clause("P_"+counter++,2);
            clause.addCLiteralDirectly(new CLiteral(pair.getLeft()));
            clause.addCLiteralDirectly(new CLiteral(pair.getRight()));
            clauses.addClause(clause);}}





    /** generates a candidate rwModel for the clauses.
     * A predicate becomes true if it occurs in more (&ge;) clauses than its negation.
     */
    public void initializeModel() {
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            if(rwModel.status[predicate] == 0) {
                int sizep = clauses.getLiterals(predicate).size();
                int sizen = clauses.getLiterals(-predicate).size();
                if(sizep == 0 && sizen == 0) {continue;}
                rwModel.status[predicate] = (byte)(sizep >= sizen ? 1 : -1);}}}


    /** flips the truth value of the predicate and updates the predicateQueue and the falseClauses list
     *
     * @param predicate to be flipped
     */
    public void flip(int predicate) {
        ++((WalkerStatistics)statistics).RW_flips;
        flipPredicate(predicate);}

    public void updateClauseScore(Clause clause, int change) {
        int trueLiteral = 0;
        for(CLiteral lit : clause.cliterals) {
            int literal = lit.literal;
            if(rwModel.isTrue(literal)) {
                if(trueLiteral != 0) {return;} // at least two true literals: flipping changes nothing.
                trueLiteral = literal;}}
        if(trueLiteral == 0) { // clause is false
            if(change > 0) {falseClauses.add(clause);}
            else           {falseClauses.remove(clause);}
            for(CLiteral lit : clause.cliterals) {
                changeScore(Math.abs(lit.literal),change);}}
        else {  // clause is true and becomes false
            changeScore(Math.abs(trueLiteral),-change);}}





}
