package Algorithms;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Theory.ImplicationDAG;

import java.util.ArrayList;

/**
 * Created by ohlbach on 18.09.2018.
 */
public class Algorithms {

    /** checks if the given clause is subsumed (possibly via the implication DAG). <br/>
     * The clause must not yet be integrated in the clause list.
     *
     * @param clause      the clause to be checked
     * @param clauseList  the other clauses
     * @param implicationDAG the implication graph
     * @return  true if the clause is subsumed.
     */
    public static boolean isSubsumed(Clause clause, ClauseList clauseList, ImplicationDAG implicationDAG) {
        int size = clause.size();
        int timestamp = ++clauseList.timestamp;
        clauseList.timestamp += size+1;
        assert size > 1;
        for(int i = 0; i < size; ++i) {
            if(clauseList.stream(clause.cliterals.get(i).literal,implicationDAG,false).
                    anyMatch(clit -> {
                        Clause otherClause = clit.clause;
                        if(otherClause.size() > size) {return false;}
                        if(otherClause.timestamp >= timestamp) {
                            if(otherClause.timestamp - timestamp == otherClause.size()-2) {return true;}
                            else {++otherClause.timestamp;}}
                        else{otherClause.timestamp = timestamp;}
                        return false;}))
                return true;}
        return false;}

    /** tries to resolve away single literals.<br/>
     * Example: p,q,r,s<br/>
     *          -p,q,r<br/>
     * causes p to be resolved away.
     *
     * @param clause        a clause to be checked
     * @param clauseList    a clause list
     * @param implicationDAG the implication DAG
     * @return  the, possibly simplified, clause.
     */
    public static Clause resolveBackwardLiterals(Clause clause, ClauseList clauseList, ImplicationDAG implicationDAG) {
        int ts = clauseList.timestamp+1;
        int size = clause.size();
        clauseList.timestamp += size+1;
        for(int i = 0; i < size; ++i) {
            int timestamp = ts + i;
            clauseList.streamContradicting(clause.cliterals.get(i).literal,implicationDAG).
                    forEach(clit -> {
                        Clause otherClause = clit.clause;
                        if(otherClause.size() <= clause.size()) {otherClause.timestamp = timestamp;}});

            for(int k = 0; k < size; ++k) {
                if(i == k) {continue;}
                if(clauseList.stream(clause.cliterals.get(k).literal,implicationDAG,false).
                    anyMatch(clit-> {
                        Clause otherClause = clit.clause;
                        if(otherClause.timestamp >= timestamp) {
                            if(otherClause.timestamp-timestamp == otherClause.size()-2) {
                                otherClause.timestamp = 0;
                                return true;}
                            ++otherClause.timestamp;}
                        return false;})) {
                    clause.removeLiteralAtPosition(i);
                    --i; --size;
                    break;}}}
        return clause;}


    /** checks if the given clause is subsumed (possibly via the implication DAG). <br/>
     * If not, all literals which can be resolveBackwardLiterals away (possibly via the implication DAG) are removed.<br/>
     * The clause must not yet be integrated in the clause list.
     *
     * @param clause      the clause to be checked
     * @param clauseList  the other clauses
     * @param implicationDAG the implication graph
     * @return  null if the clause is subsumed, otherwise the possibly simplified clause
     */
    public static Clause subsumedAndResolved(Clause clause, ClauseList clauseList, ImplicationDAG implicationDAG) {
        if(isSubsumed(clause,clauseList,implicationDAG)) {return null;}
        return resolveBackwardLiterals(clause,clauseList,implicationDAG);
    }

    /** deletes all clauses which are subsumed by the given clause(with the implication graph).<br/>
     * The clauses must be simplified with respect to the impliation DAG.
     * That means if p -&gt; q holds, then p,q must not be in the clause.
     *
     * @param clause           the clause which operates on the other clauses
     * @param clauseList       the clause list with the clause
     * @param implicationDAG the implication graph
     * @return the number of deleted clauses.
     */
    public static int subsumes(Clause clause, ClauseList clauseList, ImplicationDAG implicationDAG) {
        int size = clause.size();
        int timestamp = clauseList.timestamp+1;
        clauseList.timestamp += size+1;
        ArrayList<Clause> toBeDeleted = new ArrayList<>();
        for(int i = 0; i < size; ++i) {
            clauseList.stream(clause.cliterals.get(i).literal,implicationDAG,true).
                    forEach(clit -> {
                        Clause otherClause = clit.clause;
                        if(otherClause.size() < size) {return;}
                        if(otherClause.timestamp >= timestamp) {
                            if(otherClause.timestamp - timestamp == clause.size()-2) {
                                toBeDeleted.add(otherClause);
                                otherClause.timestamp = -10;}
                            else {++otherClause.timestamp;}}
                        else{otherClause.timestamp = timestamp;}});}
        for(Clause cl : toBeDeleted) {clauseList.removeClause(cl);}
        return toBeDeleted.size();}




    /** tries to resolve away single literals.<br/>
     * Example: p,q,r,s<br/>
     *          -p,q,r<br/>
     * causes p to be resolved away.
     *
     * @param clause        a clause to be checked
     * @param clauseList    a clause list
     * @param implicationDAG the implication DAG
     * @return  the number of deleted literals.
     */
    public static int resolveForwardLiterals(Clause clause, ClauseList clauseList, ImplicationDAG implicationDAG) {
        int ts = clauseList.timestamp+1;
        int size = clause.size();
        clauseList.timestamp += size+1;
        ArrayList<CLiteral> toBeDeleted = new ArrayList<>();
        for(int i = 0; i < size; ++i) {
            int timestamp = ts+i;
            clauseList.streamContradicting(clause.cliterals.get(i).literal,implicationDAG).
                    forEach(clit->{if(clit.clause.size() >= size) {clit.clause.timestamp = timestamp;}});
            for(int k = 0; k < size; ++k) {
                if(i == k) {continue;}
                clauseList.stream(clause.cliterals.get(k).literal,implicationDAG,true).
                        forEach(clit -> {
                            Clause otherClause = clit.clause;
                            if(otherClause.timestamp >=  timestamp) {
                                if(otherClause.timestamp == size-2) {toBeDeleted.add(clit);}
                                else {++otherClause.timestamp;}}});}}
        for(CLiteral clit : toBeDeleted) {clauseList.removeLiteral(clit);}
        return toBeDeleted.size();}

    /** subsumes clauses and resolves literals.
     *
     * @param clause        a clause to be checked
     * @param clauseList    a clause list
     * @param implicationDAG the implication DAG
     */
    public static void resolveAndSubsume(Clause clause, ClauseList clauseList, ImplicationDAG implicationDAG) {
        subsumes(clause, clauseList, implicationDAG);
        resolveForwardLiterals(clause, clauseList, implicationDAG);}
    }



