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

    private static boolean allMarked(Clause clause, int timestamp) {
        for(CLiteral cLiteral : clause.cliterals) {if(cLiteral.timestamp != timestamp) {return false;}}
        return true;}


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
        int size = clause.size();
        int timestamp = ++clauseList.timestamp;
        clauseList.timestamp += size+1;
        for(CLiteral cliteral : clause.cliterals) {
            if(clauseList.stream(cliteral.literal,implicationDAG,false).
                    anyMatch(clit -> {
                        Clause otherClause = clit.clause;
                        if(otherClause.size() > size) {return false;}
                        clit.timestamp = timestamp;
                        if(otherClause.timestamp >= timestamp) {++otherClause.timestamp;}
                        else                                   {otherClause.timestamp = timestamp;}
                        if(otherClause.timestamp - timestamp >= otherClause.size()-1 && allMarked(otherClause,timestamp)) {
                            return true;}
                        return false;})){
                return null;}}

        int length = clause.size();   // now we try resolution.
        for(int i = 0; i < length; ++i) {
            if(clauseList.streamContradicting(clause.cliterals.get(i).literal,implicationDAG).
                    anyMatch(clit -> {
                        clit.timestamp = timestamp;
                        Clause otherClause = clit.clause;
                        return otherClause.timestamp - timestamp >= otherClause.size()-2 && allMarked(otherClause,timestamp);})) {
                clause.removeLiteralAtPosition(i);
                --i;
                --length;}}
        return clause;}



    /** deletes all clauses which are subsumed by the given clause(with the implication graph).<br/>
     *
     * @param clause           the clause which operates on the other clauses
     * @param clauseList       the clause list with the clause
     * @param implicationDAG the implication graph
     * @return the number of deleted clauses.
     */
    public static int[] subsumeAndResolve(Clause clause, ClauseList clauseList, ImplicationDAG implicationDAG) {
        int size = clause.size();
        int size1 = size-1;
        int timestamp = clauseList.timestamp+1;
        clauseList.timestamp += size+1;
        ArrayList<Object> toBeDeleted = new ArrayList<>();
        for(int i = 0; i < size; ++i) {
            int j = i;
            clauseList.stream(clause.cliterals.get(i).literal,implicationDAG,true).
                    forEach(clit -> {
                        Clause otherClause = clit.clause;
                        if(otherClause.size() < size) {return;}
                        if(j == 0) {otherClause.timestamp = timestamp; return;}
                        if(j == size1) {
                            if(otherClause.timestamp == timestamp+j-1) {
                                toBeDeleted.add(otherClause);
                                otherClause.timestamp = 0;}
                            return;}
                        if(otherClause.timestamp == timestamp+j-1) {otherClause.timestamp = timestamp+j;}});}
        for(Object cl : toBeDeleted) {clauseList.removeClause((Clause)cl);}
        int subsumed = toBeDeleted.size();

        toBeDeleted.clear();
        for(CLiteral cLiteral : clause.cliterals) {
            clauseList.streamContradicting(cLiteral.literal,implicationDAG).
                    forEach(clit -> {
                        if(clit.clause.timestamp == timestamp + size1) {toBeDeleted.add(clit);}});}
        for(Object cl : toBeDeleted) {clauseList.removeLiteral((CLiteral) cl);}
        int deleted = toBeDeleted.size();
        return (subsumed + deleted > 0) ? new int[]{subsumed,deleted} : null;}
    }




