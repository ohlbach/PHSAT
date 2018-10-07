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

    /** checks if the given clause is subsumed (possibly via the implication graph). <br/>
     * If not, all literals which can be resolved away (possibly via the implication graph) are removed.<br/>
     * The clause must not yet be integrated in the clause list.
     *
     * @param clause      the clause to be checked
     * @param clauseList  the other clauses
     * @param implicationDAG the implication graph
     * @return  null if the clause is subsumed, otherwise the possibly simplified clause
     */
    public static Clause subsumedAndResolved(Clause clause, ClauseList clauseList, ImplicationDAG implicationDAG) {
        int size = clause.size();
        int timestamp = clauseList.timestamp;
        clauseList.timestamp += size;
        assert size > 1;
        CLiteral cLiteral = clause.cliterals.get(0);
        synchronized (implicationDAG) {
            clauseList.stream(clause.cliterals.get(0).literal,implicationDAG,false).
                forEach(clit -> {
                    Clause otherClause = clit.clause;
                    if(otherClause != clause && otherClause.size() <= size) {otherClause.timestamp = timestamp;}});}


        int size1 = size-1;
        for(int i = 1; i < size; ++i) {
            int j = i;
            synchronized (implicationDAG) {
                if(clauseList.stream(clause.cliterals.get(i).literal,implicationDAG,false).
                    anyMatch(clit -> {
                        Clause otherClause = clit.clause;
                        if(otherClause.timestamp - j == timestamp) {
                            if(j == size1) {return true;}   // subsumed
                            otherClause.timestamp = timestamp + j;}
                        return false;})) {
                return null;}}}


        for(int i = 0; i < clause.size(); ++i) {
            int j = i;
            synchronized (implicationDAG) {
                if(clauseList.stream(clause.cliterals.get(i).literal,implicationDAG,false).
                    anyMatch(clit -> {
                        Clause otherClause = clit.clause;
                        return otherClause.timestamp - timestamp == otherClause.size()-2;}))
                clause.removeLiteralAtPosition(i);
                --i;}}

        return clause;}



    /** deletes all clauses which are subsumed by the given clause(with the implication graph) and resolves literals by replacement resolution.
     *
     * @param clause           the clause which operates on the other clauses
     * @param clauseList       the clause list with the clause
     * @param implicationDAG the implication graph
     */
    public static void subsumesAndResolves(Clause clause, ClauseList clauseList,
                                             ImplicationDAG implicationDAG) {
        int size = clause.size();
        int timestamp = clauseList.timestamp;
        clauseList.timestamp += size;
        assert size > 1;
        CLiteral cLiteral = clause.cliterals.get(0);
        synchronized (implicationDAG) {
            clauseList.stream(clause.cliterals.get(0).literal,implicationDAG,true).
                forEach(clit -> {
                    Clause otherClause = clit.clause;
                    if(otherClause != clause && otherClause.size() >= size) {otherClause.timestamp = timestamp;}});}

        ArrayList<Clause> subsumed = new ArrayList<>();
        int size1 = size-1;
        for(int i = 1; i < size; ++i) {
            int j = i;
            synchronized (implicationDAG) {
                clauseList.stream(clause.cliterals.get(i).literal,implicationDAG,true). // ???
                    forEach(clit -> {
                        Clause otherClause = clit.clause;
                        if(otherClause.timestamp - j == timestamp) {
                            if(j == size1) {subsumed.add(otherClause);}   // subsumed
                            else{otherClause.timestamp = timestamp + j;}}});}}


        for(Clause subs : subsumed) {clauseList.removeClause(clause);}

        ArrayList<CLiteral> resolved = new ArrayList<>();
        int size2 = size - 2;
        for(int i = 0; i < size; ++i) {
            int j = i;
            synchronized (implicationDAG) {
                clauseList.streamContradicting(clause.cliterals.get(i).literal,implicationDAG).
                    forEach(clit -> {
                        Clause otherClause = clit.clause;
                        if(otherClause != clause && otherClause.timestamp - timestamp == size2) {
                            resolved.add(clit);}});}}

        for(CLiteral cliteral : resolved) {
            clauseList.removeLiteral(cliteral);}
        }







}



