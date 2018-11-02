package Algorithms;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
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

    private static boolean allMarked(Clause clause, int timestamp) {
        for(CLiteral cLiteral : clause.cliterals) {if(cLiteral.timestamp != timestamp) {return false;}}
        return true;}


    /** checks if the given clause is subsumed (possibly via the implication DAG). <br>
     * If not, all literals which can be resolveBackwardLiterals away (possibly via the implication DAG) are removed.<br>
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



    /** deletes all clauses which are subsumed by the given clause(with the implication graph).<br>
     *
     * @param clause           the clause which operates on the other clauses
     * @param clauseList       the clause list with the clause
     * @param implicationDAG the implication graph
     * @return the number of deleted clauses.
     */
    public static int subsume(Clause clause, ClauseList clauseList, ImplicationDAG implicationDAG) {
        int size = clause.size();
        int size1 = size-1;
        int timestamp = clauseList.timestamp+1;
        clauseList.timestamp += size+1;
        ArrayList<Clause> toBeDeleted = new ArrayList<>();
        for(int i = 0; i < size; ++i) {
            int j = i;
            clauseList.stream(clause.cliterals.get(i).literal,implicationDAG,true).
                    forEach(clit -> {
                        Clause otherClause = clit.clause;
                        if(otherClause == clause) {return;}
                        if(otherClause.size() < size) {return;}
                        if(j == 0) {otherClause.timestamp = timestamp; return;}
                        if(j == size1) {
                            if(otherClause.timestamp == timestamp+j-1) {
                                toBeDeleted.add(otherClause);
                                otherClause.timestamp = 0;}
                            return;}
                        if(otherClause.timestamp == timestamp+j-1) {otherClause.timestamp = timestamp+j;}});}
        for(Clause cl : toBeDeleted) {clauseList.removeClause(cl);}
        return toBeDeleted.size();
    }


    /** deletes all clauses which are subsumed by the given clause(with the implication graph).<br>
     *
     * @param clause           the clause which operates on the other clauses
     * @param clauseList       the clause list with the clause
     * @param implicationDAG the implication graph
     * @return the number of deleted clauses.
     */
    public static int resolve(Clause clause, ClauseList clauseList, ImplicationDAG implicationDAG) {
        int size = clause.size();
        int size1 = size-1;
        int ts = clauseList.timestamp+1;
        clauseList.timestamp += size*size+1;
        ArrayList<CLiteral> toBeDeleted = new ArrayList<>();
        for(int i = 0; i < size; ++i) {
            CLiteral cliteral = clause.cliterals.get(i);
            int timestamp = ts+i*size;
            // mark the potential resolution literals
            clauseList.streamContradicting(cliteral.literal,implicationDAG).forEach(cLit->cLit.timestamp = timestamp);
            for(int k = 0; k < size; ++k) {
                int j = k;
                Stream<CLiteral> stream = (i == k) ?
                        clauseList.streamContradicting(cliteral.literal,implicationDAG) :
                        clauseList.stream(clause.cliterals.get(k).literal,implicationDAG,true);
                if(k == 0) {stream.forEach(cLit -> {if(cLit.clause.size() >= size) {cLit.clause.timestamp = timestamp;}});
                            continue;}
                if(k == size1) {
                    stream.forEach(cLit ->{
                        Clause otherClause = cLit.clause;
                        if(otherClause.timestamp == timestamp+j-1) {
                            for(CLiteral clit : otherClause.cliterals) {
                                if(clit.timestamp == timestamp) {
                                    clit.timestamp = 0;
                                    toBeDeleted.add(clit);}}}});}
                else {stream.forEach(cLit ->{
                        Clause otherClause = cLit.clause;
                        if(otherClause.timestamp == timestamp+j-1) {otherClause.timestamp = timestamp+j;}});}}}
        for(CLiteral clit : toBeDeleted) {clauseList.removeLiteral(clit);}
        return toBeDeleted.size();}


    /** simplifies a clause by means of the implication DAG.<br>
     * Example: p,q,r  and p -&gt; r: remove p
     *
     * @param clause         the clause to be simplified
     * @param implicationDAG the implication DAG
     * @return               the number of literal removals.
     */
    public static int simplifyClause(Clause clause, ImplicationDAG implicationDAG) {
        int removals = 0;
        for(int i = 0; i < clause.size(); ++i) {   // p,q,r  and p -> r: remove p
            CLiteral cLiteral1 = clause.cliterals.get(i);
            Integer literal1 = cLiteral1.literal;
            if(!implicationDAG.isEmpty(literal1)) {
                for(CLiteral cLiteral2 : clause.cliterals) {
                    if(cLiteral1 != cLiteral2 && implicationDAG.implies(literal1,cLiteral2.literal)) {
                        clause.removeLiteral(cLiteral1);
                        --i;
                        ++removals;
                        break;}}}}
        return removals;}


    /** performs all subsumptions and resolutions with an implication p -&gt; g, and its consequences in the implicatinoDAG
     *
     * @param from       the antecednet of the implication
     * @param to         the succedent or the implication
     * @param clauseList  a clause list
     * @param implicationDAG  the implication DAG
     * @return  null or [number of subsumption, number of resolutions}
     */
    public static int[] simplifyWithImplication(int from, int to, ClauseList clauseList, ImplicationDAG implicationDAG) {
        ArrayList<Clause> toBeDeleted = new ArrayList<>();
        TreeSet<CLiteral> toBeRemoved = new TreeSet<>();
        int[] timestamp = new int[]{clauseList.timestamp};
        implicationDAG.apply(to,true, (q -> {
            ++timestamp[0];
            for(CLiteral clit : clauseList.literalIndex.getLiterals(q)) {clit.clause.timestamp = timestamp[0];}
            implicationDAG.apply(from,false,(p-> {
                for(CLiteral clit : clauseList.literalIndex.getLiterals(-p)) {
                    Clause clause = clit.clause;
                    if(clause.timestamp == timestamp[0]) {
                        toBeDeleted.add(clause);
                        clause.timestamp = 0;}};
                for(CLiteral clit : clauseList.literalIndex.getLiterals(p)) {
                    Clause clause = clit.clause;
                    if(clause.timestamp == timestamp[0]) {toBeRemoved.add(clit);}}}));}));
        clauseList.timestamp = timestamp[0]+1;
        for(Clause clause : toBeDeleted) {clauseList.removeClause(clause);}
        for(CLiteral cLiteral : toBeRemoved) {clauseList.removeLiteral(cLiteral);}
        return(!toBeDeleted.isEmpty() || !toBeRemoved.isEmpty()) ? new int[]{toBeDeleted.size(),toBeRemoved.size()} : null;}


    /** resolves the two clauses at the given literals.
     * All simplifications which are possible by the implicationDAG are performed
     *
     * @param literal1      a literal
     * @param literal2      a literal
     * @param implicationDAG the implication DAG
     * @return  the resolvent, or null if it would be a tautology or subsumed by the implication DAG
     */
    public static ArrayList<CLiteral> resolve(CLiteral literal1, CLiteral literal2, ImplicationDAG implicationDAG) {
        ArrayList<CLiteral> resolvent = new ArrayList<>();
        ArrayList<CLiteral> literals1 = literal1.clause.cliterals;
        for(CLiteral lit1 : literals1) {
            if(lit1 != literal1) {resolvent.add(lit1.clone());}}
        for(CLiteral lit2 : literal2.clause.cliterals) {
            if(lit2 != literal2) {
                boolean ignore = false;
                for(CLiteral lit1 : literals1) {
                    if(lit1 != literal1) {
                        if(implicationDAG.implies(-lit1.literal, lit2.literal)) {return null;}
                        if(implicationDAG.implies(lit2.literal,lit1.literal)) {ignore = true; break;}
                        if(implicationDAG.implies(lit1.literal,lit2.literal)) {
                            resolvent.removeIf(cliteral->cliteral.literal == lit1.literal);}}}
                if(!ignore) {resolvent.add(lit2.clone());}}}
        return resolvent;}

}



