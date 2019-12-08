package Datastructures.Literals;

import Datastructures.Clauses.Clause;
import Utilities.BucketSortedIndex;

import java.util.ArrayList;
import java.util.Iterator;

/**
 * Created by ohlbach on 03.07.2019.
 */
public class LitAlgorithms {



    /** This method checks if the given clause is subsumed by some other clause in the literal index
     *
     * @param clause        the clause to be checked
     * @param literalIndex  the index mapping literals to occurrences in clauses
     * @param timestamp     an incremented timestamp
     * @return              either a subsumer, or null
     */
    public static Clause isSubsumed(Clause clause, BucketSortedIndex<CLiteral<Clause>> literalIndex, int timestamp) {
        int size = clause.size()+1;
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
    public static void subsumes(Clause clause, BucketSortedIndex<CLiteral<Clause>> literalIndex, int timestamp, ArrayList<Clause> subsumed) {
        int size = clause.size();
        int difference = size - 2;
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
    public static Object[] replacementResolutionBackwards(Clause clause, BucketSortedIndex<CLiteral<Clause>> literalIndex, int timestamp) {
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
    public static void replacementResolutionForward(Clause clause, BucketSortedIndex<CLiteral<Clause>> literalIndex, int timestamp,
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
            Iterator<CLiteral<Clause>> iterator = literalIndex.iteratorFrom(-cliteral.literal,size);
            while(iterator.hasNext()) {
                CLiteral<Clause> otherLiteral = iterator.next();
                Clause otherClause = otherLiteral.clause;
                int otherTimestamp = otherClause.timestamp;
                if(otherTimestamp >= timestamp && otherTimestamp - timestamp == difference) {
                    resolvents.add(otherLiteral);
                    otherClause.timestamp = 0;}}}}

    /** The method checks if the given literal or its negation are in the literals
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
     *  Double literals are avoided. A tautology is not generated.
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
