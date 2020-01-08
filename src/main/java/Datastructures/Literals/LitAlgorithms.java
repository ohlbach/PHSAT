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
            int literal = cliteral.literal;
            Iterator<CLiteral<Clause>> iterator = literalIndex.popIteratorTo(literal,size);
            while(iterator.hasNext()) {
                CLiteral<Clause> otherLiteral = iterator.next();
                Clause otherClause = otherLiteral.clause;
                if(clause == otherClause) {continue;}
                if(otherClause.timestamp < timestamp) {otherClause.timestamp = timestamp; continue;}
                if(otherClause.timestamp - timestamp == otherClause.size()-2) {
                    literalIndex.pushIterator(literal,iterator);
                    return otherClause;}
                ++otherClause.timestamp;}
            literalIndex.pushIterator(literal,iterator);}
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
            int literal = cliteral.literal;
            Iterator<CLiteral<Clause>> iterator = literalIndex.popIteratorFrom(literal,size);
            while(iterator.hasNext()) {
                CLiteral<Clause> otherLiteral = iterator.next();
                Clause otherClause = otherLiteral.clause;
                if(clause == otherClause) {continue;}
                if(otherClause.timestamp < timestamp) {otherClause.timestamp = timestamp; continue;}
                if(otherClause.timestamp - timestamp == difference) {
                    subsumed.add(otherClause);
                    otherClause.timestamp = 0;}
                ++otherClause.timestamp;}
            literalIndex.pushIterator(literal,iterator);}}


    /** This method checks if a literal in the given clause can be removed by replacement resolution with another clause in the literal index.
     *
     * @param clause        the clause to be checked
     * @param literalIndex  the index mapping literals to occurrences in clauses
     * @param timestamp     an incremented timestamp
     * @return              [cLiteral,otherClause], or null
     */
    public static Object[] replacementResolutionBackwards(Clause clause, BucketSortedIndex<CLiteral<Clause>> literalIndex, int timestamp) {

        int size = clause.size()+1;
        for(CLiteral cliteral : clause) {
            int literal = cliteral.literal;
            Iterator<CLiteral<Clause>> iterator = literalIndex.popIteratorTo(literal,size);
            while(iterator.hasNext()) {
                CLiteral<Clause> otherLiteral = iterator.next();
                Clause otherClause = otherLiteral.clause;
                if(clause == otherClause) {continue;}
                if(otherClause.timestamp < timestamp) {otherClause.timestamp = timestamp;}
                else {++otherClause.timestamp;}}
            literalIndex.pushIterator(literal,iterator);}
        for(CLiteral cliteral : clause) {
            int literal = -cliteral.literal;
            Iterator<CLiteral<Clause>> iterator = literalIndex.popIteratorTo(literal,size+1);
            while(iterator.hasNext()) {
                Clause otherClause = iterator.next().clause;
                int otherTimestamp = otherClause.timestamp;
                if(otherTimestamp >= timestamp && otherTimestamp - timestamp == otherClause.size()-2) {
                    return new Object[]{cliteral,otherClause};}}
            literalIndex.pushIterator(literal,iterator);}
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
            int literal = cliteral.literal;
            Iterator<CLiteral<Clause>> iterator = literalIndex.popIteratorFrom(literal,size);
            while(iterator.hasNext()) {
                CLiteral<Clause> otherLiteral = iterator.next();
                Clause otherClause = otherLiteral.clause;
                if(clause == otherClause) {continue;}
                if(otherClause.timestamp < timestamp) {otherClause.timestamp = timestamp;}
                else {++otherClause.timestamp;}}
            literalIndex.pushIterator(literal,iterator);}
        for(CLiteral cliteral : clause) {
            int literal = -cliteral.literal;
            Iterator<CLiteral<Clause>> iterator = literalIndex.iteratorFrom(literal,size);
            while(iterator.hasNext()) {
                CLiteral<Clause> otherLiteral = iterator.next();
                Clause otherClause = otherLiteral.clause;
                int otherTimestamp = otherClause.timestamp;
                if(otherTimestamp >= timestamp && otherTimestamp - timestamp == difference) {
                    resolvents.add(otherLiteral);
                    otherClause.timestamp = 0;}}
            literalIndex.pushIterator(literal,iterator);}}

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
    public static Clause resolve(int[] id, CLiteral<Clause> literal1, CLiteral<Clause> literal2) {
        Clause parent1 = literal1.clause; Clause parent2 = literal2.clause;
        int size = parent1.size()+parent2.size()-2;
        Clause resolvent = new Clause(++id[0],size);
        for(CLiteral<Clause> lit1 : parent1) {if(lit1 != literal1) {resolvent.add(new CLiteral<Clause>(lit1.literal));}}
        for(CLiteral<Clause> lit2 : parent2) {
            if(lit2 == literal2) {continue;}
            if(resolvent.contains(-lit2.literal) >= 0) {return null;} // tautology
            if(resolvent.contains(lit2.literal) < 0) {
                    resolvent.add(new CLiteral<Clause>(lit2.literal));}}
        resolvent.setStructure();
        return resolvent;}



    /** checks if a literal can be removed by recursive replacement resolution.
     *  That means, a sequence of resolutions cause a clause which is just one literal shorter than the given one.
     *
     * @param clause        the clause to be tested
     * @param literalIndex  the literal index
     * @param timestamp     a fresh timestamp. It must be increased by at least maxLevel+1
     * @param maxLevel      the maximum search depth
     * @return              null or the cLiteral which can be removed.
     */
    public static CLiteral<Clause> canBRemoved(Clause clause, BucketSortedIndex<CLiteral<Clause>> literalIndex,
                                               int timestamp, int maxLevel) {
        int level = 0;
        for(CLiteral<Clause> cliteral : clause) {ignoreLiterals(cliteral.literal,literalIndex, timestamp);}
        for(CLiteral<Clause> cliteral1 : clause) {
            int literal1 = cliteral1.literal;
            blockClauses(literal1, literalIndex, timestamp);
            Iterator<CLiteral<Clause>> iterator = literalIndex.popIterator(-literal1);
            while(iterator.hasNext()) {
                if(replResRecursive(iterator.next(),literalIndex, timestamp, level,maxLevel)) {
                    literalIndex.pushIterator(-literal1,iterator);
                    return cliteral1;};}
            literalIndex.pushIterator(-literal1,iterator);
            unblockClauses(literal1,literalIndex);}
        return null;}

    /** checks if the cliteral, plus possibly some merged literals can be derived by resolution.
     *  The merge literals are the literals which merge into other literals on the resolution path
     *
     * @param cliteral      a literal, (the resolution partner in the recursive search)
     * @param literalIndex  the literal inde
     * @param timestamp     the current timestamp
     * @param level         the actual search depth
     * @param maxLevel      the maximum searh depth
     * @return              true if the literal, plus merge literals can be derived
     */
    private static boolean replResRecursive(CLiteral<Clause> cliteral, BucketSortedIndex<CLiteral<Clause>> literalIndex,
                                            int timestamp, int level, int maxLevel) {
        Clause clause = cliteral.clause;
        if(clause.timestamp > timestamp) {return false;}
        boolean solved = true;
        for(CLiteral<Clause> cliteral1 : clause) {
            if(cliteral == cliteral1 || cliteral1.timestamp == timestamp) {continue;}
            solved = false;
            ignoreLiterals(cliteral1.literal, literalIndex, timestamp);
            cliteral1.timestamp = 0;}
        if(solved) return true;
        if(level == maxLevel) {return false;}

        for(CLiteral<Clause> cliteral1 : clause) {
            if(cliteral1 == cliteral ||  cliteral1.timestamp == timestamp) {continue;}
            int literal1 = cliteral1.literal;
            blockClauses(literal1, literalIndex, timestamp);
            solved = false;
            Iterator<CLiteral<Clause>> iterator = literalIndex.popIterator(-literal1);
            while(iterator.hasNext()) {
                if(replResRecursive(iterator.next(),literalIndex, timestamp, level+1,maxLevel)) {
                    solved = true; break;}}
            literalIndex.pushIterator(-literal1,iterator);
            if(!solved) {;return false;}}
        return solved;}

    private static final int[] signs = new int[]{+1,-1};

    /** marks all clauses with the literal and its negation as blocked for the search
     *
     * @param literal      a literal
     * @param literalIndex the literal index
     * @param timestamp    the current timestamp
     */
    private static void blockClauses(int literal,BucketSortedIndex<CLiteral<Clause>> literalIndex, int timestamp) {
        for(int sign : signs) {
            Iterator<CLiteral<Clause>> iterator = literalIndex.popIterator(sign*literal);
            while(iterator.hasNext()) {
                Clause clause = iterator.next().clause;
                if(clause.timestamp < timestamp) {clause.timestamp = timestamp;}
                else {++clause.timestamp;}}
            literalIndex.pushIterator(sign*literal,iterator);}}

    /** removes the recusive block marking for one level and for all clauses with the literal and its negation
     *
     * @param literal      a literal
     * @param literalIndex the literal index
     */
    private static void unblockClauses(int literal, BucketSortedIndex<CLiteral<Clause>> literalIndex) {
        for(int sign : signs) {
            Iterator<CLiteral<Clause>> iterator = literalIndex.popIterator(sign*literal);
            while(iterator.hasNext()) {--iterator.next().clause.timestamp;}
            literalIndex.pushIterator(sign*literal,iterator);}}

    /** marks all literal occurrences with the given literal with the timestamp
     *
     * @param literal      a literal
     * @param literalIndex the literal index
     * @param timestamp    the timestamp
     */
    private static void ignoreLiterals(int literal, BucketSortedIndex<CLiteral<Clause>> literalIndex, int timestamp) {
        Iterator<CLiteral<Clause>> iterator = literalIndex.popIterator(literal);
        while(iterator.hasNext()) {iterator.next().timestamp = timestamp;}
        literalIndex.pushIterator(literal,iterator);}
    }


