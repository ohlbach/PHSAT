package Datastructures.Literals;

import Datastructures.Clauses.Clause;
import Utilities.BucketSortedIndex;

import java.lang.reflect.Array;
import java.util.*;

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
    public static Clause isSubsumed(Clause clause, BucketSortedIndex<CLiteral> literalIndex, int timestamp) {
        int size = clause.size()+1;
        for(CLiteral cliteral : clause) {
            int literal = cliteral.literal;
            Iterator<CLiteral> iterator = literalIndex.popIteratorTo(literal,size);
            while(iterator.hasNext()) {
                CLiteral otherLiteral = iterator.next();
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
    public static void subsumes(Clause clause, BucketSortedIndex<CLiteral> literalIndex, int timestamp, ArrayList<Clause> subsumed) {
         int size = clause.size();
        int difference = size - 2;
        for(CLiteral cliteral : clause) {
            int literal = cliteral.literal;
            Iterator<CLiteral> iterator = literalIndex.popIteratorFrom(literal,size);
            while(iterator.hasNext()) {
                CLiteral otherLiteral = iterator.next();
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
    public static Object[] replacementResolutionBackwards(Clause clause, BucketSortedIndex<CLiteral> literalIndex, int timestamp) {

        int size = clause.size()+1;
        for(CLiteral cliteral : clause) {
            int literal = cliteral.literal;
            Iterator<CLiteral> iterator = literalIndex.popIteratorTo(literal,size);
            while(iterator.hasNext()) {
                CLiteral otherLiteral = iterator.next();
                Clause otherClause = otherLiteral.clause;
                if(clause == otherClause) {continue;}
                if(otherClause.timestamp < timestamp) {otherClause.timestamp = timestamp;}
                else {++otherClause.timestamp;}}
            literalIndex.pushIterator(literal,iterator);}
        for(CLiteral cliteral : clause) {
            int literal = -cliteral.literal;
            Iterator<CLiteral> iterator = literalIndex.popIteratorTo(literal,size+1);
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
    public static void replacementResolutionForward(Clause clause, BucketSortedIndex<CLiteral> literalIndex, int timestamp,
                                                        ArrayList<CLiteral> resolvents) {
        int size = clause.size();
        int difference = size-2;
        for(CLiteral cliteral : clause) {
            int literal = cliteral.literal;
            Iterator<CLiteral> iterator = literalIndex.popIteratorFrom(literal,size);
            while(iterator.hasNext()) {
                CLiteral otherLiteral = iterator.next();
                Clause otherClause = otherLiteral.clause;
                if(clause == otherClause) {continue;}
                if(otherClause.timestamp < timestamp) {otherClause.timestamp = timestamp;}
                else {++otherClause.timestamp;}}
            literalIndex.pushIterator(literal,iterator);}
        for(CLiteral cliteral : clause) {
            int literal = -cliteral.literal;
            Iterator<CLiteral> iterator = literalIndex.iteratorFrom(literal,size);
            while(iterator.hasNext()) {
                CLiteral otherLiteral = iterator.next();
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
    public static int contains(ArrayList<CLiteral> cLiterals, int literal) {
        for(CLiteral cLit : cLiterals) {
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
    public static Clause resolve(int[] id, CLiteral literal1, CLiteral literal2) {
        Clause parent1 = literal1.clause; Clause parent2 = literal2.clause;
        int size = parent1.size()+parent2.size()-2;
        Clause resolvent = new Clause(++id[0],size);
        for(CLiteral lit1 : parent1) {if(lit1 != literal1) {resolvent.add(new CLiteral(lit1.literal));}}
        for(CLiteral lit2 : parent2) {
            if(lit2 == literal2) {continue;}
            if(resolvent.contains(-lit2.literal) >= 0) {return null;} // tautology
            if(resolvent.contains(lit2.literal) < 0) {
                    resolvent.add(new CLiteral(lit2.literal));}}
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
    public static CLiteral canBRemoved(Clause clause, BucketSortedIndex<CLiteral> literalIndex,
                                               int timestamp, int maxLevel,  ArrayList<Clause> usedClauses) {
        int level = 0;
        for(CLiteral cliteral : clause) {
            int literal = cliteral.literal;
            allowLiterals(literal,literalIndex, timestamp); // this allows merge of double literals
            blockClauses(-literal,literalIndex,timestamp);}  // this avoids tautologies

        for(CLiteral cliteral1 : clause) {
            int literal1 = cliteral1.literal;
            blockClauses(literal1, literalIndex, timestamp); // this avoids that a literal which is resolved away reappears
            Iterator<CLiteral> iterator = literalIndex.popIterator(-literal1);
            while(iterator.hasNext()) {
                CLiteral otherLiteral = iterator.next();
                if(replResRecursive(otherLiteral,literalIndex, timestamp, level,maxLevel,usedClauses)) {
                    literalIndex.pushIterator(-literal1,iterator);
                    return cliteral1;};}
            literalIndex.pushIterator(-literal1,iterator);
            unblockClauses(literal1,literalIndex);}
        return null;}

    /** checks if the cliteral, plus possibly some merged literals can be derived by resolution.
     *  The merge literals are the literals which merge into other literals on the resolution path
     *
     * @param cliteral      a literal, (the resolution partner in the recursive search)
     * @param literalIndex  the literal index
     * @param timestamp     the current timestamp
     * @param level         the actual search depth
     * @param maxLevel      the maximum search depth
     * @return              true if the literal, plus merge literals can be derived
     */
    private static boolean replResRecursive(CLiteral cliteral, BucketSortedIndex<CLiteral> literalIndex,
                                                                int timestamp, int level, int maxLevel, ArrayList<Clause> usedClauses) {
        Clause clause = cliteral.clause;
        boolean solved = true;
        for(CLiteral cliteral1 : clause) { // is it entirely solved already?
            if(cliteral == cliteral1 || cliteral1.timestamp >= timestamp) {continue;}
            solved = false;
            int literal1 = cliteral1.literal;
            allowLiterals(literal1, literalIndex, timestamp); // they can be merged
            blockClauses(-literal1,literalIndex,timestamp);   // to avoid tautologies
            cliteral1.timestamp = 0;}                         // this still must be solved

        if(solved || level == maxLevel) {
            for(CLiteral cliteral1 : clause) {
                if(cliteral == cliteral1 || cliteral1.timestamp >= timestamp) {continue;}
                int literal1 = cliteral1.literal;
                unallowLiterals(literal1, literalIndex);
                unblockClauses(-literal1,literalIndex);}
            if(solved && usedClauses != null) {usedClauses.add(clause);}
            return solved;}

        solved = true;
        int usedClausesSize = (usedClauses != null) ? usedClauses.size() : 0;
        for(CLiteral cliteral1 : clause) {
            if(cliteral1 == cliteral ||  cliteral1.timestamp >= timestamp) {continue;} // they do merge
            int literal1 = cliteral1.literal;
            blockClauses(literal1, literalIndex, timestamp);   // to avoid reappearing literal1
            boolean found = false;
            Iterator<CLiteral> iterator = literalIndex.popIterator(-literal1);
            while(iterator.hasNext()) {
                CLiteral cliteral2 = iterator.next();
                if(cliteral2.clause.timestamp > timestamp) {continue;}   // blocked
                if(replResRecursive(cliteral2,literalIndex, timestamp, level+1,maxLevel,usedClauses)) {
                    found = true; break;}}
            literalIndex.pushIterator(-literal1,iterator);
            if(!found) {
                for(CLiteral cliteral2 : clause) {
                    if(cliteral == cliteral2 || cliteral2.timestamp >= timestamp) {continue;}
                    unblockClauses(cliteral2.literal,literalIndex);
                    if(cliteral2 == cliteral1) break;}
                solved = false; break;}}

        for(CLiteral cliteral1 : clause) {
            if(cliteral == cliteral1 || cliteral1.timestamp >= timestamp) {continue;}
            int literal1 = cliteral1.literal;
            unallowLiterals(literal1, literalIndex);
            unblockClauses(-literal1,literalIndex);}

        if(usedClauses != null) {
            if(solved) {usedClauses.add(clause);}
            else       {cutArray(usedClauses,usedClausesSize);}}
        return solved;}

    private static <T> void  cutArray(ArrayList<T> array, int size) {
        for(int i = array.size()-1; i >= size; --i) {array.remove(i);}}



    /** marks all clauses with the literal and its negation as blocked for the search
     *
     * @param literal      a literal
     * @param literalIndex the literal index
     * @param timestamp    the current timestamp
     */
    private static void blockClauses(int literal,BucketSortedIndex<CLiteral> literalIndex, int timestamp) {
        Iterator<CLiteral> iterator = literalIndex.popIterator(literal);
        while(iterator.hasNext()) {
            Clause clause = iterator.next().clause;
            if(clause.timestamp < timestamp) {clause.timestamp = timestamp;}
            else {++clause.timestamp;}}
        literalIndex.pushIterator(literal,iterator);}

    /** removes the recusive block marking for one level and for all clauses with the literal and its negation
     *
     * @param literal      a literal
     * @param literalIndex the literal index
     */
    private static void unblockClauses(int literal, BucketSortedIndex<CLiteral> literalIndex) {
        Iterator<CLiteral> iterator = literalIndex.popIterator(literal);
        while(iterator.hasNext()) {
            Clause clause = iterator.next().clause;
            --clause.timestamp;}
        literalIndex.pushIterator(literal,iterator);}

    /** marks all literal occurrences with the given literal with the timestamp
     *
     * @param literal      a literal
     * @param literalIndex the literal index
     * @param timestamp    the timestamp
     */
    private static void allowLiterals(int literal, BucketSortedIndex<CLiteral> literalIndex, int timestamp) {
        Iterator<CLiteral> iterator = literalIndex.popIterator(literal);
        while(iterator.hasNext()) {
            CLiteral cliteral = iterator.next();
            if(cliteral.timestamp < timestamp) {cliteral.timestamp = timestamp;}
            else {++cliteral.timestamp;}
        }
        literalIndex.pushIterator(literal,iterator);}

    /** unmarks all literal occurrences with the given literal with the timestamp
     *
     * @param literal      a literal
     * @param literalIndex the literal index
     */
    private static void unallowLiterals(int literal, BucketSortedIndex<CLiteral> literalIndex) {
        Iterator<CLiteral> iterator = literalIndex.popIterator(literal);
        while(iterator.hasNext()) {
            CLiteral cliteral = iterator.next();
            --cliteral.timestamp;}
        literalIndex.pushIterator(literal,iterator);}


    /** This method tries to simplify a clause by means of UR-Resolution with the other clauses.
     * There are three types of results: for a clause like p,q,r,s <br>
     *     - a literal like -p if this can be derived from the other clauses <br>
     *     - an array of literals like -p,q,s,  which allows to resolve p away, but my be also useful for other inferences <br>
     *     - a CLiteral [p] which indicates that this literal can be removed by replacement resolution. <br>
     * The timestamp must be incremented at the end by (maxClauseLength +1) * clause.size()
     *
     * @param clause            the clause to be investigated
     * @param literalIndex      the literal index
     * @param timestamp         a new timestamp
     * @param maxClauseLength   the maximum clause length
     * @param usedClauses       is filled with the clauses which are used for the inferences
     * @return                  a literal, an array of literals or a CLiteral
     */
    public static Object urResolution(Clause clause, BucketSortedIndex<CLiteral> literalIndex, int timestamp, int maxClauseLength,
                                           ArrayList<Clause> usedClauses) {
        int size = clause.size();
        HashMap<Clause,ArrayList<Clause>> usedClausesMap = null;
        if(usedClauses != null) {usedClauses.clear(); usedClausesMap = new HashMap<>();}
        for(int k = 0; k < size; ++k) {
            CLiteral cliteral = clause.getCLiteral(k);
            if(findEmptyClause(-cliteral.literal,clause,null,literalIndex,timestamp,usedClausesMap)) {
                    if(usedClauses != null) {usedClauses.addAll(usedClausesMap.values().iterator().next());}
                if(k == 0) {return -cliteral.literal;}}
            for(int i = 0; i < size; ++i) {
                CLiteral cliteral1 = clause.getCLiteral(i);
                if(cliteral1 == cliteral) {continue;}
                if(findEmptyClause(cliteral1.literal,clause,null,literalIndex,timestamp,usedClausesMap)) {
                    if(i == size-1 || (i == size-2 && k == size-1)) {
                        if(usedClauses != null) {usedClauses.addAll(usedClausesMap.values().iterator().next());}
                        return cliteral;}
                    else {
                        int length = i + ((i < k) ? 2 : 1);
                        int[] literals = new int[length];
                        for(int j = 0; j <= i; ++j) {
                            literals[j] = (j==k) ? (-clause.getLiteral(j)) : clause.getLiteral(j);}
                        if(i < k) {literals[length-1] = -clause.getLiteral(k);}
                        if(usedClauses != null) {usedClauses.addAll(usedClausesMap.values().iterator().next());}
                        return literals;}}}
            timestamp += maxClauseLength +1;}
        return null;}

    public static Object isDerivableBinaryClause(int literal1, int literal2, Clause blockedClause, BucketSortedIndex<CLiteral> literalIndex, int timestamp,
                                                  ArrayList<Clause> usedClauses) {

        HashMap<Clause,ArrayList<Clause>> usedClausesMap = null;
        if(usedClauses != null) {usedClauses.clear(); usedClausesMap = new HashMap<>();}
        if(findEmptyClause(-literal1,blockedClause,null,literalIndex,timestamp,usedClausesMap)) {
            if(usedClauses != null) {usedClauses.addAll(usedClausesMap.values().iterator().next());}
            return -literal1;}
        if(findEmptyClause(-literal2,blockedClause,null,literalIndex,timestamp,usedClausesMap)) {
            if(usedClauses != null) {usedClauses.addAll(usedClausesMap.values().iterator().next());}
            return true;}
        return null;}



    /** This method searches for the empty clause if the literal is assumed to be true.
     *
     * @param literal         a literal which is assumed to be false
     * @param blockedClause   a clause which cannot be used for the search
     * @param parentClause    null or the parent clause in the recursion
     * @param literalIndex    the literal index
     * @param timestamp       the current timestamp
     * @param usedClauses     is filled with the clause which are used for the empty clause
     * @return                true if the empty clause is found
     */
        private static boolean findEmptyClause(int literal, Clause blockedClause, Clause parentClause, BucketSortedIndex<CLiteral> literalIndex, int timestamp,
                                           HashMap<Clause,ArrayList<Clause>> usedClauses) {
        Iterator<CLiteral> iterator = literalIndex.popIterator(literal);
        while(iterator.hasNext()) {
            CLiteral cliteral = iterator.next();
            if(cliteral.timestamp == timestamp) {continue;}
            Clause clause = cliteral.clause;
            if(clause == blockedClause) {continue;}
            int size = clause.size();
            if(usedClauses != null) {joinUsedClauses(usedClauses,parentClause,clause);}
            cliteral.timestamp = timestamp;
            if(clause.timestamp < timestamp) {clause.timestamp = timestamp;} // not yet visited
            else {++clause.timestamp;}
            int ts = clause.timestamp;
            if(ts - timestamp == size-1) {
                literalIndex.pushIterator(literal,iterator);
                if(usedClauses != null) {clearUsedClauses(usedClauses,parentClause,clause);}
                return true;}
            if(ts - timestamp == size-2) {
                for(CLiteral cliteral1 : clause) {
                    if(cliteral1 != cliteral && cliteral1.timestamp < timestamp) {
                        if(findEmptyClause(-cliteral1.literal, blockedClause, clause, literalIndex,timestamp,usedClauses)) {
                            literalIndex.pushIterator(literal,iterator);
                            return true;}
                        else {break;}}}}}
        literalIndex.pushIterator(literal,iterator);
        return false;}

    /** joins the used clauses with the used clauses of the parent clause and the given clause
     *
     * @param usedClauses   maps clauses to used clauses
     * @param parentClause  the parent clause in the search
     * @param clause        the current clause
     */
    private static void joinUsedClauses(HashMap<Clause,ArrayList<Clause>> usedClauses, Clause parentClause,Clause clause) {
        ArrayList<Clause> parentClauses = usedClauses.get(parentClause);
        ArrayList<Clause> actualClauses = usedClauses.get(clause);
        if(actualClauses == null) {actualClauses = new ArrayList<>(); usedClauses.put(clause,actualClauses);}
        if(parentClauses != null) {actualClauses.addAll(parentClauses);}
        if(parentClause != null) {actualClauses.add(parentClause);}}


    /** joins the used clauses into the used clauses for the given clause and clears all other used clauses
     *
     * @param usedClauses   maps clauses to used clauses
     * @param parentClause  the current parent clause
     * @param clause        the actual clause
     */
    private static void clearUsedClauses(HashMap<Clause,ArrayList<Clause>> usedClauses, Clause parentClause, Clause clause) {
        ArrayList<Clause> parentClauses = usedClauses.get(parentClause);
        ArrayList<Clause> actualClauses = usedClauses.get(clause);
        if(actualClauses == null) {actualClauses = new ArrayList<>();}
        if(parentClauses != null) {actualClauses.addAll(parentClauses);}
        if(parentClause != null) {actualClauses.add(parentClause);}
        actualClauses.add(clause);
        usedClauses.clear();
        usedClauses.put(clause,actualClauses);}




}


