package Datastructures.Literals;

import Datastructures.Clauses.Clause;
import Utilities.BucketSortedIndex;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Stack;

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
                                               int timestamp, int maxLevel,  ArrayList<Clause> usedClauses) {
        int level = 0;
        for(CLiteral<Clause> cliteral : clause) {
            int literal = cliteral.literal;
            allowLiterals(literal,literalIndex, timestamp); // this allows merge of double literals
            blockClauses(-literal,literalIndex,timestamp);}  // this avoids tautologies

        for(CLiteral<Clause> cliteral1 : clause) {
            int literal1 = cliteral1.literal;
            blockClauses(literal1, literalIndex, timestamp); // this avoids that a literal which is resolved away reappears
            Iterator<CLiteral<Clause>> iterator = literalIndex.popIterator(-literal1);
            while(iterator.hasNext()) {
                CLiteral<Clause> otherLiteral = iterator.next();
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
    private static boolean replResRecursive(CLiteral<Clause> cliteral, BucketSortedIndex<CLiteral<Clause>> literalIndex,
                                                                int timestamp, int level, int maxLevel, ArrayList<Clause> usedClauses) {
        Clause clause = cliteral.clause;
        boolean solved = true;
        for(CLiteral<Clause> cliteral1 : clause) { // is it entirely solved already?
            if(cliteral == cliteral1 || cliteral1.timestamp >= timestamp) {continue;}
            solved = false;
            int literal1 = cliteral1.literal;
            allowLiterals(literal1, literalIndex, timestamp); // they can be merged
            blockClauses(-literal1,literalIndex,timestamp);   // to avoid tautologies
            cliteral1.timestamp = 0;}                         // this still must be solved

        if(solved || level == maxLevel) {
            for(CLiteral<Clause> cliteral1 : clause) {
                if(cliteral == cliteral1 || cliteral1.timestamp >= timestamp) {continue;}
                int literal1 = cliteral1.literal;
                unallowLiterals(literal1, literalIndex);
                unblockClauses(-literal1,literalIndex);}
            if(solved && usedClauses != null) {usedClauses.add(clause);}
            return solved;}

        solved = true;
        int usedClausesSize = (usedClauses != null) ? usedClauses.size() : 0;
        for(CLiteral<Clause> cliteral1 : clause) {
            if(cliteral1 == cliteral ||  cliteral1.timestamp >= timestamp) {continue;} // they do merge
            int literal1 = cliteral1.literal;
            blockClauses(literal1, literalIndex, timestamp);   // to avoid reappearing literal1
            boolean found = false;
            Iterator<CLiteral<Clause>> iterator = literalIndex.popIterator(-literal1);
            while(iterator.hasNext()) {
                CLiteral<Clause> cliteral2 = iterator.next();
                if(cliteral2.clause.timestamp > timestamp) {continue;}   // blocked
                if(replResRecursive(cliteral2,literalIndex, timestamp, level+1,maxLevel,usedClauses)) {
                    found = true; break;}}
            literalIndex.pushIterator(-literal1,iterator);
            if(!found) {
                for(CLiteral<Clause> cliteral2 : clause) {
                    if(cliteral == cliteral2 || cliteral2.timestamp >= timestamp) {continue;}
                    unblockClauses(cliteral2.literal,literalIndex);
                    if(cliteral2 == cliteral1) break;}
                solved = false; break;}}

        for(CLiteral<Clause> cliteral1 : clause) {
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
    private static void blockClauses(int literal,BucketSortedIndex<CLiteral<Clause>> literalIndex, int timestamp) {
        Iterator<CLiteral<Clause>> iterator = literalIndex.popIterator(literal);
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
    private static void unblockClauses(int literal, BucketSortedIndex<CLiteral<Clause>> literalIndex) {
        Iterator<CLiteral<Clause>> iterator = literalIndex.popIterator(literal);
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
    private static void allowLiterals(int literal, BucketSortedIndex<CLiteral<Clause>> literalIndex, int timestamp) {
        Iterator<CLiteral<Clause>> iterator = literalIndex.popIterator(literal);
        while(iterator.hasNext()) {
            CLiteral<Clause> cliteral = iterator.next();
            if(cliteral.timestamp < timestamp) {cliteral.timestamp = timestamp;}
            else {++cliteral.timestamp;}
        }
        literalIndex.pushIterator(literal,iterator);}

    /** unmarks all literal occurrences with the given literal with the timestamp
     *
     * @param literal      a literal
     * @param literalIndex the literal index
     */
    private static void unallowLiterals(int literal, BucketSortedIndex<CLiteral<Clause>> literalIndex) {
        Iterator<CLiteral<Clause>> iterator = literalIndex.popIterator(literal);
        while(iterator.hasNext()) {
            CLiteral<Clause> cliteral = iterator.next();
            --cliteral.timestamp;}
        literalIndex.pushIterator(literal,iterator);}



    public static Object urResolution(Clause clause, BucketSortedIndex<CLiteral<Clause>> literalIndex, int timestamp, int maxClauseLength,
                                           ArrayList<Clause> usedClauses) {
        int size = clause.size();
        HashMap<Clause,ArrayList<Clause>> usedClausesMap = null;
        if(usedClauses != null) {usedClauses.clear(); usedClausesMap = new HashMap<>();}
        for(CLiteral<Clause> cliteral : clause) {
            if(findEmptyClause(-cliteral.literal,clause,literalIndex,timestamp,usedClausesMap)) {
                if(usedClauses != null) {usedClauses.addAll(usedClausesMap.values().iterator().next());}
                return -cliteral.literal;}
            for(int i = 0; i < size; ++i) {
                CLiteral<Clause> cliteral1 = clause.getCLiteral(i);
                if(cliteral1 == cliteral1) {continue;}
                if(findEmptyClause(cliteral1.literal,clause,literalIndex,timestamp,usedClausesMap)) {
                    if(i == size-1) {return cliteral;}
                    else {
                        int[] literals = new int[i];
                        for(int j = 0; j <= i; ++j) {
                            literals[j] = (j==i) ? -clause.getLiteral(j) : clause.getLiteral(j);}
                        if(usedClauses != null) {usedClauses.addAll(usedClausesMap.values().iterator().next());}
                        return literals;}}}
            timestamp += maxClauseLength +1;}
        return null;}



        private static boolean findEmptyClause(int literal, Clause blockedClause, BucketSortedIndex<CLiteral<Clause>> literalIndex, int timestamp,
                                           HashMap<Clause,ArrayList<Clause>> usedClauses) {
        Iterator<CLiteral<Clause>> iterator = literalIndex.popIterator(literal);
        while(iterator.hasNext()) {
            CLiteral<Clause> cliteral = iterator.next();
            Clause clause = cliteral.clause;
            if(clause == blockedClause) {continue;}
            cliteral.timestamp = timestamp;
            int ts = clause.timestamp;
            if(ts < timestamp) {clause.timestamp = timestamp; ts = timestamp;} // not yet visited
            int size = clause.size();
            if(ts - timestamp == size-1) {
                literalIndex.pushIterator(literal,iterator);
                if(usedClauses != null) {addUsedClause(usedClauses,clause); clearUsedClauses(usedClauses,clause);}
                return true;}
            if(ts - timestamp == size-2) {
                for(CLiteral<Clause> cliteral1 : clause) {
                    if(cliteral1 != cliteral && cliteral1.timestamp < timestamp) {
                        if(usedClauses != null) {addUsedClause(usedClauses,clause);}
                        if(findEmptyClause(-cliteral1.literal, blockedClause, literalIndex,timestamp,usedClauses)) {
                            literalIndex.pushIterator(literal,iterator);
                            return true;}
                        else {break;}}}}
            ++clause.timestamp;}
        literalIndex.pushIterator(literal,iterator);
        return false;}


    private static void addUsedClause(HashMap<Clause,ArrayList<Clause>> usedClauses, Clause clause) {
        ArrayList<Clause> clauses = usedClauses.get(clause);
        if(clauses == null) {clauses = new ArrayList<>(); usedClauses.put(clause,clauses);}
        clauses.add(clause);}

    private static void clearUsedClauses(HashMap<Clause,ArrayList<Clause>> usedClauses, Clause clause) {
        ArrayList<Clause> clauses = usedClauses.get(clause);
        if(clauses == null) {clauses = new ArrayList<>();}
        usedClauses.clear();
        usedClauses.put(clause,clauses);
        clauses.add(clause);}



}


