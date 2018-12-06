package Datastructures.Clauses;

/** These tags are used for clauses and clause sets
 * Created by ohlbach on 03.12.2018.
 */
public enum ClauseStructure {
    POSITIVE, // clause: it consists only positive literals. ClauseList: it contains no negative clauses
    NEGATIVE, // clause: it consists only negative literals. ClauseList: it contains no positive clauses
    BOTH,     // clause list: it contains only mixed literals
    MIXED;    // clause: it contains positive and negative literals. ClauseList: no restriction.
}
