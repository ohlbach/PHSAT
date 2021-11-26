package Datastructures.Literals;

import java.util.AbstractCollection;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * Created by ohlbach on 25.06.2019.
 */
public class LiteralIndexArray extends LiteralIndex  {
    private ArrayList<CLiteral>[] posOccurrences;  // maps each positive predicate to the list of occurrences
    private ArrayList<CLiteral>[] negOccurrences;  // maps each negative predicate to the list of occurrences
    private ArrayList<CLiteral> emptyList;

    /** constructs an index for a given number of predicates
     *
     * @param predicates the number of predicates
     */
    public LiteralIndexArray(int predicates) {
        super(predicates);
        posOccurrences = new ArrayList[predicates + 1];
        negOccurrences = new ArrayList[predicates + 1];
        emptyList = new ArrayList<>(0);}


    /** adds a literal to the index
     *
     * @param cliteral the literal to be added
     */
    public void addLiteral(CLiteral cliteral) {
        int literal = cliteral.literal;
        int predicate = Math.abs(literal);
        ArrayList<CLiteral>[] list = literal > 0 ? posOccurrences : negOccurrences;
        ArrayList<CLiteral> lits = list[predicate];
        if(lits == null) {
            lits = new ArrayList();
            list[predicate] = lits;}
        cliteral.indexPosition = lits.size();
        lits.add(cliteral);}

    /** removes the literal from the index (in constant time)
     *
     * @param cliteral the literal to be removed.
     */
    public void removeLiteral(CLiteral cliteral) {
        int literal = cliteral.literal;
        ArrayList<CLiteral> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        if(list == null) {return;}
        int size = list.size()-1;
        if(size == 0) {
            if(literal > 0) {posOccurrences[literal] = null;}
            else            {negOccurrences[-literal] = null;}}
        else {
            int position = cliteral.indexPosition;
            CLiteral lastLiteral = list.get(size);
            lastLiteral.indexPosition = position;
            list.set(position,lastLiteral);
            list.remove(size);}
    }


    /** returns the CLiterals with the given literal (integer)
     *
     * @param literal the literal (integer)
     * @return the list of occurrences (CLiterals)
     */
    public AbstractCollection<CLiteral> getLiterals(int literal) {
        assert literal != 0 && (Math.abs(literal) <= predicates);
        ArrayList<CLiteral> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return list == null ? emptyList : list;}

    /** returns the number of cLiterals indexed by this literal
     *
     * @param literal a literal
     * @return the number of cLiterals indexed by this literal
     */
    public int size(int literal) {
        ArrayList<CLiteral> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return list == null ? 0 : list.size();}

    public boolean isEmpty(int literal) {
        ArrayList<CLiteral> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return list == null || list.isEmpty();}



    public Iterator<CLiteral> iterator(int literal) {
        ArrayList<CLiteral> list =  literal > 0 ? posOccurrences[literal] : negOccurrences[-literal];
        return (list == null) ? emptyList.iterator() : list.iterator();}


}
