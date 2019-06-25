package Solvers.Resolution;

import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Literals.LiteralIndex;
import Datastructures.Literals.LiteralIndexArray;
import Utilities.BucketSortedList;

import java.util.Random;
import java.util.function.BiConsumer;
import java.util.function.Function;

/**
 * Created by ohlbach on 09.06.2019.
 */
public class ClauseLists {
    private BucketSortedList<Clause> clauseList1;
    private BucketSortedList<Clause> clauseList2;
    public LiteralIndex literalIndex;

    public ClauseLists(int predicates) {
        Function<Clause,Integer> getSize       = ((Clause clause)->clause.size()-3);
        clauseList1 = new BucketSortedList<Clause>(getSize);
        clauseList2 = new BucketSortedList<Clause>(getSize);
        literalIndex = new LiteralIndexArray(predicates);}

    public void addClause1(Clause clause) {
        clauseList1.add(clause);
        for(CLiteral<Clause> literal : clause) {literalIndex.addLiteral(literal);}}

    public void addClause2(Clause clause) {
        clauseList2.add(clause);
        for(CLiteral<Clause> literal : clause) {literalIndex.addLiteral(literal);}}

    public void removeClause(Clause clause) {
        clauseList1.remove(clause);
        clauseList2.remove(clause);
        for(CLiteral<Clause> literal : clause) {literalIndex.removeLiteral(literal);}}

    public Clause getRandom(Random random) {
        return clauseList1.getRandom(random);}

    public boolean isEmpty1() {
        return clauseList1.isEmpty();}

    public boolean isEmpty2() {
        return clauseList2.isEmpty();}

    public boolean isEmpty() {
        return clauseList1.isEmpty() && clauseList2.isEmpty();}

    public int size1() {
        return clauseList1.size();}

    public int size2() {
        return clauseList2.size();}

    public int size() {
        return clauseList1.size() + clauseList2.size();}

    public boolean containedIn1(Clause clause) {
        return clauseList1.contains(clause);}

    public boolean containedIn2(Clause clause) {
        return clauseList2.contains(clause);}

    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Clause List 1:\n").append(clauseList1.toString());
        st.append("\n\nClause List 2:\n").append(clauseList2.toString());
        return st.toString();}


}
