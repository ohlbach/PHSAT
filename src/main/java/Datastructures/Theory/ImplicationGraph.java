package Datastructures.Theory;

import java.util.ArrayList;

/**
 * Created by ohlbach on 29.08.2018.
 */
public class ImplicationGraph {
    private int predicates;
    private final ArrayList<Integer>[] posImplicants;
    private final ArrayList<Integer>[] negImplicants;

    public ImplicationGraph(int predicates) {
        this.predicates = predicates;
        posImplicants = new ArrayList[predicates+1];
        negImplicants = new ArrayList[predicates+1];
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            ArrayList<Integer> list = new ArrayList<>();
            list.add(-predicate);
            posImplicants[predicate] = list;
            list = new ArrayList<>();
            list.add(predicate);
            negImplicants[predicate] = list;}}

    public ArrayList<Integer> getContradictoryLiterals(int literal) {
        return literal > 0 ? posImplicants[literal] : negImplicants[literal];}

    public int addClause(int literal1, int literal2) {
        int unit = addClauseSide(literal1,literal2);
        if(unit != 0) {return unit;}
        return addClauseSide(literal2,literal1);}

    public int addClauseSide(int literal1, int literal2) {
        int negliteral1 = -literal1;
        int negliteral2 = -literal2;
        ArrayList<Integer> list = negliteral1 > 0 ? posImplicants[negliteral1] : negImplicants[literal1];
        for(Integer literal :list) {
            if(literal == negliteral2) {return 0;}
            if(literal == literal2) {return negliteral1;}}
        list.add(negliteral2);
        return 0;}

    public String toString(){
        StringBuffer st = new StringBuffer();
        int numbersize = ("-"+predicates).length();
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            ArrayList<Integer> list = posImplicants[predicate];
            if(list.size() > 1) {
                st.append(String.format("%"+numbersize+"d",predicate)).append("->");
                for(int i = 1; i < list.size(); ++i) {st.append(-list.get(i)).append(",");}
                st.append("\n");}
            list = negImplicants[predicate];
            if(list.size() > 1) {
                st.append(String.format("%"+numbersize+"d",-predicate)).append("->");
                for(int i = 1; i < list.size(); ++i) {st.append(-list.get(i)).append(",");}
                st.append("\n");}}
        return st.toString();}

}
