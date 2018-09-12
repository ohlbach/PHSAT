package Coordinator;

import java.util.ArrayList;

/**
 * Created by ohlbach on 12.09.2018.
 */
public class Incoming {
    private ArrayList<Integer> oneLiteralClauses = new ArrayList<>();
    private ArrayList<int[]> twoLiteralClauses = new ArrayList<>();

    public synchronized boolean addOne(int literal) {
        if(oneLiteralClauses.contains(-literal)) {return true;}
        if(!oneLiteralClauses.contains(literal)) {oneLiteralClauses.add(literal); notify();}
        return false;}

    public synchronized void addTwo(int[] clause) {
        for(int[] two : twoLiteralClauses) {
            if(!oneLiteralClauses.contains(clause))
                twoLiteralClauses.add(clause);
                notify();}}


    public synchronized Object[] getIncoming()  {
        while(oneLiteralClauses.isEmpty() && twoLiteralClauses.isEmpty()) {
            try {wait();} catch (InterruptedException e) {}}
        int[] ones = new int[oneLiteralClauses.size()];
        for(int i = 0; i < oneLiteralClauses.size();++i) {ones[i] = oneLiteralClauses.get(i);}
        int[][] twos = new int[2][2];
        for(int i = 0; i < twoLiteralClauses.size();++i) {twos[i] = twoLiteralClauses.get(i);}
        oneLiteralClauses.clear();
        twoLiteralClauses.clear();
        return new Object[]{ones,twos};}

    public String toString() {
        return "Ones: " + oneLiteralClauses.toString() + "\n" +
                "Twos: " +twoLiteralClauses;}


}
