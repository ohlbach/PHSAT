package Datastructures.Theory;

import java.util.ArrayList;

/**
 * Created by ohlbach on 29.08.2018.
 */
public class Theory {
    ArrayList<Integer> dummy = new ArrayList<>(1);
    public ArrayList<Integer> getContradictoryLiterals(int literal) {
        dummy.add(0,-literal);
        return dummy;}
}
