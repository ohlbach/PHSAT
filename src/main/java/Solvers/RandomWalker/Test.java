package Solvers.RandomWalker;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import java.util.TreeSet;

/**
 * Created by ohlbach on 16.05.2019.
 */
public class Test {
    public static void main(String[] args) {
        TreeSet<Pair<Integer,Integer>> pairs = new TreeSet<Pair<Integer,Integer>>();
        pairs.add(new ImmutablePair(3,5));
        pairs.add(new ImmutablePair(3,5));
        System.out.println(pairs);

        TreeSet<int[]> pairs1 = new TreeSet<int[]>();
        pairs1.add(new int[]{3,5});
        pairs1.add(new int[]{3,5});
        System.out.println(pairs1);



    }
}
