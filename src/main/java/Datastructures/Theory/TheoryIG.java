package Datastructures.Theory;

import java.util.ArrayList;

/**
 * Created by ohlbach on 29.08.2018.
 */
public class TheoryIG extends Theory {
    ImplicationGraph implicationGraph;

    public TheoryIG(ImplicationGraph implicationGraph) {
        this.implicationGraph = implicationGraph;}

    public ArrayList<Integer> getContradictoryLiterals(int literal) {
        return implicationGraph.getContradictoryLiterals(literal);}
}
