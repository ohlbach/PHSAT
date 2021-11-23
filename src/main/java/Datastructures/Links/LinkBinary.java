package Datastructures.Links;

import Datastructures.Literals.CLiteralOld;

import java.util.Arrays;

/**
 * Created by ohlbach on 29.08.2018.
 */
public class LinkBinary {
    public CLiteralOld[] cliterals = new CLiteralOld[2];

    public LinkBinary(CLiteralOld cliteral1, CLiteralOld cliteral2) {
        cliterals[0] = cliteral1;
        cliterals[1] = cliteral2;}

    public String toString() {
        return Arrays.toString(cliterals);
    }

}
