package Datastructures.Links;

import Datastructures.Literals.CLiteral;

import java.util.Arrays;

/**
 * Created by ohlbach on 29.08.2018.
 */
public class LinkBinary {
    public CLiteral[] cliterals = new CLiteral[2];

    public LinkBinary(CLiteral cliteral1, CLiteral cliteral2) {
        cliterals[0] = cliteral1;
        cliterals[1] = cliteral2;}

    public String toString() {
        return Arrays.toString(cliterals);
    }

}
