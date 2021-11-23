package Datastructures.Links;

import Datastructures.Literals.CLiteralOld;

import java.util.ArrayList;

/**
 * Created by ohlbach on 29.08.2018.
 */
public class LinkMultiple {
    public ArrayList<CLiteralOld> literals;

    public void addLiteral(CLiteralOld literal) {
        literals.add(literal);
    }

    public void removeLiteral(CLiteralOld literal) {
        literals.remove(literal);
    }

    public int size() {return literals.size();}

    public String toString() {
        return literals.toString();
    }
}
