package Datastructures.Links;

import Datastructures.Literals.CLiteral;

import java.util.ArrayList;

/**
 * Created by ohlbach on 29.08.2018.
 */
public class LinkMultiple {
    public ArrayList<CLiteral> literals;

    public void addLiteral(CLiteral literal) {
        literals.add(literal);
    }

    public void removeLiteral(CLiteral literal) {
        literals.remove(literal);
    }

    public int size() {return literals.size();}

    public String toString() {
        return literals.toString();
    }
}
