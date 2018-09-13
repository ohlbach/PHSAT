package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Model;
import Datastructures.Symboltable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.function.Consumer;
import java.util.stream.Stream;

/**
 * Created by Ohlbach on 25.08.2018.
 * A clause consists of a clause number and an array of CLiterals.
 */
public class Clause extends AbstractClause {

    /** constructs a clause
     *
     * @param number   the clause number
     * @param maxSize  the maximum number of literals
     */
    public Clause(int number, int maxSize) {
        super(number,maxSize);}

}
