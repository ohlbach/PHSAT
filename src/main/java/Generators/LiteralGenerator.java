package Generators;


import Datastructures.Literals.CLiteral;

/**
 * Created by ohlbach on 26.08.2018.
 *
 * This is a Factory class for generating literals.
 * The generator creates just CLiterals.
 * The class can be subclassed to generate more specific literal types.
 */
public class LiteralGenerator {

    /** generates just a CLiteral
     *
     * @param literal the literal
     * @return a new CLiteral (without pointer to a class).
     */
    public CLiteral newLiteral(int literal) {
        return new CLiteral(literal);}

    /** for delivering an info string.
     *
     * @return an info string
     */
    public String toString() {
        return "The Literal Generator creates CLiterals.";}
}
