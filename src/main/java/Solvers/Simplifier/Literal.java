package Solvers.Simplifier;

import Datastructures.Symboltable;

public class Literal {

    public int literal;
    public int multiplicity;
    public Clause clause;
    public Literal previousLiteral;
    public Literal nextLiteral;

    public Literal(int literal, int multiplicity) {
        this.literal = literal;
        this.multiplicity = multiplicity;
    }

    public String toString() {
        return toString(null);}

    public String toString(Symboltable symboltable) {
        String lit = Symboltable.toString(literal,symboltable);
        return (multiplicity == 1) ? lit : lit+"^"+multiplicity;}
}
