package Datastructures.Theory;

import it.unimi.dsi.fastutil.ints.IntArrayList;

import static Utilities.Utilities.addIntArray;

public class DisjointnessClass {
    public IntArrayList literals;
    public IntArrayList origins;

    public DisjointnessClass(IntArrayList literals, IntArrayList origins) {
        this.literals = literals;
        this.origins  = origins;}

    public void addLiteral(int literal, IntArrayList origins) {
        literals.add(literal);
        addIntArray(this.origins,origins);
    }


}
