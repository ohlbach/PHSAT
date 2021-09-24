package Datastructures.Clauses;

import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import static org.junit.Assert.*;

public class MRMatrixTest {

    static Clause[] combination = new Clause[4];
    static Symboltable symboltable = new Symboltable(20);
    static{
        symboltable.setName(1,"a");
        symboltable.setName(2,"b");
        symboltable.setName(3,"c");
        symboltable.setName(4,"d");
        symboltable.setName(5,"e");
        symboltable.setName(6,"f");
        symboltable.setName(7,"g");
        symboltable.setName(8,"h");

        combination[0] = new Clause(1,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{1,2}),null);
        combination[1] = new Clause(2,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{2,3,4,-6}),null);
        combination[2] = new Clause(344,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{3,-4,-5}),null);
        combination[3] = new Clause(4,ClauseType.DISJOINT,IntArrayList.wrap(new int[]{5,6,7}),null);
    }

    @Test
    public void infoString() {
        System.out.println("infoString");
        MRMatrix matrix = new MRMatrix(combination,symboltable,null,true);
        System.out.println(matrix.infoString(symboltable));

    }
}