package Datastructures;

import Datastructures.Clauses.Quantifier;
import junit.framework.TestCase;

import java.util.function.Function;

public class ClauseTest extends TestCase {

    static Function<Integer,Literal> litCreator = (literal) -> new Literal(literal,1);
    static int or = Quantifier.OR.ordinal();
    static Symboltable symboltable = new Symboltable(10);
    static {symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
    }

    public void testConstructor1() {
        System.out.println("Constructor1");
        Clause c1 = new Clause(new int[]{1,or,1,-2,3},true,litCreator,symboltable);
        assertEquals("1: pv-qvr",c1.toString(symboltable,0));
        assertEquals("1: 1v-2v3",c1.toString(null,0));
        assertEquals("Input: Clause 1: 1v-2v3 -> 1: pv-qvr",c1.inferenceSteps.get(0).toString());

        c1 = new Clause(new int[]{2,or,1,-2,3,1,-2,1},true,litCreator,symboltable);
        assertEquals("2: pv-qvr",c1.toString(symboltable,0));
        assertEquals("2: 1v-2v3",c1.toString(null,0));
        assertEquals("Input: Clause 2: 1v-2v3v1v-2v1 -> 2: pv-qvr",c1.inferenceSteps.get(0).toString());

        c1 = new Clause(new int[]{3,or,1,-2,3,1,2,1},true,litCreator,symboltable);
        assertEquals("3: >=0 p^3,r",c1.toString(symboltable,0));
        assertEquals("Input: Clause 3: 1v-2v3v1v2v1 -> 3: >=0 p^3,r",c1.inferenceSteps.get(0).toString());

    }


    public void testClassifyQuantifier() {
    }

    public void testIsEmpty() {
    }

    public void testFindLiteral() {
    }

    public void testSimplify() {
    }

    public void testSimplifyRecursively() {
    }

    public void testGetModels() {
    }

    public void testSingletonModel() {
    }

    public void testExtractTrueLiterals() {
    }

    public void testExtractIrrelevantLiterals() {
    }

    public void testDivideByGCD() {
    }

    public void testRemoveLiteral() {
    }

    public void testSize() {
    }

    public void testExpandedSize() {
    }

    public void testIsTrue() {
    }

    public void testIsFalse() {
    }

    public void testSimpleClone() {
    }

    public void testTestToString() {
    }

    public void testModelString() {
    }

    public void testTestToString1() {
    }
}