package Datastructures;

import Datastructures.Clauses.Quantifier;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.function.Consumer;
import java.util.function.Function;

public class ClauseTest extends TestCase {

    static Function<Integer,Literal> litCreator = (literal) -> new Literal(literal,1);
    static Consumer<String> monitor = System.out::println;
    static int or = Quantifier.OR.ordinal();
    static int atl = Quantifier.ATLEAST.ordinal();
    static int atm = Quantifier.ATMOST.ordinal();
    static int intv = Quantifier.INTERVAL.ordinal();

    static Symboltable symboltable = new Symboltable(10);
    static {symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
    }

    private String toString(IntArrayList models, Clause clause) {
        StringBuilder st = new StringBuilder();
        for(int model : models) st.append(clause.modelString(model,null)).append("\n");
        return st.toString();}

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

        c1 = new Clause(new int[]{4,atl,2,1,-2,3,1,2,1},true,litCreator,symboltable);
        assertEquals("4: p^3vr",c1.toString(symboltable,0));
        assertEquals("Input: Clause 4: >= 2 1,-2,3,1,2,1 -> 4: p^3vr",c1.inferenceSteps.get(0).toString());

        c1 = new Clause(new int[]{5,atl,2,1,2,3,1,2,1},true,litCreator,symboltable);
        assertEquals("5: >=2 p^2,q^2,r",c1.toString(symboltable,0));
        assertEquals("Input: Clause 5: >= 2 1,2,3,1,2,1 -> 5: >=2 p^2,q^2,r",c1.inferenceSteps.get(0).toString());

        c1 = new Clause(new int[]{6,intv,2,3, 1,1,1,2,2,3,3,3},true,litCreator,null);
        assertEquals("6: [2,3] 1^3,2^2,3^3",c1.toString(null,0));
        assertEquals("Input: Clause 6: 2-3: 1,1,1,2,2,3,3,3 -> 6: [2,3] 1^3,2^2,3^3",c1.inferenceSteps.get(0).toString());

        c1 = new Clause(new int[]{7,intv,2,3, 1,-1,1,2,-2,3,-3,3},true,litCreator,null);
        assertEquals("7: =0 1,3",c1.toString(null,0));
        assertEquals("Input: Clause 7: 2-3: 1,-1,1,2,-2,3,-3,3 -> 7: =0 1,3",c1.inferenceSteps.get(0).toString());

        c1 = new Clause(new int[]{8,atm,2, 1,-1,1,2,-2,3,-3,3},true,litCreator,null);
        assertEquals("8: <=-1 1,3",c1.toString(null,0));
        assertEquals("Input: Clause 8: <= 2 1,-1,1,2,-2,3,-3,3 -> 8: <=-1 1,3",c1.inferenceSteps.get(0).toString());

        c1 = new Clause(new int[]{9,intv,2,3, 1,-1,1,2,-2,3,-3,3,-3},true,litCreator,null);
        assertEquals("9: <=-1 1",c1.toString(null,0));
        assertEquals("Input: Clause 9: 2-3: 1,-1,1,2,-2,3,-3,3,-3 -> 9: <=-1 1",c1.inferenceSteps.get(0).toString());

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
        System.out.println("getModels");
        Clause c = new Clause(new int[]{1,or,1,2,3},true,litCreator,null);
        IntArrayList models = c.getModels(monitor,null);
        assertEquals("1,-2,-3\n" +
                "-1,2,-3\n" +
                "1,2,-3\n" +
                "-1,-2,3\n" +
                "1,-2,3\n" +
                "-1,2,3\n" +
                "1,2,3\n",toString(models,c));

        c = new Clause(new int[]{2,or,1,-2,3},true,litCreator,null);
        models = c.getModels(monitor,null);
        assertEquals("-1,-2,-3\n" +
                "1,-2,-3\n" +
                "1,2,-3\n" +
                "-1,-2,3\n" +
                "1,-2,3\n" +
                "-1,2,3\n" +
                "1,2,3\n",toString(models,c));

        c = new Clause(new int[]{3,intv,2,2,1,2,3,4,5},true,litCreator,null);
        models = c.getModels(monitor,null);
        assertEquals("1,2,-3,-4,-5\n" +
                "1,-2,3,-4,-5\n" +
                "-1,2,3,-4,-5\n" +
                "1,-2,-3,4,-5\n" +
                "-1,2,-3,4,-5\n" +
                "-1,-2,3,4,-5\n" +
                "1,-2,-3,-4,5\n" +
                "-1,2,-3,-4,5\n" +
                "-1,-2,3,-4,5\n" +
                "-1,-2,-3,4,5\n",toString(models,c));

        c = new Clause(new int[]{4,intv,2,2,1,1,2,2,3,3},true,litCreator,null);
        models = c.getModels(monitor,null);
        assertEquals("1,-2,-3\n" +
                "-1,2,-3\n" +
                "-1,-2,3\n",toString(models,c));

        c = new Clause(new int[]{5,intv,2,2,1,1,-2,-2,3,3},true,litCreator,null);
        models = c.getModels(monitor,null);
        assertEquals("-1,-2,-3\n" +
                "1,2,-3\n" +
                "-1,2,3\n",toString(models,c));

        c = new Clause(new int[]{6,intv,1,3,1,1,2,2},true,litCreator,null);
        models = c.getModels(monitor,null);
        assertEquals("6.1: =2 1^2,2^2",c.toString(null,0));
        assertEquals("1,-2\n" +
                "-1,2\n",toString(models,c));
    }

    public void testSingletonModel() {
        System.out.println("Singleton Model");
        Clause<Literal> c = new Clause<>(new int[]{1, intv,3,3, 1,1, -2}, true, litCreator, null);
        assertEquals("1: =3 1^2,-2",c.toString(null,0));
        IntArrayList models = c.getModels(monitor,null);
        assertEquals(1,models.size());
        IntArrayList truth = new IntArrayList();
        ArrayList<InferenceStep> steps = new ArrayList<>();
        c.singletonModel(models.getInt(0),true,(literal,step)-> {truth.add(literal);steps.add(step);}, monitor,null);
        assertEquals("[1, -2]",truth.toString());
        assertEquals(2,steps.size());
        System.out.println(steps.get(0).toString(null));
        System.out.println(steps.get(1).toString(null));
    }

    public void testExtractTrueLiterals() {
        System.out.println("extractTrueLiterals");
        Clause c = new Clause(new int[]{1, intv,2,2, 1,1,1,2,3,4}, true, litCreator, symboltable);
        assertEquals("1: =2 1^3,2,3,4", c.toString(null, 0));
        IntArrayList models = c.getModels(monitor,null);
        assertEquals("-1,2,3,-4\n" +
                "-1,2,-3,4\n" +
                "-1,-2,3,4\n",toString(models,c));
        IntArrayList truth = new IntArrayList();
        ArrayList<InferenceStep> steps = new ArrayList<>();
        IntArrayList removes = new IntArrayList();
        c.extractTrueLiterals(models,true, (literalObject) -> {removes.add(((Literal)literalObject).literal);},
                (literal,step)-> {truth.add((Integer)literal);steps.add((InferenceStep) step);}, monitor,null);
        assertEquals("[-1]",truth.toString());
        System.out.println(steps.get(0).toString(null));

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
        System.out.println("modelString");
        Clause c1 = new Clause(new int[]{1,or,1,2,3},true,litCreator,null);
        Clause c2 = new Clause(new int[]{2,or,1,-2,3},true,litCreator,null);
        assertEquals("-1,-2,-3",c1.modelString(0,null));
        assertEquals("-1,-2,-3",c2.modelString(0,null));
        assertEquals("1,2,-3",c1.modelString(3,null));
        assertEquals("1,2,-3",c2.modelString(3,null));

    }

    public void testTestToString1() {
    }
}