package Solvers.RandomWalker;

import Datastructures.Theory.Model;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 07.05.2019.
 */
public class RWModelTest {
    @Test
    public void isUnassigned() throws Exception {
        System.out.println("isUnassigned isTrue isFalse");
        RWModel model = new RWModel(10);
        System.out.println(model.toString());
        for(int pred = 1; pred <= 10; ++pred) {
            assertTrue(model.isUnassigned(pred));
            assertTrue(model.isUnassigned(-pred));
            assertFalse(model.isTrue(pred));
            assertFalse(model.isTrue(-pred));
            assertFalse(model.isFalse(pred));
            assertFalse(model.isFalse(-pred));}}

    @Test
    public void makeTrue() throws Exception {
        System.out.println("makeTrue makeFalse");
        RWModel model = new RWModel(10);
        model.makeTrue(2);
        model.makeTrue(-4);
        model.makeFalse(6);
        model.makeFalse(-8);
        assertFalse(model.isUnassigned(2));
        assertFalse(model.isUnassigned(-4));
        assertTrue(model.isTrue(2));
        assertTrue(model.isTrue(-4));
        assertTrue(model.isTrue(-6));
        assertTrue(model.isTrue(8));


        assertTrue(model.isFalse(-2));
        assertTrue(model.isFalse(4));
        assertTrue(model.isFalse(6));
        assertTrue(model.isFalse(-8));

        System.out.println(model.toString());
    }


    @Test
    public void flip() throws Exception {
        System.out.println("flip");
        RWModel model = new RWModel(10);
        model.makeTrue(2);
        model.makeTrue(-4);
        model.flip(2);
        model.flip(4);
        assertTrue(model.isFalse(2));
        assertTrue(model.isTrue(4));
        System.out.println(model.toString());
    }

    @Test
    public void predicates() throws Exception {
        System.out.println("predicates");
        RWModel model = new RWModel(10);
        assertEquals(10,model.predicates());

    }

    @Test
    public void globalModel() throws Exception {
        System.out.println("global model");
        Model globalModel = new Model(10);
        globalModel.add(4);
        globalModel.add(-6);
        RWModel model = new RWModel(globalModel);
        assertTrue(model.isTrue(4));
        assertTrue(model.isTrue(-6));
        System.out.println(model.toString());
    }
}