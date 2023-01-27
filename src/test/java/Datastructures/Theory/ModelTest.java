package Datastructures.Theory;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import InferenceSteps.InferenceTest;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 25.08.2018.
 */
public class ModelTest {

    @Test
    public void addImmediately() {
        System.out.println("addImmediately");
        Model model = new Model(5);
        model.addImmediately(1);
        model.addImmediately(-2);
        assertEquals("1,-2",model.toString());
        assertEquals(1,model.status(1));
        assertEquals(-1,model.status(-1));
        assertEquals(-1,model.status(2));
        assertEquals(1,model.status(-2));
        assertEquals(0,model.status(3));
        assertTrue(model.isTrue(1));
        assertTrue(model.isFalse(-1));
        assertTrue(model.isTrue(-2));
        assertTrue(model.isFalse(2));
        assertFalse(model.isTrue(3));

        Symboltable symboltable = new Symboltable(5);
        symboltable.setName(1,"A");
        symboltable.setName(2,"B");
        assertEquals("A,-B",model.toString(symboltable));
        }


    @Test
    public void add() throws Exception {
        System.out.println("add");
        Symboltable symboltable = new Symboltable(5);
        symboltable.setName(1, "p");
        symboltable.setName(2, "q");
        symboltable.setName(3, "r");
        Model model = new Model(5);
        IntArrayList lits = new IntArrayList();
        ArrayList<InferenceStep> infs = new ArrayList<>();
        model.addObserver(
                ((Integer literal, InferenceStep inference) -> {lits.add((int)literal); infs.add(inference);}));
        InferenceTest inf1 = new InferenceTest("comment1");
        InferenceTest inf2 = new InferenceTest("comment2");
        model.add(-2, inf1);
        model.add(1, inf2);
        assertEquals(2,lits.size());
        assertEquals(2,infs.size());
        assertEquals("1,-2",model.toString());
        assertEquals("p,-q",model.toString(symboltable));
    }

    @Test
    public void addThrow() throws Exception {
        System.out.println("add with Throw");
        Symboltable symboltable = new Symboltable(5);
        symboltable.setName(1, "p");
        symboltable.setName(2, "q");
        symboltable.setName(3, "r");
        Model model = new Model(5);
        InferenceTest inf1 = new InferenceTest("comment1");
        InferenceTest inf2 = new InferenceTest("comment2");
        InferenceTest inf3 = new InferenceTest("comment3");

        model.add(1,inf1);
        model.add(2,inf2);
        try {model.add(-2,inf3);}
        catch(Unsatisfiable unsat) {System.out.println(unsat);}}

    @Test
    public void cloneTest() throws Exception {
        System.out.println("clone");
        Model model = new Model(5);
        InferenceTest inf1 = new InferenceTest("comment1");
        InferenceTest inf2 = new InferenceTest("comment2");
        model.add(-2, inf1);
        model.add(1, inf2);
        assertEquals(2,model.inferenceSteps.size());
        assertEquals("1,-2",model.toString());
        Model model1 = model.clone();
        assertEquals("1,-2",model1.toString());
        assertEquals(2,model1.inferenceSteps.size());
        assertTrue(model1.isTrue(1));
        assertTrue(model1.isTrue(-2));
        assertEquals(inf2,model1.getInferenceStep(1));
        assertEquals(inf1,model1.getInferenceStep(2));


    }
    }