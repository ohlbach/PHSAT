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
    public void addImmediately1() {
        System.out.println("addImmediately1");
        Model model = new Model(5,null);
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
        }

    @Test
    public void addImmediately2() {
        System.out.println("addImmediately with origins");
        Model model = new Model(5, null);
        model.addImmediately(1);
        model.addImmediately(-2);
        model.addImmediately(3);
        assertEquals("1,-2,3", model.infoString(false));
    }
    @Test
    public void addImmediately3()  {
        System.out.println("addImmediately with symboltable");
        Symboltable symboltable = new Symboltable(5);
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        Model model = new Model(5,symboltable);
        model.addImmediately(1);
        model.addImmediately(-2);
        assertEquals("p,-q",model.toString());
        assertEquals("1,-2",model.toNumbers());

    }
    @Test
    public void add() throws Exception {
        System.out.println("add");
        Symboltable symboltable = new Symboltable(5);
        symboltable.setName(1, "p");
        symboltable.setName(2, "q");
        symboltable.setName(3, "r");
        Model model = new Model(5, symboltable);
        IntArrayList lits = new IntArrayList();
        ArrayList<InferenceStep> infs = new ArrayList<>();
        model.addObserver(Thread.currentThread(),
                ((Integer literal, InferenceStep inference) -> {lits.add((int)literal); infs.add(inference);}));
        InferenceTest inf1 = new InferenceTest("comment1");
        InferenceTest inf2 = new InferenceTest("comment2");
        model.add(-2, inf1, null);
        model.add(1, inf2, Thread.currentThread());
        assertEquals(1,lits.size());
        assertEquals(1,infs.size());
        assertEquals("1,-2",model.toNumbers());
        assertEquals("p,-q",model.toString());
        System.out.println(model.infoString(false));
    }

    @Test
    public void addThrow() throws Exception {
        System.out.println("add with Throw");
        Symboltable symboltable = new Symboltable(5);
        symboltable.setName(1, "p");
        symboltable.setName(2, "q");
        symboltable.setName(3, "r");
        Model model = new Model(5, symboltable);
        InferenceTest inf1 = new InferenceTest("comment1");
        InferenceTest inf2 = new InferenceTest("comment2");
        InferenceTest inf3 = new InferenceTest("comment3");

        model.add(1,inf1,null);
        model.add(2,inf2,null);
        try {
            model.add(-2,inf3,null);}
        catch(Unsatisfiable unsat) {System.out.println(unsat);}}

}