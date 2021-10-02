package Datastructures.Theory;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.function.Consumer;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 25.08.2018.
 */
public class ModelTest {

    @Test
    public void addImmediately1() throws Exception {
        System.out.println("addImmediately1");
        Model model = new Model(5,null);
        model.addImmediately(1,null);
        model.addImmediately(-2,null);
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
    public void addImmediately2() throws Exception {
        System.out.println("addImmediately with origins");
        Model model = new Model(5, null);
        IntArrayList origins1 = new IntArrayList();
        origins1.add(10);
        origins1.add(11);
        model.addImmediately(1, origins1);
        model.addImmediately(-2, null);
        IntArrayList origins3 = new IntArrayList();
        origins3.add(30);
        model.addImmediately(3, origins3);
        assertEquals("1 @ [10, 11]\n" +
                "-2\n" +
                "3 @ [30]", model.infoString(false));
        assertEquals("[10, 11]",model.getOrigin(1).toString());
        assertEquals("[10, 11]",model.getOrigin(-1).toString());
        assertNull(model.getOrigin(2));
        assertEquals("[30]",model.getOrigin(3).toString());
        assertEquals("[30]",model.getOrigin(-3).toString());
    }
    @Test
    public void addImmediately3() throws Exception {
        System.out.println("addImmediately with symboltable");
        Symboltable symboltable = new Symboltable(5);
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        Model model = new Model(5,symboltable);
        model.addImmediately(1,null);
        model.addImmediately(-2,null);
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
        ArrayList<Object> observed = new ArrayList<>();
        model.addObserver(Thread.currentThread(),
                ((literal, originals) -> {
                    observed.add(literal); observed.add(originals);}));
        IntArrayList orig = new IntArrayList();
        orig.add(10);
        orig.add(20);
        model.add(1, null, null);
        assertEquals(1,observed.get(0));
        assertEquals("[10, 20]",observed.get(1).toString());
        orig = new IntArrayList();
        orig.add(30);
        orig.add(40);
        model.add(2, null, Thread.currentThread());
        assertEquals(2,observed.size());
        assertEquals("1,2",model.toNumbers());
        assertEquals("p,q",model.toString());
    }

    @Test
    public void addThrow() throws Exception {
        System.out.println("add with Throw");
        Symboltable symboltable = new Symboltable(5);
        symboltable.setName(1, "p");
        symboltable.setName(2, "q");
        symboltable.setName(3, "r");
        Model model = new Model(5, symboltable);
        model.add(1,null,null);
        IntArrayList orig1 = new IntArrayList(); orig1.add(20);
        model.add(2,null,null);
        try {
            IntArrayList orig2 = new IntArrayList(); orig2.add(30);
            model.add(-2,null,null);}
        catch(Unsatisfiable unsat) {
            System.out.println(unsat.toString());}}

}