package Datastructures.Theory;

import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

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
        assertEquals("1@10,11\n" +
                "-2\n" +
                "3@30", model.infoString(false));
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
}