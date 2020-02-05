package Datastructures.Theory;

import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import javax.swing.plaf.synth.SynthMenuBarUI;
import java.util.Arrays;
import java.util.function.BiConsumer;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 08.12.2019.
 */
public class EquivalenceClassesTest {

    static BiConsumer<Integer,IntArrayList> cH =
            ((literal,origin) -> {System.out.println("CL "+ literal);
            if(origin != null) System.out.println(origin.toString());});

    static BiConsumer<Integer,IntArrayList> uH =
            ((literal,origin) -> {System.out.println("UN "+ literal);
                if(origin != null) System.out.println(origin.toString());});


    static Symboltable symboltable = null;
    @Test
    public void addEquivalenceClass() throws Exception {
        System.out.println("add, no joins");
        EquivalenceClasses eq = new EquivalenceClasses(symboltable,cH,uH);
        int[] clause = new int[]{1,4,4,1,2,3};
        eq.addEquivalenceClass(clause);
        //System.out.println(eq.toString());
        assertEquals(1,eq.mapToRepresentative(4));
        assertEquals(-1,eq.mapToRepresentative(-4));
    }

    @Test
    public void addJoins() throws Exception {
        System.out.println("add, with joins");
        EquivalenceClasses eq = new EquivalenceClasses(symboltable,cH,uH);
        int[] clause = new int[]{1,4,4,1,2,3};
        eq.addEquivalenceClass(clause);
        clause = new int[]{2,4,5,6,3,2};
        eq.addEquivalenceClass(clause);
        System.out.println(eq.toString());
        assertEquals(1,eq.mapToRepresentative(5));
        assertEquals(-1,eq.mapToRepresentative(-5));
    }

    @Test
    public void addContradictions() throws Exception {
        System.out.println("add, with contradiction");
        String[] reason = new String[1];
        EquivalenceClasses eq = new EquivalenceClasses(symboltable,
                ((r,o)->reason[0] = r.toString() + " " + o.toString()),uH);
        int[] clause = new int[]{1,4,4,1,2,3};
        eq.addEquivalenceClass(clause);
        clause = new int[]{2,4,2,3,-4};
        eq.addEquivalenceClass(clause);
        System.out.println(reason[0]);
        assertEquals("1 [1, 2]",reason[0]);
    }

    @Test
    public void completeModel() throws Exception {
        System.out.println("complete model");
        EquivalenceClasses eq = new EquivalenceClasses(symboltable,cH,uH);
        int[] clause = new int[]{0,0,4,1,2,3};
        eq.addEquivalenceClass(clause);
        clause = new int[]{0,0,2,3,5};
        eq.addEquivalenceClass(clause);
        Model model = new Model(5);
        model.add(1);
        eq.completeModel(model);
        assertEquals(1,model.status(1));
        assertEquals(1,model.status(2));
        assertEquals(1,model.status(3));
        assertEquals(1,model.status(4));
        assertEquals(1,model.status(5));
    }
}