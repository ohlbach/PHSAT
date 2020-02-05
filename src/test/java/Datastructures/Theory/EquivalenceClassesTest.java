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
        int[] clause = new int[]{1,4,4,1,2,3,2};
        assertTrue(eq.addEquivalenceClass(clause));
        //System.out.println(eq.toString());
        assertEquals(1,eq.mapToRepresentative(4));
        assertEquals(-1,eq.mapToRepresentative(-4));
        assertEquals("[1]",eq.mapToOrigins(4).toString());
        assertEquals("[1]",eq.mapToOrigins(-2).toString());
    }

    @Test
    public void addEquivalenceClass1() throws Exception {
        System.out.println("unit");
        String[] result = new String[1];
        EquivalenceClasses eq = new EquivalenceClasses(symboltable, cH, ((l,o)-> result[0] = "U " + l + " " + o.toString()));
        int[] clause = new int[]{1, 4, 2, 2};
        assertTrue(eq.addEquivalenceClass(clause));
        assertEquals("U 2 [1]",result[0]);
    }

    @Test
    public void addEquivalenceClass2() throws Exception {
        System.out.println("contradict");
        String[] result = new String[1];
        EquivalenceClasses eq = new EquivalenceClasses(symboltable, ((l,o)-> {result[0] = "U " + l + " " + o.toString();}),uH);
        int[] clause = new int[]{1, 4, 2, -2};
        assertFalse(eq.addEquivalenceClass(clause));
        assertEquals("U 2 [1]",result[0]);
    }



    @Test
    public void addJoins() throws Exception {
        System.out.println("add, with joins");
        EquivalenceClasses eq = new EquivalenceClasses(symboltable,cH,uH);
        int[] clause = new int[]{1,4,4,1,2,3};
        eq.addEquivalenceClass(clause);
        clause = new int[]{2,4,5,6,3,2};
        eq.addEquivalenceClass(clause);
        //System.out.println(eq.toString());
        assertEquals(1,eq.mapToRepresentative(5));
        assertEquals(-1,eq.mapToRepresentative(-5));
    }

    @Test
    public void addJoins1() throws Exception {
        System.out.println("add, more joins");
        EquivalenceClasses eq = new EquivalenceClasses(symboltable,cH,uH);
        assertTrue(eq.addEquivalenceClass(new int[]{1,4,1,2,3}));
        assertTrue(eq.addEquivalenceClass(new int[]{2,4,4,5,6}));
        assertTrue(eq.addEquivalenceClass(new int[]{3,4,7,8,9}));
        assertTrue(eq.addEquivalenceClass(new int[]{4,4,3,6,7,9}));
        //System.out.println(eq.toString());
        assertEquals(1,eq.mapToRepresentative(9));
        assertEquals(-1,eq.mapToRepresentative(-9));
        assertEquals("[1, 2, 3, 4]",eq.mapToOrigins(-8).toString());
    }

    @Test
    public void addJoins2() throws Exception {
        System.out.println("add, more joins, negated");
        EquivalenceClasses eq = new EquivalenceClasses(symboltable,cH,uH);
        assertTrue(eq.addEquivalenceClass(new int[]{1,4,1,2,3}));
        assertTrue(eq.addEquivalenceClass(new int[]{2,4,4,5,6}));
        assertTrue(eq.addEquivalenceClass(new int[]{3,4,7,8,9}));
        assertTrue(eq.addEquivalenceClass(new int[]{4,4,-3,6,4,-9}));
        //System.out.println(eq.toString());
        assertEquals(1,eq.mapToRepresentative(9));
        assertEquals(-1,eq.mapToRepresentative(-9));
        assertEquals("[1, 2, 3, 4]",eq.mapToOrigins(-8).toString());
    }


    @Test
    public void addJoins3() throws Exception {
        System.out.println("add, more joins, contradiction");
        String[] result = new String[1];
        EquivalenceClasses eq = new EquivalenceClasses(symboltable,((l,o)-> {result[0] = "C " + l + " " + o.toString();}),uH);
        assertTrue(eq.addEquivalenceClass(new int[]{1,4,1,2,3}));
        assertTrue(eq.addEquivalenceClass(new int[]{2,4,4,5,6}));
        assertTrue(eq.addEquivalenceClass(new int[]{3,4,7,8,9}));
        assertFalse(eq.addEquivalenceClass(new int[]{4,4,-3,6,7,-9}));
        assertEquals("C 7 [1, 2, 3, 4]",result[0]);
        //System.out.println(eq.toString());

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
        //System.out.println(reason[0]);
        assertEquals("1 [1, 2]",reason[0]);
    }

    @Test
    public void addDouble1() throws Exception {
        System.out.println("add double 1");
        String[] result = new String[1];
        EquivalenceClasses eq = new EquivalenceClasses(symboltable, ((l,o)-> {result[0] = "C " + l + " " + o.toString();}),
                ((l,o)-> {result[0] = "U " + l + " " + o.toString();}));
        IntArrayList origins = IntArrayList.wrap(new int[]{10,11,12});
        assertTrue(eq.addEquivalenceClass(1,2,origins));
        assertEquals(1,eq.mapToRepresentative(2));
        assertEquals(-1,eq.mapToRepresentative(-2));
        assertEquals("[10, 11, 12]",eq.mapToOrigins(2).toString());
        origins = IntArrayList.wrap(new int[]{14,15,16});
        assertTrue(eq.addEquivalenceClass(3,3,origins));
        assertEquals("U 3 [14, 15, 16]",result[0]);
        origins = IntArrayList.wrap(new int[]{17,18,19});
        assertFalse(eq.addEquivalenceClass(4,-4,origins));
        assertEquals("C 4 [17, 18, 19]",result[0]);
    }
    @Test
    public void addDouble2() throws Exception {
        System.out.println("add double join");
        String[] result = new String[1];
        EquivalenceClasses eq = new EquivalenceClasses(symboltable, ((l,o)-> {result[0] = "C " + l + " " + o.toString();}),
                ((l,o)-> {result[0] = "U " + l + " " + o.toString();}));
        IntArrayList origins = IntArrayList.wrap(new int[]{10,11,12});
        assertTrue(eq.addEquivalenceClass(1,2,origins));
        origins = IntArrayList.wrap(new int[]{14,15});
        assertTrue(eq.addEquivalenceClass(3,4,origins));
        origins = IntArrayList.wrap(new int[]{14,15});
        assertTrue(eq.addEquivalenceClass(2,-4,origins));
        //System.out.println(eq);
        assertEquals(-1,eq.mapToRepresentative(3));
        assertEquals(1,eq.mapToRepresentative(-4));
        assertEquals("[10, 11, 12, 14, 15]",eq.mapToOrigins(4).toString());
    }

    @Test
    public void truths() throws Exception {
        System.out.println("truth");
        EquivalenceClasses eq = new EquivalenceClasses(symboltable,cH,uH);
        assertTrue(eq.addEquivalenceClass(new int[]{1,4,1,2,3}));
        assertTrue(eq.addEquivalenceClass(new int[]{2,4,4,5,6}));
        assertTrue(eq.addEquivalenceClass(new int[]{3,4,7,8,9}));
        assertTrue(eq.addEquivalenceClass(new int[]{4,4,3,6,7,9}));
        //System.out.println(eq.toString());
        IntArrayList truths = new IntArrayList();
        assertTrue(eq.truths(6,truths));
        assertEquals("[1, 2, 3, 4, 5, 7, 8, 9]",truths.toString());
        truths.clear();
        assertTrue(eq.truths(-6,truths));
        assertEquals("[-1, -2, -3, -4, -5, -7, -8, -9]",truths.toString());
    }




        @Test
    public void completeModel() throws Exception {
        System.out.println("complete model");
        EquivalenceClasses eq = new EquivalenceClasses(symboltable,cH,uH);
        int[] clause = new int[]{1,4,4,1,2,3};
        eq.addEquivalenceClass(clause);
        clause = new int[]{2,4,2,3,5};
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