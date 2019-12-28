package Datastructures.Theory;

import org.junit.Test;

import java.util.Arrays;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 08.12.2019.
 */
public class EquivalenceClassesTest {

    static Consumer<String> contradictionHandler = (reason -> System.out.println("CL "+ reason));
    @Test
    public void addEquivalenceClass() throws Exception {
        System.out.println("add, no joins");
        EquivalenceClasses eq = new EquivalenceClasses(null,null);
        int[] clause = new int[]{0,0,4,1,2,3};
        eq.addEquivalenceClass(clause);
        //System.out.println(eq.toString());
        assertEquals(1,eq.mapToRepresentative(4));
        assertEquals(-1,eq.mapToRepresentative(-4));
    }

    @Test
    public void addJoins() throws Exception {
        System.out.println("add, with joins");
        EquivalenceClasses eq = new EquivalenceClasses(null,null);
        int[] clause = new int[]{0,0,4,1,2,3};
        eq.addEquivalenceClass(clause);
        clause = new int[]{0,0,2,3,5};
        eq.addEquivalenceClass(clause);
        //System.out.println(eq.toString());
        assertEquals(1,eq.mapToRepresentative(5));
        assertEquals(-1,eq.mapToRepresentative(-5));
    }

    @Test
    public void addContradictions() throws Exception {
        System.out.println("add, with contradiction");
        String[] reason = new String[1];
        EquivalenceClasses eq = new EquivalenceClasses(null,(r->reason[0] = r));
        int[] clause = new int[]{0,0,4,1,2,3};
        eq.addEquivalenceClass(clause);
        clause = new int[]{0,0,2,3,-4};
        eq.addEquivalenceClass(clause);
        //System.out.println(reason[0]);
        assertEquals("Equivalence 2 = -4 contradicts existing equivalences: \n" +
                "1 = 2 = 3 = 4",reason[0]);
    }

    @Test
    public void completeModel() throws Exception {
        System.out.println("complete model");
        EquivalenceClasses eq = new EquivalenceClasses(null,contradictionHandler);
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