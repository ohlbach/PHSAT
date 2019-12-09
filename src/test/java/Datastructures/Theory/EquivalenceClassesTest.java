package Datastructures.Theory;

import org.junit.Test;

import java.util.Arrays;
import java.util.function.BiConsumer;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 08.12.2019.
 */
public class EquivalenceClassesTest {

    static BiConsumer<int[],Integer> contradictionHandler = ((clause,literal) -> {
        System.out.println("CL "+ Arrays.toString(clause));
        System.out.println("LI "+ literal);});
    @Test
    public void addEquivalenceClass() throws Exception {
        System.out.println("add, no joins");
        EquivalenceClasses eq = new EquivalenceClasses(contradictionHandler);
        int[] clause = new int[]{0,0,4,1,2,3};
        eq.addEquivalenceClass(clause);
        //System.out.println(eq.toString());
        assertEquals(1,eq.mapToRepresentative(4));
        assertEquals(-1,eq.mapToRepresentative(-4));
    }

    @Test
    public void addJoins() throws Exception {
        System.out.println("add, with joins");
        EquivalenceClasses eq = new EquivalenceClasses(contradictionHandler);
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
        Object[] contr = new Object[2];
        BiConsumer<int[],Integer> contradictionHandler = ((clause,literal) -> {contr[0] = clause; contr[1] = literal;});
        EquivalenceClasses eq = new EquivalenceClasses(contradictionHandler);
        int[] clause = new int[]{0,0,4,1,2,3};
        eq.addEquivalenceClass(clause);
        clause = new int[]{0,0,2,3,-4};
        eq.addEquivalenceClass(clause);
        assertEquals(clause, contr[0]);
        assertEquals(4, contr[1]);
    }

    @Test
    public void completeModel() throws Exception {
        System.out.println("complete model");
        EquivalenceClasses eq = new EquivalenceClasses(contradictionHandler);
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