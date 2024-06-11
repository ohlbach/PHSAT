package Solvers.Normalizer;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.function.Consumer;


public class ClauseTest extends TestCase {
    int nand = Quantifier.AND.ordinal();
    int nor = Quantifier.OR.ordinal();
    int natl = Quantifier.ATLEAST.ordinal();
    int natm = Quantifier.ATMOST.ordinal();
    int nex = Quantifier.EXACTLY.ordinal();
    int nint = Quantifier.INTERVAL.ordinal();

    static Symboltable symboltable = new Symboltable(10);
    static Consumer<String> monitor = (string -> System.out.println(string));




    public void testRemoveLiteral() throws Unsatisfiable {
        System.out.println("remove literal");
        StringBuilder errors = new StringBuilder();
        Clause clause1 = new Clause(new int[]{1, nint, 1,2, 1, 2, 3},true,null);
        assertEquals(0,  clause1.removeLiteral(1, true,null,monitor,  null));
        assertEquals("1.2: 2v3",clause1.toString(null,0));
        InferenceStep step = clause1.inferenceSteps.get(1);
        System.out.println(step.toString(null));

        IntArrayList trueLits = new IntArrayList();
        ArrayList<InferenceStep> steps = new ArrayList<>();
        clause1 = new Clause(new int[]{5, nint, 2,3, 1, 2, 3},true,null);
        assertEquals(1,  clause1.removeLiteral(1, true,
                ((lit,iStep) -> {trueLits.add(lit); steps.add(iStep);}),monitor,  null));
        assertEquals("[2, 3]",trueLits.toString());
        System.out.println(steps.get(0).toString(null));
        System.out.println(steps.get(1).toString(null));
        assertTrue(steps.get(0).verify(monitor,null));



    }

    }