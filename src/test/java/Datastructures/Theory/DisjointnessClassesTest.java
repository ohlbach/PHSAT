package Datastructures.Theory;

import org.junit.Test;
import org.omg.PortableServer.IMPLICIT_ACTIVATION_POLICY_ID;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 21.09.2018.
 */
public class DisjointnessClassesTest {

    
    @Test
    public void addDisjointnessClass() throws Exception {
        System.out.println("addDisjointnessClass");
        Model model = new Model(20);
        ImplicationDAG ig = new ImplicationDAG();
        DisjointnessClasses djc = new DisjointnessClasses(model,ig,null);
        djc.addDisjointnessClass(new int[]{1,0,1,2,3});
        djc.addDisjointnessClass(new int[]{2,0,3,4,5});
        assertEquals("Disjointenss Classes:\n" +
                "D1: (1,2,3)\n" +
                "D2: (3,4,5)\n",djc.toString());
        djc.addDisjointnessClass(new int[]{3,0,1,5});
        assertEquals("Disjointenss Classes:\n" +
                " D3: (1,5)\n" +
                "D3j: (3,4,5)\n" +
                " D1: (1,2,3)\n",djc.toString());
        djc.addDisjointnessClass(new int[]{4,0,1,2,4,5});
        assertEquals("Disjointenss Classes:\n" +
                "D4j: (1,2,3,4,5)\n",djc.toString());
    }

    @Test
    public void addDisjointness1Class() throws Exception {
        System.out.println("addDisjointnessClass with observers");
        Model model = new Model(20);
        ImplicationDAG ig = new ImplicationDAG();
        DisjointnessClasses djc = new DisjointnessClasses(model, ig, null);
        StringBuilder stt = new StringBuilder();
        StringBuilder std = new StringBuilder();
        StringBuilder stu = new StringBuilder();
        djc.addTrueLiteralObserver(lit -> stt.append("T " + lit + "  "));
        djc.addDisjointnessObserver(cl -> std.append("C " + cl.toString() + "  "));
        djc.addUnsatisfiabilityObserver(u -> stu.append(u.getClass().getName()));
        model.add(2);
        djc.addDisjointnessClass(new int[]{1, 0, 1, 2, 3});
        assertEquals("T -1  T -3  ",stt.toString());
        djc.addDisjointnessClass(new int[]{2, 0, 10, 12, 10});
        assertEquals("Datastructures.Results.Unsatisfiable",stu.toString());
        djc.addDisjointnessClass(new int[]{2, 0, 13, 14, -15});
        assertEquals("C D2: (13,14,-15)  ",std.toString());
    }

    @Test
    public void addDisjointness() throws Exception {
        System.out.println("addDisjointness");
        Model model = new Model(20);
        ImplicationDAG ig = new ImplicationDAG();
        DisjointnessClasses djc = new DisjointnessClasses(model,ig,null);
        ig.addClause(-1,-2);
        assertTrue(djc.isEmpty());
        ig.addClause(-2,-3);
        assertTrue(djc.isEmpty());
        ig.addClause(-1,-3);
        assertEquals("Disjointenss Classes:\nD1!=3: (1,3,2)\n",djc.toString());
        ig.addClause(-3,-4);
        ig.addClause(-1,-4);
        assertEquals("Disjointenss Classes:\nD1!=3: (1,3,2)\nD1!=4: (1,4,3)\n",djc.toString());
        ig.addClause(-2,-4);
        assertEquals("Disjointenss Classes:\nD2!=4: (2,4,3,1)\n",djc.toString());
    }

}