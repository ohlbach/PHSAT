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

    }

    @Test
    public void addDisjointness() throws Exception {
        System.out.println("addDisjointness");
        Model model = new Model(20);
        ImplicationGraph ig = new ImplicationGraph(20);
        DisjointnessClasses djc = new DisjointnessClasses(model,ig,null);
        ig.addClause(-1,-2);
        assertTrue(djc.isEmpty());
        ig.addClause(-2,-3);
        assertTrue(djc.isEmpty());
        ig.addClause(-1,-3);
        System.out.println(djc);
        assertEquals("Disjointenss Classes:\nD1!=3: (1,3,2)\n",djc.toString());
        ig.addClause(-3,-4);
        ig.addClause(-1,-4);
        System.out.println(djc);
        //assertEquals("Disjointenss Classes:\nD1!=3: (1,3,2)\n",djc.toString());
        ig.addClause(-2,-4);
        System.out.println(djc);
        //assertEquals("Disjointenss Classes:\nD1!=3: (1,3,2,4)\n",djc.toString());
    }

}