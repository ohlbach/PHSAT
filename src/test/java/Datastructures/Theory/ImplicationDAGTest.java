package Datastructures.Theory;

import com.sun.istack.internal.Pool;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 26.09.2018.
 */
public class ImplicationDAGTest {

    @Test
    public void addImplication() throws Exception {
        ImplicationDAG id = new ImplicationDAG();
        id.addImplication(1,2);
        assertEquals("1 -> 2\n",id.toString());
        id.addImplication(1,3);
        assertEquals("1 -> 2,3\n",id.toString());
        id.addImplication(2,4);
        assertEquals("1 -> 2,3\n" +
                "     2 -> 4\n",id.toString());
        id.addImplication(3,4);
        assertEquals("1 -> 2,3\n" +
                "     2 -> 4\n" +
                "     3 -> 4\n",id.toString());
        id.addImplication(4,5);
        assertEquals("1 -> 2,3\n" +
                "     2 -> 4\n" +
                "          4 -> 5\n" +
                "     3 -> 4\n" +
                "          4 -> 5\n",id.toString());
        System.out.printf("I " + id.implies(1,5));
        id.addImplication(1,5);
        assertEquals("1 -> 2,3\n" +
                "     2 -> 4\n" +
                "          4 -> 5\n" +
                "     3 -> 4\n" +
                "          4 -> 5\n",id.toString());
        System.out.println(id.toString());

    }

    @Test
    public void implies() throws Exception {

    }

    @Test
    public void rootLiterals() throws Exception {

    }

    @Test
    public void addClause() throws Exception {

    }



    @Test
    public void newTrueLiteral() throws Exception {

    }

    @Test
    public void apply() throws Exception {

    }

    @Test
    public void apply1() throws Exception {

    }

}