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