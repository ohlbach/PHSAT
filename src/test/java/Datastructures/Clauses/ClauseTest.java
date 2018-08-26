package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Model;
import com.sun.prism.shader.AlphaOne_RadialGradient_AlphaTest_Loader;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 26.08.2018.
 */
public class ClauseTest {
    @Test
    public void addLiteral() throws Exception {
        System.out.println("addLiteral, size");
        Clause cl = new Clause(1,3);
        assertEquals(0,cl.size());
        CLiteral lit = new CLiteral(5);
        assertEquals(0,cl.addLiteral(lit));
        assertEquals(1,cl.size());
        CLiteral lit1 = new CLiteral(5);
        assertEquals(1,cl.addLiteral(lit1));
        assertEquals(1,cl.size());
        CLiteral lit2 = new CLiteral(-5);
        assertEquals(-1,cl.addLiteral(lit2));
        assertEquals(1,cl.size());
        CLiteral lit3 = new CLiteral(-6);
        assertEquals(0,cl.addLiteral(lit3));
        assertEquals(2,cl.size());
        assertEquals(cl,lit3.getClause());
        assertEquals(1,lit3.getPosition());
    }

    @Test
    public void removeLiteral() throws Exception {
        System.out.println("removeLiteral");
        Clause cl = new Clause(1,3);
        CLiteral lit1 = new CLiteral(5);
        assertEquals(0,cl.addLiteral(lit1));
        CLiteral lit2 = new CLiteral(-6);
        assertEquals(0,cl.addLiteral(lit2));
        cl.removeLiteral(lit1);
        assertEquals(1,cl.size());
        assertNull(lit1.getClause());
        cl.removeLiteral(lit2);
        assertEquals(0,cl.size());
        assertNull(lit2.getClause());
    }

    @Test
    public void size() throws Exception {
        System.out.println("size, isEmpty with model");
        Clause cl = new Clause(1,3);
        Model model = new Model(10);
        CLiteral lit1 = new CLiteral(5);
        cl.addLiteral(lit1);
        CLiteral lit2 = new CLiteral(-3);
        cl.addLiteral(lit2);
        assertEquals(2,cl.size(model));
        model.push(-5);
        assertEquals(1,cl.size(model));
        assertFalse(cl.isEmpty(model));
        model.push(3);
        assertEquals(0,cl.size(model));
        assertTrue(cl.isEmpty(model));
    }

    @Test
    public void isTrue() throws Exception {
        System.out.println("isTrue in model");
        Clause cl = new Clause(1,3);
        Model model = new Model(10);
        CLiteral lit1 = new CLiteral(5);
        cl.addLiteral(lit1);
        CLiteral lit2 = new CLiteral(-3);
        cl.addLiteral(lit2);
        assertFalse(cl.isTrue(model));
        model.push(-3);
        assertTrue(cl.isTrue(model));
    }

    @Test
    public void apply() throws Exception {
        System.out.println("apply");
        Clause cl = new Clause(1,3);
        CLiteral lit1 = new CLiteral(5);
        cl.addLiteral(lit1);
        CLiteral lit2 = new CLiteral(-3);
        cl.addLiteral(lit2);
        StringBuffer st = new StringBuffer();
        cl.apply(literal->{st.append(Integer.toString(literal.literal));});
        assertEquals("5-3",st.toString());
    }

    @Test
    public void apply1() throws Exception {
        System.out.println("apply with model");
        Clause cl = new Clause(1,3);
        Model model = new Model(10);
        CLiteral lit1 = new CLiteral(5);
        cl.addLiteral(lit1);
        CLiteral lit2 = new CLiteral(-3);
        cl.addLiteral(lit2);
        model.push(-5);
        StringBuffer st = new StringBuffer();
        cl.apply(model,literal->{st.append(Integer.toString(literal.literal));});
        assertEquals("-3",st.toString());
    }

    @Test
    public void toStream() throws Exception {
        System.out.println("toStream");
        Clause cl = new Clause(1,2);
        CLiteral lit1 = new CLiteral(5);
        cl.addLiteral(lit1);
        CLiteral lit2 = new CLiteral(-3);
        cl.addLiteral(lit2);
        StringBuffer st = new StringBuffer();
        cl.toStream().peek(literal->{st.append(Integer.toString(literal.literal));}).count();
        assertEquals("5-3",st.toString());
    }

    @Test
    public void toStream1() throws Exception {
        System.out.println("toStream with model");
        Clause cl = new Clause(1,3);
        Model model = new Model(10);
        CLiteral lit1 = new CLiteral(5);
        cl.addLiteral(lit1);
        CLiteral lit2 = new CLiteral(-3);
        cl.addLiteral(lit2);
        model.push(-5);
        StringBuffer st = new StringBuffer();
        cl.toStream(model).peek(literal->{st.append(Integer.toString(literal.literal));}).count();
        assertEquals("-3",st.toString());

    }

    @Test
    public void toStringTest() throws Exception {
        System.out.println("toString with model");
        Clause cl = new Clause(1,3);
        Model model = new Model(10);
        CLiteral lit1 = new CLiteral(5);
        cl.addLiteral(lit1);
        CLiteral lit2 = new CLiteral(-3);
        cl.addLiteral(lit2);
        model.push(-5);
        assertEquals("1: -3,",cl.toString(model));
    }

}