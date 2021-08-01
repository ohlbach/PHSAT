package Datastructures;

import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 25.08.2018.
 */
public class SymboltableTest {


    @Test
    public void toString1() throws Exception {
        System.out.println("st.toString");
        Symboltable st = new Datastructures.Symboltable(5);
        st.setName(1,"A");
        assertEquals("A",st.toString(1));
        assertEquals("-A",st.toString(-1));
        assertEquals("2",st.toString(2));
        assertEquals("-3",st.toString(-3));
        assertEquals("1:A,",st.toString());
    }

    @Test
    public void toString2() throws Exception {
        System.out.println("Symboltable.toString");
        Symboltable st = new Datastructures.Symboltable(5);
        st.setName(1,"A");
        st.setName(2,"B");
        assertEquals("1",Symboltable.toString(1,null));
        assertEquals("A",Symboltable.toString(1,st));
        assertEquals("-2",Symboltable.toString(-2,null));
        assertEquals("-B",Symboltable.toString(-2,st));
        assertEquals("-3",Symboltable.toString(-3,st));
        assertEquals("1:A,2:B,",st.toString());
    }

    @Test
    public void toString3() throws Exception {
        System.out.println("Symboltable.toString(IntArrayList)");
        Symboltable st = new Datastructures.Symboltable(5);
        st.setName(1,"A");
        st.setName(2,"B");
        IntArrayList list = new IntArrayList();
        list.add(1); list.add(-2); list.add(3);
        assertEquals("1,-2,3",Symboltable.toString(list,null));
        assertEquals("A,-B,3",Symboltable.toString(list,st));
        assertEquals("1 = -2 = 3",Symboltable.toString(list," = ",null));
        assertEquals("A = -B = 3",Symboltable.toString(list, " = ",st));}


    }