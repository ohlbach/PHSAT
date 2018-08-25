package Datastructures;

import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 25.08.2018.
 */
public class SymboltableTest {
    @Test
    public void getPredicateName() throws Exception {
            System.out.println("getPredicateName");
            Symboltable st = new Datastructures.Symboltable(5);
            st.setName(1,"A");
            assertEquals("A",st.getPredicateName(1));
            assertEquals(5,st.size);
    }

    @Test
    public void getLiteralName() throws Exception {
        System.out.println("getLiteralName");
        Symboltable st = new Datastructures.Symboltable(5);
        st.setName(1,"A");
        assertEquals("A",st.getLiteralName(1));
        assertEquals("-A",st.getLiteralName(-1));
    }


}