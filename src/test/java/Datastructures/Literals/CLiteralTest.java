package Datastructures.Literals;

import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 25.08.2018.
 */
public class CLiteralTest {
    @Test
    public void getLiteral() throws Exception {
        CLiteral lit = new CLiteral(5);
        assertEquals(5, lit.getLiteral());

    }

}