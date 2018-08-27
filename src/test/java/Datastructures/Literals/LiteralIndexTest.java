package Datastructures.Literals;

import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 27.08.2018.
 */
public class LiteralIndexTest {
    @Test
    public void addLiteral() throws Exception {
        System.out.println("addLiteral");
        LiteralIndex ind = new LiteralIndex(10);
        CLiteral l1 = new CLiteral(5);
        CLiteral l2 = new CLiteral(-5);
        CLiteral l3 = new CLiteral(6);
        CLiteral l4 = new CLiteral(-7);
        CLiteral l5 = new CLiteral(-7);
        ind.addLiteral(l1);
        ind.addLiteral(l2);
        ind.addLiteral(l3);
        ind.addLiteral(l4);
        ind.addLiteral(l5);
        assertEquals("[5]",ind.getLiterals(5).toString());
        assertEquals("[-5]",ind.getLiterals(-5).toString());
        assertEquals("[-7, -7]",ind.getLiterals(-7).toString());
        System.out.println(ind);
    }

    @Test
    public void removeLiteral() throws Exception {
        System.out.println("removeLiteral");
        LiteralIndex ind = new LiteralIndex(10);
        CLiteral l1 = new CLiteral(5);
        CLiteral l2 = new CLiteral(-5);
        CLiteral l3 = new CLiteral(6);
        CLiteral l4 = new CLiteral(-7);
        CLiteral l5 = new CLiteral(-7);
        ind.addLiteral(l1);
        ind.addLiteral(l2);
        ind.addLiteral(l3);
        ind.addLiteral(l4);
        ind.addLiteral(l5);
        assertEquals("[5]",ind.getLiterals(5).toString());
        ind.removeLiteral(l1);
        assertEquals("[]",ind.getLiterals(5).toString());
        ind.removeLiteral(l4);
        assertEquals("[-7]",ind.getLiterals(-7).toString());

    }

    @Test
    public void getLiterals() throws Exception {

    }

}