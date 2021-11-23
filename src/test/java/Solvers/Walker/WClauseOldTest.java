package Solvers.Walker;

import Datastructures.Clauses.ClauseOld;
import Datastructures.Clauses.Connective;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import static org.junit.Assert.*;

public class WClauseOldTest {

    private static final Connective or = Connective.OR;
    private static final Connective al = Connective.ATLEAST;
    private static final Connective am = Connective.ATMOST;
    private static final Connective ex = Connective.EXACTLY;

    private static WClause make(int id, Connective type, int quantifier, int... literals) {
        ClauseOld clause = new ClauseOld(id,type,quantifier, IntArrayList.wrap(literals));
        return new WClause(clause);}

    @Test
    public void multiplicity1() {
        System.out.println("multiplicity no doubles");
        WClause c1 = make(1,or,1,1,2,3);
        assertEquals("1: 1,2,3",c1.toString());
        assertEquals(1,c1.multiplicity(1));
        assertEquals(1,c1.multiplicity(3));
        assertEquals(1,c1.multiplicityByPosition(1));
    }

    @Test
    public void multiplicity2() {
        System.out.println("multiplicity with doubles");
        WClause c1 = make(1,or,1,1,-2,3,1,-2,1,-2,-2);
        assertEquals("1: 1,-2,3,1,-2,1,-2,-2",c1.toString());
        assertEquals(3,c1.multiplicity(1));
        assertEquals(1,c1.multiplicity(3));
        assertEquals(4,c1.multiplicity(-2));
        assertEquals(4,c1.multiplicityByPosition(4));
        assertEquals(4,c1.multiplicityByPosition(1));
    }

    @Test
    public void symboltable() {
        System.out.println("toString");
        Symboltable st = new Symboltable(10);
        st.setName(1,"p");
        st.setName(2,"q");
        st.setName(3,"r");
        WClause c1 = make(1, or, 1, 1,2,3);
        c1.isGloballyTrue = true;
        assertEquals("1:    p,q,r GT",c1.toString(6,st));

        WClause c2 = make(2, al, 2, 1,2,3);
        c2.isGloballyTrue = true; c2.isLocallyTrue = true;
        assertEquals("L-2: ATLEAST 2: p,q,r GT LT",c2.toString(3,st));

        WClause c3 = make(3, am, 2, 1,2,3);
        c3.isLocallyTrue = true;
        assertEquals("M-3: ATMOST 2: p,q,r LT",c3.toString(3,st));

        WClause c4 = make(4, ex, 2, 1,2,3);
        c4.isLocallyTrue = true;
        assertEquals("X-4: EXACTLY 2: p,q,r LT",c4.toString(3,st));

    }
}