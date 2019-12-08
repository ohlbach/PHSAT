package Datastructures.Literals;

import Datastructures.Clauses.Clause;
import Utilities.BucketSortedIndex;
import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 06.12.2019.
 */
public class LitAlgorithmsTest {


    private Clause make(int id,BucketSortedIndex<CLiteral<Clause>> literalIndex,int... literals) {
        Clause cl = new Clause(Integer.toString(id),literals.length);
        int i = -1;
        for(int l:literals) {
            CLiteral lit = new CLiteral(l,cl,++i);
            cl.add(lit);}
        for(CLiteral<Clause> lit : cl) {literalIndex.add(lit);}
        return cl;}

        private BucketSortedIndex<CLiteral<Clause>> makeIndex(int predicates) {
            return
                new BucketSortedIndex<CLiteral<Clause>>(predicates,
                    (cLiteral->cLiteral.literal),
                    (cLiteral-> cLiteral.clause.size()));}

    @Test
    public void isSubsumed() throws Exception {
        System.out.println("isSubsumed");
        BucketSortedIndex<CLiteral<Clause>> index = makeIndex(41);
        Clause c1 = make(1,index, 10,20,30);
        Clause c2 = make(2,index,30,20,10);
        Clause c3 = make(3,index,30,20,10,40);
        Clause c4 = make(4,index,30,20,-10,40);
        //System.out.println(index.toString());
        assertEquals(c2,LitAlgorithms.isSubsumed(c1,index,1));
        assertEquals(c1,LitAlgorithms.isSubsumed(c2,index,10));
        assertEquals(c1,LitAlgorithms.isSubsumed(c3,index,20));
        assertNull(LitAlgorithms.isSubsumed(c4,index,30));
    }

    @Test
    public void subsumes() throws Exception {
        System.out.println("subsumes");
        BucketSortedIndex<CLiteral<Clause>> index = makeIndex(41);
        Clause c1 = make(1,index, 10,20,30);
        Clause c2 = make(2,index,30,20,10);
        Clause c3 = make(3,index,30,20,10,40);
        Clause c4 = make(4,index,30,20,-10,40);
        Clause c5 = make(5,index,30,40);
        ArrayList<Clause> subsumed = new ArrayList<>();
        LitAlgorithms.subsumes(c1,index,1,subsumed);
        assertEquals(2,subsumed.size());
        assertEquals(c2,subsumed.get(0));
        assertEquals(c3,subsumed.get(1));
        subsumed.clear();
        LitAlgorithms.subsumes(c5,index,10,subsumed);
        assertEquals(2,subsumed.size());
        assertEquals(c3,subsumed.get(0));
        assertEquals(c4,subsumed.get(1));
    }

    @Test
    public void replacementResolutionBackwards() throws Exception {
        System.out.println("replacement resolution backwards");
        BucketSortedIndex<CLiteral<Clause>> index = makeIndex(41);
        Clause c1 = make(1,index, 10,20,30);
        Clause c2 = make(2,index,30,20,10);
        Clause c3 = make(3,index,30,20,10,40);
        Clause c4 = make(4,index,30,20,-10,40);
        Clause c5 = make(5,index,30,-40);
        Object[] result = LitAlgorithms.replacementResolutionBackwards(c4,index,1);
        assertEquals(2,result.length);
        assertEquals("-10",result[0].toString());
        assertEquals(c1,result[1]);
        result = LitAlgorithms.replacementResolutionBackwards(c3,index,10);
        assertEquals("40",result[0].toString());
        assertEquals(c5,result[1]);
        result = LitAlgorithms.replacementResolutionBackwards(c1,index,20);
        assertNull(result);
        //System.out.println(result[0]); System.out.println(result[1]);

    }

    @Test
    public void replacementResolutionForward() throws Exception {
        System.out.println("replacement resolution forward");
        BucketSortedIndex<CLiteral<Clause>> index = makeIndex(41);
        Clause c1 = make(1,index, 10,20,30);
        Clause c2 = make(2,index,30,20,11);
        Clause c3 = make(3,index,30,20,10,40);
        Clause c4 = make(4,index,30,20,-10,40);
        Clause c5 = make(5,index,30,-40);
        ArrayList<CLiteral<Clause>> result = new ArrayList<>();
        LitAlgorithms.replacementResolutionForward(c5,index, 1, result);
        assertEquals(2,result.size());
        assertEquals(c3.getCLiteral(3),result.get(0));
        assertEquals(c4.getCLiteral(3),result.get(1));
        result.clear();
        LitAlgorithms.replacementResolutionForward(c1,index, 10, result);
        assertEquals(1,result.size());
        assertEquals(c4.getCLiteral(2),result.get(0));
        result.clear();
        LitAlgorithms.replacementResolutionForward(c3,index, 20, result);
        assertEquals(1,result.size());
        assertEquals(c4.getCLiteral(2),result.get(0));
        result.clear();
        LitAlgorithms.replacementResolutionForward(c2,index, 30, result);
        assertTrue(result.isEmpty());
    }

    @Test
    public void contains() throws Exception {
        System.out.println("contains");
        BucketSortedIndex<CLiteral<Clause>> index = makeIndex(41);
        Clause c1 = make(1,index, 10,-20,30);
        assertEquals(1,LitAlgorithms.contains(c1.cliterals,10));
        assertEquals(-1,LitAlgorithms.contains(c1.cliterals,-10));
        assertEquals(1,LitAlgorithms.contains(c1.cliterals,-20));
        assertEquals(-1,LitAlgorithms.contains(c1.cliterals,20));
    }

    @Test
    public void resolve() throws Exception {
        System.out.println("resolve");
        BucketSortedIndex<CLiteral<Clause>> index = makeIndex(41);
        Clause c1 = make(1,index, 10,-20,30);
        Clause c2 = make(2,index, 10,20,30);
        Clause c3 = make(3,index, -10,20,30);
        Clause res = LitAlgorithms.resolve(c1.getCLiteral(1),c2.getCLiteral(2));
        assertEquals("1+2:(10,30,20)", res.toString());
        res = LitAlgorithms.resolve(c1.getCLiteral(1),c3.getCLiteral(2));
        assertNull(res);


    }

}