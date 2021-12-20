package Datastructures.Literals;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.Connective;
import Datastructures.Results.Unsatisfiable;
import Utilities.BucketSortedIndex;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 06.12.2019.
 */
public class LitAlgorithmsTest {


    private Clause make(int id, BucketSortedIndex<CLiteral> literalIndex, int... literals) throws Unsatisfiable {
        Clause cl = new Clause(id, Connective.OR, literals.length);
        int i = -1;
        for(int l:literals) {
            CLiteral lit = new CLiteral(l,cl,++i,(short)1);
            cl.add(lit);}
        for(CLiteral lit : cl) {literalIndex.add(lit);}
        cl.setStructure();
        return cl;}

        private BucketSortedIndex<CLiteral> makeIndex(int predicates) {
            return
                new BucketSortedIndex<CLiteral>(predicates,
                    (cLiteral->cLiteral.literal),
                    (cLiteral-> cLiteral.clause.size()));}

    @Test
    public void isSubsumed() throws Exception {
        System.out.println("isSubsumed");
        BucketSortedIndex<CLiteral> index = makeIndex(41);
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
        BucketSortedIndex<CLiteral> index = makeIndex(41);
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
        BucketSortedIndex<CLiteral> index = makeIndex(41);
        Clause c1 = make(1,index, 10,20,30);
        Clause c2 = make(2,index,30,20,10);
        Clause c3 = make(3,index,30,20,10,40);
        Clause c4 = make(4,index,30,20,-10,40);
        Clause c5 = make(5,index,30,-40);
        //System.out.println(index.toString(l->""+l.literal+"@"+l.clause.id));
        Object[] result = LitAlgorithms.replacementResolutionBackwards(c4,index,1);
        assertEquals(2,result.length);
        assertEquals("-10",result[0].toString());
        assertEquals(c1,result[1]);
        result = LitAlgorithms.replacementResolutionBackwards(c3,index,10);
        assertEquals("10",result[0].toString());
        assertEquals(c4,result[1]);
        result = LitAlgorithms.replacementResolutionBackwards(c1,index,20);
        assertNull(result);
        //System.out.println(result[0]); System.out.println(result[1]);

    }

    @Test
    public void replacementResolutionForward() throws Exception {
        System.out.println("replacement resolution forward");
        BucketSortedIndex<CLiteral> index = makeIndex(41);
        Clause c1 = make(1,index, 10,20,30);
        Clause c2 = make(2,index,30,20,11);
        Clause c3 = make(3,index,30,20,10,40);
        Clause c4 = make(4,index,30,20,-10,40);
        Clause c5 = make(5,index,30,-40);
        ArrayList<CLiteral> result = new ArrayList<>();
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
        BucketSortedIndex<CLiteral> index = makeIndex(41);
        Clause c1 = make(1,index, 10,-20,30);
        assertEquals(1,LitAlgorithms.contains(c1.cliterals,10));
        assertEquals(-1,LitAlgorithms.contains(c1.cliterals,-10));
        assertEquals(1,LitAlgorithms.contains(c1.cliterals,-20));
        assertEquals(-1,LitAlgorithms.contains(c1.cliterals,20));
    }

    @Test
    public void resolve() throws Exception {
        System.out.println("resolve");
        BucketSortedIndex<CLiteral> index = makeIndex(41);
        Clause c1 = make(1,index, 10,-20,30);
        Clause c2 = make(2,index, 10,20,30);
        Clause c3 = make(3,index, -10,20,30);
        int[] id = new int[]{3};
        Clause res = LitAlgorithms.resolve(id,c1.getCLiteral(1),c2.getCLiteral(1));
        assertEquals("4: 10,30", res.toString());
        res = LitAlgorithms.resolve(id,c1.getCLiteral(1),c3.getCLiteral(2));
        assertNull(res);


    }

    @Test
    public void canBRemoved1() throws Exception {
        System.out.println("canBRemoved1");
        BucketSortedIndex<CLiteral> index = makeIndex(41);
        Clause c1 = make(1, index, 10, 20, 30);
        Clause c2 = make(2, index, 30, -10, 20);
        ArrayList<CLiteral> result = new ArrayList<>();
        CLiteral clit = LitAlgorithms.canBRemoved(c1,index,1,1,null);
        assertNotNull(clit);
        assertEquals(10,clit.literal);
        clit = LitAlgorithms.canBRemoved(c2,index,3,1,null);
        assertNotNull(clit);
        assertEquals(-10,clit.literal);
    }

    @Test
    public void canBRemoved2() throws Exception {
        System.out.println("canBRemoved2");
        BucketSortedIndex<CLiteral> index = makeIndex(41);
        Clause c1 = make(1, index, 10, 20, 30);
        Clause c2 = make(2, index, -10,20, 31);
        ArrayList<CLiteral> result = new ArrayList<>();
        CLiteral clit = LitAlgorithms.canBRemoved(c1,index,1,1,null);
        assertNull(clit);
    }
    @Test
    public void canBRemoved3() throws Exception {
        System.out.println("canBRemoved3");
        BucketSortedIndex<CLiteral> index = makeIndex(41);
        Clause c1 = make(1, index, 10, 20, 30);
        Clause c2 = make(2, index, -10,20, 31);
        Clause c3 = make(3, index, -31,20, 30);
        ArrayList<CLiteral> result = new ArrayList<>();
        CLiteral clit = LitAlgorithms.canBRemoved(c1,index,1,3,null);
        assertNotNull(clit);
        assertEquals(10,clit.literal);
    }

    @Test
    public void canBRemoved4() throws Exception {
        System.out.println("canBRemoved4");
        BucketSortedIndex<CLiteral> index = makeIndex(41);
        Clause c1 = make(1, index, 10, 20, 30);
        Clause c2 = make(2, index, -10,20, 31);
        Clause c3 = make(3, index, -31,20, 33);
        ArrayList<CLiteral> result = new ArrayList<>();
        CLiteral clit = LitAlgorithms.canBRemoved(c1,index,1,3,null);
        assertNull(clit);
    }


    @Test
    public void canBRemoved5() throws Exception {
        System.out.println("canBRemoved5");
        BucketSortedIndex<CLiteral> index = makeIndex(41);
        Clause c1 = make(1, index, 10, 20, 30);
        Clause c2 = make(2, index, -10,20, 31);
        Clause c3 = make(3, index, -31,20, 10);
        ArrayList<CLiteral> result = new ArrayList<>();
        CLiteral clit = LitAlgorithms.canBRemoved(c1,index,1,3,null);
        assertNull(clit);
    }

    @Test
    public void canBRemoved6() throws Exception {
        System.out.println("canBRemoved6");
        BucketSortedIndex<CLiteral> index = makeIndex(41);
        Clause c1 = make(1, index, 1, 2, 3);
        Clause c2 = make(2, index, -1,4, 5);
        Clause c3 = make(3, index, -4,5, 3);
        Clause c4 = make(4, index, -5,4, 2);

        ArrayList<CLiteral> result = new ArrayList<>();
        CLiteral clit = LitAlgorithms.canBRemoved(c1,index,1,3,null);
        assertNull(clit);
    }

    @Test
    public void canBRemoved7() throws Exception {
        System.out.println("canBRemoved7");
        BucketSortedIndex<CLiteral> index = makeIndex(41);
        Clause c1 = make(1, index, 3, 2, 1);
        Clause c2 = make(2, index, -1,4, 5);
        Clause c3 = make(3, index, 3,5, -4);
        Clause c4 = make(4, index, -5,3, 2);

        ArrayList<CLiteral> result = new ArrayList<>();
        CLiteral clit = LitAlgorithms.canBRemoved(c1,index,1,3,null);
        assertNotNull(clit);
        assertEquals(1,clit.literal);
    }

    @Test
    public void canBRemoved8() throws Exception {
        System.out.println("canBRemoved8");
        BucketSortedIndex<CLiteral> index = makeIndex(41);
        Clause c1 = make(1, index, -1,-10,16);
        Clause c2 = make(54, index, 19,-8,-10);
        Clause c3 = make(85, index, 5,16,-9);
        Clause c4 = make(5, index, -6,18,3);
        Clause c5 = make(14, index, 9,17, -18);
        Clause c6 = make(24, index, -19,-17, 1);
        ArrayList<Clause> stack = new ArrayList<>();
        ArrayList<CLiteral> result = new ArrayList<>();
        CLiteral clit = LitAlgorithms.canBRemoved(c1,index,1,3,stack);
        assertNull(clit);
    }


    @Test
    public void urResolution1() throws Exception {
        System.out.println("urResolution1");
        BucketSortedIndex<CLiteral> index = makeIndex(41);
        Clause c1 = make(1, index, 1,2,3);
        Clause c2 = make(2, index, -1,4);
        Clause c3 = make(3, index, -4,-1);
        ArrayList<Clause> usedClauses = new ArrayList<>();
        Object result = LitAlgorithms.urResolution(c1,index,1,3,usedClauses);
        assertEquals("-1",(result.toString()));
        assertEquals(result.getClass(),Integer.class);
        assertEquals("[2:(-1,4), 3:(-4,-1)]",usedClauses.toString());
        result = LitAlgorithms.urResolution(c1,index,1,3,null);
        assertEquals("-1",(result.toString()));
    }

    @Test
    public void urResolution2() throws Exception {
        System.out.println("urResolution2");
        BucketSortedIndex<CLiteral> index = makeIndex(41);
        Clause c1 = make(1, index, 10,-1,11);
        Clause c2 = make(2, index, 1,2);
        Clause c3 = make(3, index, -2,3);
        Clause c4 = make(4, index, -3,4,1);
        Clause c5 = make(5, index, -4,1);

        ArrayList<Clause> usedClauses = new ArrayList<>();
        Object result = LitAlgorithms.urResolution(c1,index,1,3,usedClauses);
        assertEquals("1",(result.toString()));
        assertEquals("[2:(1,2), 3:(-2,3), 5:(-4,1), 4:(-3,4,1)]",usedClauses.toString());
    }

    @Test
    public void urResolution3() throws Exception {
        System.out.println("urResolution3");
        BucketSortedIndex<CLiteral> index = makeIndex(41);
        Clause c1 = make(1, index, 1,2);
        Clause c2 = make(2, index, -2,3);
        Clause c3 = make(3, index, -3,4,1);
        Clause c4 = make(4, index, -4,1,10);
        Clause c5 = make(5, index, -1,10,11);
        Clause c6 = make(6, index, 10,-1,11);
        Clause c7 = make(7, index, 10,11,-1);
        Clause c8 = make(8, index, 11,10,-1);
        ArrayList<Clause> usedClauses = new ArrayList<>();
        Object result = LitAlgorithms.urResolution(c5,index,1,3,usedClauses);
        assertEquals(result.getClass(),int[].class);
        assertEquals("[1, 10]", Arrays.toString((int[])result));
        assertEquals("[1:(1,2), 2:(-2,3), 3:(-3,4,1), 4:(-4,1,10)]",usedClauses.toString());

        result = LitAlgorithms.urResolution(c6,index,100,3,usedClauses);
        assertEquals("[10, 1]", Arrays.toString((int[])result));
        assertEquals("[1:(1,2), 2:(-2,3), 3:(-3,4,1), 4:(-4,1,10)]",usedClauses.toString());

        result = LitAlgorithms.urResolution(c7,index,200,3,usedClauses);
        assertEquals("[10, 1]", Arrays.toString((int[])result));
        assertEquals("[1:(1,2), 2:(-2,3), 3:(-3,4,1), 4:(-4,1,10)]",usedClauses.toString());


        result = LitAlgorithms.urResolution(c8,index,300,3,usedClauses);
        assertEquals("-1", result.toString());
        assertEquals("[1:(1,2), 2:(-2,3), 3:(-3,4,1), 4:(-4,1,10)]",usedClauses.toString());
    }

    @Test
    public void urResolution4() throws Exception {
        System.out.println("urResolution4");
        BucketSortedIndex<CLiteral> index = makeIndex(41);
        Clause c1 = make(1, index, 1,2);
        Clause c2 = make(2, index, 11,-2,3);
        Clause c3 = make(3, index, -3,4,1);
        Clause c4 = make(4, index, -4,1,10);
        Clause c5 = make(5, index, -1,10,11);
        Clause c6 = make(6, index, 10,-1,11);
        Clause c7 = make(7, index, 10,11,-1);
        Clause c8 = make(8, index, 11,10,-1);
        ArrayList<Clause> usedClauses = new ArrayList<>();
        Object result = LitAlgorithms.urResolution(c5,index,1,3,usedClauses);
        assertEquals("-1", result.toString());
        assertEquals("[1:(1,2), 4:(-4,1,10), 3:(-3,4,1), 2:(11,-2,3)]",usedClauses.toString());

        result = LitAlgorithms.urResolution(c6,index,100,3,usedClauses);
        assertEquals("-1", result.toString());
        assertEquals("[1:(1,2), 4:(-4,1,10), 3:(-3,4,1), 2:(11,-2,3)]",usedClauses.toString());


        result = LitAlgorithms.urResolution(c7,index,200,3,usedClauses);
        assertEquals("-1", result.toString());
        assertEquals("[1:(1,2), 4:(-4,1,10), 3:(-3,4,1), 2:(11,-2,3)]",usedClauses.toString());

        result = LitAlgorithms.urResolution(c8,index,300,3,usedClauses);
        assertEquals("-1", result.toString());
        assertEquals("[1:(1,2), 2:(11,-2,3), 3:(-3,4,1), 4:(-4,1,10)]",usedClauses.toString());



    }

    @Test
    public void urResolution5() throws Exception {
        System.out.println("urResolution5");
        BucketSortedIndex<CLiteral> index = makeIndex(41);
        Clause c1 = make(1, index, -1, -10,16);
        Clause c2 = make(28, index, -5, -10, 1);
        Clause c3 = make(30, index, 15,16,-10);
        Clause c4 = make(35, index, -15,1,13);
        Clause c5 = make(29, index, -13,14,1);
        Clause c6 = make(3, index, -14,-16,5);
        Clause c7 = make(7, index, -16,-19,-15);
        Clause c8 = make(23, index, 16,7,14);
        ArrayList<Clause> usedClauses = new ArrayList<>();
        Object result = LitAlgorithms.urResolution(c1,index,1,3,usedClauses);
        System.out.println(result);
    }




}