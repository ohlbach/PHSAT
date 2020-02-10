package Datastructures.Theory;

import Datastructures.Clauses.ClauseType;
import Datastructures.Symboltable;
import Utilities.TriConsumer;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.function.BiConsumer;

import static org.junit.Assert.*;


/**
 * Created by ohlbach on 08.02.2020.
 */
public class DisjointnessClassesTest {
    Symboltable stb = null;
    static int type = ClauseType.DISJOINT.ordinal();

    private String toSt(ArrayList<IntArrayList> list) {
        StringBuilder stb = new StringBuilder();
        for(IntArrayList l : list) {stb.append(l.toString()).append(" ");}
        return stb.toString();}


    @Test
    public void removeDuplicates1() throws Exception {
        System.out.println("remove duplicates1");
        IntArrayList lits = IntArrayList.wrap(new int[]{1,2,3});
        ArrayList<IntArrayList> origs = new ArrayList<>();
        origs.add(IntArrayList.wrap(new int[]{11,12}));
        origs.add(IntArrayList.wrap(new int[]{21,22}));
        origs.add(IntArrayList.wrap(new int[]{31,32}));
        DisjointnessClasses.removeDuplicates(0,lits,origs);
        assertEquals("[1, 2, 3]",lits.toString());
        assertEquals("[11, 12] [21, 22] [31, 32] ",toSt(origs));
        DisjointnessClasses.removeDuplicates(2,lits,origs);
        assertEquals("[1, 3]",lits.toString());
        assertEquals("[11, 12] [31, 32] ",toSt(origs));
    }

    @Test
    public void removeDuplicates2() throws Exception {
        System.out.println("remove duplicates 2");
        IntArrayList lits = IntArrayList.wrap(new int[]{1, 2, 3, 2, 3, 3, 2, 1});
        ArrayList<IntArrayList> origs = new ArrayList<>();
        origs.add(IntArrayList.wrap(new int[]{11}));
        origs.add(IntArrayList.wrap(new int[]{22}));
        origs.add(IntArrayList.wrap(new int[]{33,34}));
        origs.add(IntArrayList.wrap(new int[]{24}));
        origs.add(IntArrayList.wrap(new int[]{35}));
        origs.add(IntArrayList.wrap(new int[]{36}));
        origs.add(IntArrayList.wrap(new int[]{27}));
        origs.add(IntArrayList.wrap(new int[]{18}));
        DisjointnessClasses.removeDuplicates(0, lits, origs);
        assertEquals("[1, 2, 3]", lits.toString());
        assertEquals("[11] [22] [35] ", toSt(origs));
    }

    @Test
    public void removeDuplicates3() throws Exception {
        System.out.println("remove duplicates 3");
        IntArrayList lits = IntArrayList.wrap(new int[]{1, 2, 3, 2, 3, 3, 2, 1});
        ArrayList<IntArrayList> origs = new ArrayList<>();
        origs.add(IntArrayList.wrap(new int[]{11}));
        origs.add(IntArrayList.wrap(new int[]{22}));
        origs.add(null);
        origs.add(IntArrayList.wrap(new int[]{24}));
        origs.add(IntArrayList.wrap(new int[]{35}));
        origs.add(IntArrayList.wrap(new int[]{36}));
        origs.add(IntArrayList.wrap(new int[]{27}));
        origs.add(null);
        DisjointnessClasses.removeDuplicates(0, lits, origs);
        assertEquals("[1, 2, 3]", lits.toString());
        assertEquals("[11] [22] [35] ", toSt(origs));
    }

    @Test
    public void analyseClause1() throws Exception {
        System.out.println("analyse clause standard");
        String[] s = new String[1];
        BiConsumer<Integer,IntArrayList> ucH = (lit,origin) -> s[0] = "U "+ lit + " " + origin.toString();
        TriConsumer<Integer,Integer,IntArrayList> bcH = (lit1,lit2,origin) -> s[0] = "B "+ lit1 + " " + lit2 + " " + origin.toString();
        DisjointnessClasses dc = new DisjointnessClasses(stb,ucH,bcH);
        Object[] result = dc.analyseClause(new int[]{10,type,1,2,3},null);
        IntArrayList preds = (IntArrayList)result[0];
        ArrayList<IntArrayList> origs = (ArrayList<IntArrayList>)result[1];
        assertEquals("[1, 2, 3]",preds.toString());
        assertEquals("[10] [10] [10] ",toSt(origs));}

    @Test
    public void analyseClause2() throws Exception {
        System.out.println("analyse clause negations");
        String[] s = new String[1]; s[0] = "";
        BiConsumer<Integer,IntArrayList> ucH = (lit,origin) -> s[0] += "U "+ lit + " " + origin.toString() + " ";
        TriConsumer<Integer,Integer,IntArrayList> bcH = (lit1,lit2,origin) -> s[0] += "B "+ lit1 + " " + lit2 + " " + origin.toString() + " ";
        DisjointnessClasses dc = new DisjointnessClasses(stb,ucH,bcH);
        Object[] result = dc.analyseClause(new int[]{10,type,1,-2,3,-4},null);
        IntArrayList preds = (IntArrayList)result[0];
        ArrayList<IntArrayList> origs = (ArrayList<IntArrayList>)result[1];
        assertEquals("[1, 3]",preds.toString());
        assertEquals("[10] [10] ",toSt(origs));
        assertEquals("B 2 -1 [10] B 2 -3 [10] B 2 4 [10] B 4 -1 [10] B 4 2 [10] B 4 -3 [10] ",s[0]);
    }

    @Test
    public void analyseClause3() throws Exception {
        System.out.println("analyse clause units");
        String[] s = new String[1]; s[0] = "";
        BiConsumer<Integer,IntArrayList> ucH = (lit,origin) -> s[0] += "U "+ lit + " " + origin.toString() + " ";
        TriConsumer<Integer,Integer,IntArrayList> bcH = (lit1,lit2,origin) -> s[0] += "B "+ lit1 + " " + lit2 + " " + origin.toString() + " ";
        DisjointnessClasses dc = new DisjointnessClasses(stb,ucH,bcH);
        Object[] result = dc.analyseClause(new int[]{10,type,1,2,3,2,3},null);
        assertNull(result);
        assertEquals("U -2 [10] U -3 [10] ",s[0]);
    }

    @Test
    public void analyseClause4() throws Exception {
        System.out.println("analyse clause equivalences");
        String[] u = new String[]{""};
        String[] b = new String[]{""};
        BiConsumer<Integer,IntArrayList> ucH = (lit,origin) -> u[0] += "U "+ lit + " " + origin.toString() + " ";
        TriConsumer<Integer,Integer,IntArrayList> bcH = (lit1,lit2,origin) -> b[0] += "B "+ lit1 + " " + lit2 + " " + origin.toString() + " ";
        DisjointnessClasses dc = new DisjointnessClasses(stb,ucH,bcH);
        EquivalenceClasses eq = new EquivalenceClasses(stb,null);
        eq.addEquivalenceClass(1,3,IntArrayList.wrap(new int[]{11,12}));
        Object[] result = dc.analyseClause(new int[]{10,type,1,-2,3,4,5},eq);
        IntArrayList preds = (IntArrayList)result[0];
        ArrayList<IntArrayList> origs = (ArrayList<IntArrayList>)result[1];
        assertEquals("[4, 5]",preds.toString());
        assertEquals("[10] [10] ",toSt(origs));
        assertEquals("U -1 [10, 11, 12] ",u[0]);
        assertEquals("B 2 -4 [10] B 2 -5 [10] ",b[0]);
    }



    @Test
    public void addDisjointnessClass() throws Exception {
        System.out.println("add Disjointness, areDisjoint, truth, getOrigins");
        String[] u = new String[]{""};
        String[] b = new String[]{""};
        BiConsumer<Integer,IntArrayList> ucH = (lit,origin) -> u[0] += "U "+ lit + " " + origin.toString() + " ";
        TriConsumer<Integer,Integer,IntArrayList> bcH = (lit1,lit2,origin) -> b[0] += "B "+ lit1 + " " + lit2 + " " + origin.toString() + " ";
        stb = new Symboltable(10);
        stb.setName(1,"p");
        EquivalenceClasses eq = new EquivalenceClasses(stb,null);
        eq.addEquivalenceClass(5,6,IntArrayList.wrap(new int[]{12,13}));
        DisjointnessClasses dc = new DisjointnessClasses(stb,ucH,bcH);
        int[] ca = new int[]{10,type,1,2,3};
        dc.addDisjointnessClass(ca,null);
        int[] cb = new int[]{11,type,3,4,6};
        dc.addDisjointnessClass(cb,eq);
        //System.out.println(dc.toString());
        assertTrue(dc.areDisjoint(2,3));
        assertTrue(dc.areDisjoint(1,3));
        assertFalse(dc.areDisjoint(1,4));
        assertFalse(dc.areDisjoint(1,5));
        assertTrue(dc.areDisjoint(5,3));

        IntArrayList truth = new IntArrayList(5);
        assertTrue(dc.truths(1,truth));
        assertEquals("[-2, -3]",truth.toString());
        truth.clear();
        assertTrue(dc.truths(3,truth));
        assertEquals("[-1, -2, -4, -5]",truth.toString());

        assertEquals("[11, 12, 13]",dc.getOrigins(3,5).toString());



    }

    @Test
    public void addDisjointnessClass1() throws Exception {
        System.out.println("addDisjointnessClass l1 l2");
        String[] u = new String[]{""};
        String[] b = new String[]{""};
        BiConsumer<Integer,IntArrayList> ucH = (lit,origin) -> u[0] += "U "+ lit + " " + origin.toString() + " ";
        TriConsumer<Integer,Integer,IntArrayList> bcH = (lit1,lit2,origin) -> b[0] += "B "+ lit1 + " " + lit2 + " " + origin.toString() + " ";
        stb = new Symboltable(10);
        EquivalenceClasses eq = new EquivalenceClasses(stb,null);
        eq.addEquivalenceClass(5,6,IntArrayList.wrap(new int[]{12,13}));
        DisjointnessClasses dc = new DisjointnessClasses(stb,ucH,bcH);
        int[] ca = new int[]{10,type,1,2,3};
        dc.addDisjointnessClass(ca,null);
        dc.addDisjointnessClass(3,4,IntArrayList.wrap(new int[]{20,21}));
        //System.out.println(dc.toString());
        IntArrayList truth = new IntArrayList(5);
        assertTrue(dc.truths(1,truth));
        assertEquals("[-2, -3]",truth.toString());
        truth.clear();
        assertTrue(dc.truths(3,truth));
        assertEquals("[-1, -2, -4]",truth.toString());

        assertEquals("[20, 21]",dc.getOrigins(3,4).toString());

    }


}