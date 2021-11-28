package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Results.Aborted;
import Datastructures.Symboltable;
import Utilities.Interval;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import java.util.function.IntSupplier;
import java.util.function.IntUnaryOperator;

import static org.junit.Assert.*;

public class ClauseTest {

    static Symboltable symboltable = new Symboltable(10);
    static{
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"s");
        symboltable.setName(5,"t");
        symboltable.setName(6,"u");
        symboltable.setName(7,"v");
        symboltable.setName(8,"w");
    }

    @Test
    public void constructor1() {
        System.out.println("Constructor 1");
        Clause clause = new Clause(1,Connective.INTERVAL,new Interval(1,2),
                IntArrayList.wrap(new int[]{2,-3,4}));
        assertEquals("I-1: [1,2]: 2,-3,4",clause.toNumbers());
        assertEquals("I-1:   [1,2]: q,-r,s",clause.toString(5,symboltable));
        assertEquals(Connective.INTERVAL,clause.connective);
        assertEquals(2,clause.getLiteral(0));
        String s = "";
        for(CLiteral cLiteral : clause) {s += cLiteral.literal;}
        assertEquals("2-34",s);
        assertEquals("Input: Clause 1",clause.inferenceStep.toString());
        assertEquals("[2, -3, 4]",clause.toArray().toString());

        assertSame(clause,clause.getCLiteral(0).clause);
        assertSame(clause,clause.getCLiteral(2).clause);
        assertEquals(1,clause.getCLiteral(1).clausePosition);
    }

    @Test
    public void setConnective()  {
        System.out.println("setConnective");
        Clause c1 = new Clause(1,Connective.AND,null,
                IntArrayList.wrap(new int[]{2,-3,4}));
        assertEquals(Connective.AND,c1.connective);

        Clause c2 = new Clause(2,Connective.OR,null,
                IntArrayList.wrap(new int[]{2,-3,4}));
        assertEquals(Connective.OR,c2.connective);

        Clause c3= new Clause(3,Connective.INTERVAL,new Interval(1,3),
                IntArrayList.wrap(new int[]{2,-3,4}));
        assertEquals(Connective.OR,c3.connective);

        Clause c4= new Clause(4,Connective.INTERVAL,new Interval(2,2),
                IntArrayList.wrap(new int[]{2,-3,4}));
        assertEquals(Connective.EXACTLY,c4.connective);

        Clause c5= new Clause(5,Connective.INTERVAL,new Interval(2,4),
                IntArrayList.wrap(new int[]{2,-3,4}));
        assertEquals(Connective.ATLEAST,c5.connective);
        assertEquals(3,c5.interval.max);

        Clause c6= new Clause(6,Connective.INTERVAL,new Interval(0,2),
                IntArrayList.wrap(new int[]{2,-3,4}));
        assertEquals(Connective.ATMOST,c6.connective);

        Clause c7= new Clause(7,Connective.INTERVAL,new Interval(3,3),
                IntArrayList.wrap(new int[]{2,-3,4}));
        assertEquals(Connective.AND,c7.connective);
        assertEquals("A-7: 2&-3&4",c7.toNumbers());

        Clause c8= new Clause(8,Connective.INTERVAL,new Interval(0,0),
                IntArrayList.wrap(new int[]{2,-3,4}));
        assertEquals(Connective.AND,c8.connective);
        assertEquals("A-8: -2&3&-4",c8.toNumbers());

        Clause c9= new Clause(9,Connective.ATMOST,new Interval(0,2),
                IntArrayList.wrap(new int[]{2,-3,4}));
        assertEquals(Connective.ATMOST,c9.connective);

        Clause c10= new Clause(10,Connective.ATLEAST,new Interval(2,3),
                IntArrayList.wrap(new int[]{2,-3,4}));
        assertEquals(Connective.ATLEAST,c10.connective);

        Clause c11= new Clause(11,Connective.EXACTLY,new Interval(2,2),
                IntArrayList.wrap(new int[]{2,-3,4}));
        assertEquals(Connective.EXACTLY,c11.connective);

        Clause c12= new Clause(12,Connective.OR,null,
                IntArrayList.wrap(new int[]{2}));
        assertEquals(Connective.AND,c12.connective);

        Clause c13= new Clause(13,Connective.ATLEAST,new Interval(1,1),
                IntArrayList.wrap(new int[]{2}));
        assertEquals(Connective.AND,c13.connective);
    }


    @Test
    public void constructor2() throws Aborted {
        System.out.println("Constructor Basic Clause");
        int[] bc1 = new int[]{1, Connective.OR.ordinal(), 2, 3, 4};
        Clause c1 = new Clause(bc1);
        assertEquals(Connective.OR,c1.connective);
        assertEquals("1: 2,3,4", c1.toNumbers());

        int[] bc2 = new int[]{2, Connective.AND.ordinal(), 2, 3, 4};
        Clause c2 = new Clause(bc2);
        assertEquals(Connective.AND,c2.connective);
        assertEquals("A-2: 2&3&4", c2.toNumbers());

        int[] bc3 = new int[]{3, Connective.ATLEAST.ordinal(), 2, 2, 3, 4};
        Clause c3 = new Clause(bc3);
        assertEquals(Connective.ATLEAST,c3.connective);
        assertEquals("L-3: [2,3]: 2,3,4", c3.toNumbers());

        int[] bc4 = new int[]{4, Connective.ATMOST.ordinal(), 2, 2, 3, 4};
        Clause c4 = new Clause(bc4);
        assertEquals(Connective.ATMOST,c4.connective);
        assertEquals("M-4: [0,2]: 2,3,4", c4.toNumbers());

        int[] bc5 = new int[]{5, Connective.EXACTLY.ordinal(), 2, 2, 3, 4};
        Clause c5 = new Clause(bc5);
        assertEquals(Connective.EXACTLY,c5.connective);
        assertEquals("X-5: 2: 2,3,4", c5.toNumbers());

        int[] bc6 = new int[]{6, Connective.EQUIV.ordinal(), 2, 3, 4};
        Clause c6 = new Clause(bc6);
        assertEquals(Connective.EQUIV,c6.connective);
        assertEquals("E-6: 2=3=4", c6.toNumbers());

        int[] bc7 = new int[]{7, Connective.INTERVAL.ordinal(), 2,3,2, 3, 4, 5};
        Clause c7 = new Clause(bc7);
        assertEquals(Connective.INTERVAL,c7.connective);
        assertEquals("I-7: [2,3]: 2,3,4,5", c7.toNumbers());

        int[] bc8 = new int[]{8, Connective.INTERVAL.ordinal(), 2,4,2, 3, 4, 5};
        Clause c8 = new Clause(bc8);
        assertEquals(Connective.ATLEAST,c8.connective);
        assertEquals("L-8: [2,4]: 2,3,4,5", c8.toNumbers());

        int[] bc9 = new int[]{9, Connective.INTERVAL.ordinal(), 0,2,2, 3, 4, 5};
        Clause c9 = new Clause(bc9);
        assertEquals(Connective.ATMOST,c9.connective);
        assertEquals("M-9: [0,2]: 2,3,4,5", c9.toNumbers());

        int[] bc10 = new int[]{10, Connective.INTERVAL.ordinal(), 1,4,2, 3, 4, 5};
        Clause c10 = new Clause(bc10);
        assertEquals(Connective.OR,c10.connective);
        assertEquals("10: 2,3,4,5", c10.toNumbers());

        int[] bc11 = new int[]{11, Connective.INTERVAL.ordinal(), 4,4,2, 3, 4, 5};
        Clause c11 = new Clause(bc11);
        assertEquals(Connective.AND,c11.connective);
        assertEquals("A-11: 2&3&4&5", c11.toNumbers());
        assertEquals(ClauseStructure.POSITIVE, c11.structure);

        int[] bc12 = new int[]{12, Connective.INTERVAL.ordinal(), 0,0,2, 3, 4, 5};
        Clause c12 = new Clause(bc12);
        assertEquals(Connective.AND,c12.connective);
        assertEquals("A-12: -2&-3&-4&-5", c12.toNumbers());
        assertEquals(ClauseStructure.NEGATIVE, c12.structure);


    }

    @Test
    public void constructor3()  {
        System.out.println("constructor 3");
        Clause c1 = new Clause(1,Connective.AND,2,-3,4);
        assertEquals(Connective.AND,c1.connective);

        Clause c2 = new Clause(2,Connective.OR,2,-3,4);
        assertEquals(Connective.OR,c2.connective);

        Clause c3= new Clause(3,Connective.INTERVAL,1,3,2,-3,4);
        assertEquals(Connective.OR,c3.connective);

        Clause c4= new Clause(4,Connective.INTERVAL,2,2,2,-3,4);
        assertEquals(Connective.EXACTLY,c4.connective);

        Clause c5= new Clause(5,Connective.INTERVAL,2,4,2,-3,4);
        assertEquals(Connective.ATLEAST,c5.connective);
        assertEquals(3,c5.interval.max);

        Clause c6= new Clause(6,Connective.INTERVAL,0,2,2,-3,4);
        assertEquals(Connective.ATMOST,c6.connective);

        Clause c7= new Clause(7,Connective.INTERVAL,3,3,2,-3,4);
        assertEquals(Connective.AND,c7.connective);
        assertEquals("A-7: 2&-3&4",c7.toNumbers());

        Clause c8= new Clause(8,Connective.INTERVAL,0,0,2,-3,4);
        assertEquals(Connective.AND,c8.connective);
        assertEquals("A-8: -2&3&-4",c8.toNumbers());

        Clause c9= new Clause(9,Connective.ATMOST,2,2,-3,4);
        assertEquals(Connective.ATMOST,c9.connective);

        Clause c10= new Clause(10,Connective.ATLEAST,2,2,-3,4);
        assertEquals(Connective.ATLEAST,c10.connective);

        Clause c11= new Clause(11,Connective.EXACTLY,2,2,-3,4);
        assertEquals(Connective.EXACTLY,c11.connective);
    }

    @Test
    public void cloneTest() {
        System.out.println("clone 1");
        Clause c1 = new Clause(1, Connective.INTERVAL, 2, 3, 2, -3, 4, 5);
        Clause c2 = c1.clone(1);
        assertEquals(c1.toNumbers(), c2.toNumbers());
        Clause c3 = new Clause(2, Connective.AND,  2, -3, 4, 5);
        Clause c4 = c3.clone(2);
        assertEquals(c3.toNumbers(), c4.toNumbers());
        Clause c5 = c3.clone(3);
        assertEquals("A-3: 2&-3&4&5",c5.toNumbers());
    }
    @Test
    public void replaceEquivalences() {
        System.out.println("replaceEquivalences");
        int[] id = new int[]{1};
        Clause c1 = new Clause(1, Connective.INTERVAL, 2, 3, 2, -3, 4, 5);
        IntArrayList replacements = new IntArrayList();
        Clause c2 = c1.replaceEquivalences((literal)-> literal > 0 ? literal+10 : literal, ()->++id[0],replacements);
        assertEquals("I-1: [2,3]: 2,-3,4,5",c1.toNumbers());
        assertEquals("I-2: [2,3]: 12,-3,14,15",c2.toNumbers());
        assertEquals("[2, 12, 4, 14, 5, 15]",replacements.toString());

        Clause c3 = new Clause(3, Connective.INTERVAL, 2, 3, -2, -3, -4, -5);
        Clause c4 = c3.replaceEquivalences((literal)-> literal > 0 ? literal+10 : literal, ()->++id[0],replacements);
        assertSame(c3,c4);
        Clause c5 = new Clause(5, Connective.INTERVAL, 2, 3, 2, -3, 4, 5);
        Clause c6 = c5.replaceEquivalences((literal)-> literal > 0 ? -literal : literal, ()->++id[0],replacements);
        assertEquals("I-3: [2,3]: -2,-3,-4,-5",c6.toNumbers());
        assertEquals("[2, -2, 4, -4, 5, -5]",replacements.toString());
        assertTrue(c6.isNegative());

        Clause c7 = c5.replaceEquivalences((literal)-> literal > 0 ? -literal : literal, null,replacements);
        assertSame(c7,c5);
        assertEquals("I-5: [2,3]: -2,-3,-4,-5",c7.toNumbers());
        assertEquals("[2, -2, 4, -4, 5, -5]",replacements.toString());
    }

    @Test
    public void removeTrueFalseLiterals() {
        System.out.println("removeTrueFalseLiterals");
        int[] id = new int[]{1};
        IntSupplier nextInt = () -> ++id[0];
        IntUnaryOperator getTruthStatus = (literal) -> literal > 4 ? (literal % 2 == 0 ? 1 : -1) : 0;
        IntArrayList trueLiterals = new IntArrayList();
        IntArrayList falseLiterals = new IntArrayList();

        Clause c1 = new Clause(1, Connective.INTERVAL, 2, 3, 1, 4, 5, 6, 2, 3);
        Clause c2 = c1.removeTrueFalseLiterals(getTruthStatus, nextInt, trueLiterals, falseLiterals);
        assertEquals("[6]",trueLiterals.toString());
        assertEquals("[5]",falseLiterals.toString());
        assertEquals("I-2: [1,2]: 1,4,2,3",c2.toNumbers());
        assertEquals(3,c2.getCLiteral(3).clausePosition);

        Clause c3 = new Clause(2, Connective.INTERVAL, 2, 3, 5,7,9);
        Clause c4 = c3.removeTrueFalseLiterals(getTruthStatus, null, trueLiterals, falseLiterals);
        assertSame(c3,c4);
        assertEquals("[]",trueLiterals.toString());
        assertEquals("[5, 7, 9]",falseLiterals.toString());
        assertEquals(ClauseStructure.CONTRADICTORY,c3.structure);

        Clause c5 = new Clause(3, Connective.OR, 3,4,5,6);
        Clause c6 = c5.removeTrueFalseLiterals(getTruthStatus, nextInt, trueLiterals, falseLiterals);
        assertNotSame(c5,c6);
        assertEquals("[6]",trueLiterals.toString());
        assertEquals("[5]",falseLiterals.toString());
        assertEquals(ClauseStructure.TAUTOLOGY,c6.structure);

        Clause c7 = new Clause(4, Connective.OR, 3,4,6,5);
        Clause c8 = c7.removeTrueFalseLiterals(getTruthStatus, nextInt, trueLiterals, falseLiterals);
        assertNotSame(c7,c8);
        assertEquals("[6]",trueLiterals.toString());
        assertEquals("[]",falseLiterals.toString());
        assertEquals(ClauseStructure.TAUTOLOGY,c8.structure);

        Clause c9 = new Clause(5, Connective.INTERVAL, 2, 3, 1, 2, 3, 4, 5, 6,7,8);
        Clause c10 = c9.removeTrueFalseLiterals(getTruthStatus, nextInt, trueLiterals, falseLiterals);
        assertEquals("[6, 8]",trueLiterals.toString());
        assertEquals("[5, 7]",falseLiterals.toString());
        assertEquals("M-5: [0,1]: 1,2,3,4",c10.toNumbers());

        Clause c11 = new Clause(5, Connective.EXACTLY, 2, 1, 2, 3, 4, 5, 6,7,8);
        Clause c12 = c11.removeTrueFalseLiterals(getTruthStatus, nextInt, trueLiterals, falseLiterals);
        assertEquals("[6, 8]",trueLiterals.toString());
        assertEquals("[5, 7]",falseLiterals.toString());
        assertEquals("A-6: -1&-2&-3&-4",c12.toNumbers());
    }


    @Test
    public void removeDoubles() {
        System.out.println("removeDoubleAndComplementaryLiterals");
        int[] id = new int[]{1};
        IntSupplier nextInt = () -> ++id[0];
        IntArrayList doubleLiterals = new IntArrayList();
        IntArrayList complementaryLiterals = new IntArrayList();
        Clause c1 = new Clause(1, Connective.OR, 3,4,6,3,4,3);
        Clause c2 = c1.removeDoubleAndComplementaryLiterals(nextInt,doubleLiterals,complementaryLiterals);
        assertNotSame(c1,c2);
        assertEquals("[3, 4]", doubleLiterals.toString());
        assertEquals("2: 3,4,6",c2.toNumbers());
        assertEquals(c2.structure,ClauseStructure.POSITIVE);

        Clause c3 = new Clause(1, Connective.OR, 3,3,3);
        Clause c4 = c3.removeDoubleAndComplementaryLiterals(nextInt,doubleLiterals,complementaryLiterals);
        assertNotSame(c3,c4);
        assertEquals("[3]", doubleLiterals.toString());
        assertEquals("A-3: 3",c4.toNumbers());

        Clause c5 = c3.removeDoubleAndComplementaryLiterals(null,doubleLiterals,complementaryLiterals);
        assertSame(c3,c5);
        assertEquals("A-1: 3",c3.toNumbers());

        Clause c6 = new Clause(1, Connective.OR, 3,3,-3);
        Clause c7 = c6.removeDoubleAndComplementaryLiterals(nextInt,doubleLiterals,complementaryLiterals);
        assertNotSame(c6,c7);
        assertEquals("[3]", doubleLiterals.toString());
        assertEquals("[3]", complementaryLiterals.toString());
        assertEquals(c7.structure,ClauseStructure.TAUTOLOGY);
        assertEquals("4: 3,-3",c7.toNumbers());

        Clause c8 = c6.removeDoubleAndComplementaryLiterals(null,doubleLiterals,complementaryLiterals);
        assertSame(c6,c8);
        assertEquals(c6.structure,ClauseStructure.TAUTOLOGY);

        Clause c9 = new Clause(1, Connective.INTERVAL, 2,3, 3,3,-3);
        Clause c10 = c9.removeDoubleAndComplementaryLiterals(nextInt,doubleLiterals,complementaryLiterals);
        assertNotSame(c9,c10);
        assertEquals("[]", doubleLiterals.toString());
        assertEquals("[3]", complementaryLiterals.toString());
        assertEquals(c10.structure,ClauseStructure.CONTRADICTORY);
        assertEquals("L-5: [2,1]: 3",c10.toNumbers());

        Clause c11 = new Clause(1, Connective.INTERVAL, 2,3, 3,4,5,-3,-4,6,7);
        Clause c12 = c11.removeDoubleAndComplementaryLiterals(nextInt,doubleLiterals,complementaryLiterals);
        assertNotSame(c11,c12);
        assertEquals("[]", doubleLiterals.toString());
        assertEquals("[3, 4]", complementaryLiterals.toString());
        assertEquals(c12.structure,ClauseStructure.POSITIVE);
        assertEquals("L-6: [2,3]: 5,6,7",c12.toNumbers());

    }
}