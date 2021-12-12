package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Results.Aborted;
import Datastructures.Symboltable;
import Utilities.Interval;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
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
    int or = Connective.OR.ordinal();
    int atl = Connective.ATLEAST.ordinal();
    int atm = Connective.ATMOST.ordinal();
    int eqv = Connective.EQUIV.ordinal();
    int and = Connective.AND.ordinal();

    @Test
    public void constructor1() {
        System.out.println("Constructor 1");
        Clause clause = new Clause(1,Connective.OR,1,
                IntArrayList.wrap(new int[]{2,-3,4}));
        assertEquals("1: 2,-3,4",clause.toNumbers());
        assertEquals("1:   q,-r,s",clause.toString(5,symboltable));
        assertEquals(Connective.OR,clause.connective);
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
    public void constructor2() {
        System.out.println("Constructor 2");
        Clause clause = new Clause(1,Connective.ATLEAST,1,
                IntArrayList.wrap(new int[]{2,3,4}));
        assertEquals("1: 2,3,4",clause.toNumbers());
        assertEquals("1: q,r,s",clause.toString(0,symboltable));
        assertEquals(Connective.OR,clause.connective);
        assertEquals(clause.structure,ClauseStructure.POSITIVE);
    }

    @Test
    public void constructor3() {
        System.out.println("Constructor 3");
        Clause clause = new Clause(1,Connective.ATLEAST,3,
                IntArrayList.wrap(new int[]{-2,-3,-4}));
        assertEquals("A-1: -2&-3&-4",clause.toNumbers());
        assertEquals("A-1: -q&-r&-s",clause.toString(0,symboltable));
        assertEquals(Connective.AND,clause.connective);
        assertEquals(clause.structure,ClauseStructure.NEGATIVE);
    }

    @Test
    public void constructor4() {
        System.out.println("Constructor 4");
        Clause clause = new Clause(1,Connective.ATLEAST,4,
                IntArrayList.wrap(new int[]{-2,-3,-4}));
        assertEquals(clause.structure,ClauseStructure.CONTRADICTORY);
    }
    @Test
    public void constructor5() {
        System.out.println("Constructor Atmost");
        Clause clause = new Clause(1,Connective.ATMOST,1,
                IntArrayList.wrap(new int[]{2,3,4}));
        assertEquals("L-1: 2: -2,-3,-4",clause.toNumbers());
        assertEquals("L-1: 2: -q,-r,-s",clause.toString(0,symboltable));
        assertEquals(Connective.ATLEAST,clause.connective);
        assertEquals(clause.structure,ClauseStructure.NEGATIVE);
    }

    @Test
    public void constructor6() {
        System.out.println("Constructor Atmost 0");
        Clause clause = new Clause(1,Connective.ATMOST,0,
                IntArrayList.wrap(new int[]{2,3,4}));
        assertEquals("A-1: -2&-3&-4",clause.toNumbers());
        assertEquals(Connective.AND,clause.connective);
        assertEquals(clause.structure,ClauseStructure.NEGATIVE);
    }

    @Test
    public void constructor7() {
        System.out.println("Constructor Atmost all");
        Clause clause = new Clause(1,Connective.ATMOST,3,
                IntArrayList.wrap(new int[]{2,3,4}));
        assertEquals(clause.structure,ClauseStructure.TAUTOLOGY);
    }

    @Test
    public void constructorBCOr() {
        System.out.println("Constructor BC or");
        int[] bc = new int[]{1, or, 2, 3, 4};
        Clause cl = new Clause(bc);
        assertEquals("1: 2,3,4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.POSITIVE);
        assertEquals(1,cl.limit);
    }

    @Test
    public void constructorBCAtleast() {
        System.out.println("Constructor BC atleast");
        int[] bc = new int[]{1, atl, 2, 2, 3, 4};
        Clause cl = new Clause(bc);
        assertEquals("L-1: 2: 2,3,4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.POSITIVE);
        assertEquals(2,cl.limit);

        bc = new int[]{2, atl, 3, 2, 3, 4};
        cl = new Clause(bc);
        assertEquals("A-2: 2&3&4", cl.toNumbers());
        assertEquals(-1,cl.limit);

        bc = new int[]{3, atl, 0, 2, 3, 4};
        cl = new Clause(bc);
        assertEquals(cl.structure,ClauseStructure.TAUTOLOGY);

        bc = new int[]{4, atl, 1, -2, -3, -4};
        cl = new Clause(bc);
        assertEquals("4: -2,-3,-4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.NEGATIVE);

    }

    @Test
    public void constructorBCAtmost() {
        System.out.println("Constructor BC atmost");
        int[] bc = new int[]{1, atm, 2, 2, 3, 4};
        Clause cl = new Clause(bc);
        assertEquals("1: -2,-3,-4", cl.toNumbers());
        assertEquals(cl.connective, Connective.OR);
        assertEquals(cl.structure,ClauseStructure.NEGATIVE);

        bc = new int[]{2, atm, 0, 2, 3, 4};
        cl = new Clause(bc);
        assertEquals("A-2: -2&-3&-4", cl.toNumbers());
        bc = new int[]{2, atm, 3, 2, 3, 4};
        cl = new Clause(bc);
        assertEquals(cl.structure,ClauseStructure.TAUTOLOGY);
    }
    @Test
    public void constructorBCAnd() {
        System.out.println("Constructor BC and");
        int[] bc = new int[]{1, and, 2, 3, 4};
        Clause cl = new Clause(bc);
        assertEquals("A-1: 2&3&4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.POSITIVE);
        assertTrue(cl.limit < 0);
    }

    @Test
    public void constructorBCEquiv() {
        System.out.println("Constructor BC equiv");
        int[] bc = new int[]{1, eqv, 2, 3, 4};
        Clause cl = new Clause(bc);
        assertEquals("E-1: 2=3=4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.POSITIVE);
        assertTrue(cl.limit < 0);
    }


    @Test
    public void constructorLitOr() {
        System.out.println("Constructor Lit or");
        Clause cl = new Clause(1,Connective.OR,2,3,4);
        assertEquals("1: 2,3,4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.POSITIVE);
        assertEquals(1,cl.limit);
    }

    @Test
    public void constructorLitAtleast() {
        System.out.println("Constructor Lit atleast");
        Clause cl = new Clause(1,Connective.ATLEAST,1,2,3,4);
        assertEquals("1: 2,3,4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.POSITIVE);
        assertEquals(1,cl.limit);
        cl = new Clause(1,Connective.ATLEAST,3,2,3,4);
        assertEquals("A-1: 2&3&4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.POSITIVE);
        assertEquals(-1,cl.limit);
    }

    @Test
    public void constructorLitAnd() {
        System.out.println("Constructor Lit and");
        Clause cl = new Clause(1,Connective.AND,-2,-3,-4);
        assertEquals("A-1: -2&-3&-4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.NEGATIVE);
        assertEquals(-1,cl.limit);
    }

    @Test
    public void constructorLitEquiv() {
        System.out.println("Constructor Lit equiv");
        Clause cl = new Clause(1,Connective.EQUIV,-2,-3,-4);
        assertEquals("E-1: -2=-3=-4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.NEGATIVE);
        assertEquals(-1,cl.limit);
    }




    /*
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
        Clause c2 = c1.removeMultipleAndComplementaryLiterals(nextInt,doubleLiterals,complementaryLiterals);
        assertNotSame(c1,c2);
        assertEquals("[3, 4]", doubleLiterals.toString());
        assertEquals("2: 3,4,6",c2.toNumbers());
        assertEquals(c2.structure,ClauseStructure.POSITIVE);

        Clause c3 = new Clause(1, Connective.OR, 3,3,3);
        Clause c4 = c3.removeMultipleAndComplementaryLiterals(nextInt,doubleLiterals,complementaryLiterals);
        assertNotSame(c3,c4);
        assertEquals("[3]", doubleLiterals.toString());
        assertEquals("A-3: 3",c4.toNumbers());

        Clause c5 = c3.removeMultipleAndComplementaryLiterals(null,doubleLiterals,complementaryLiterals);
        assertSame(c3,c5);
        assertEquals("A-1: 3",c3.toNumbers());

        Clause c6 = new Clause(1, Connective.OR, 3,3,-3);
        Clause c7 = c6.removeMultipleAndComplementaryLiterals(nextInt,doubleLiterals,complementaryLiterals);
        assertNotSame(c6,c7);
        assertEquals("[3]", doubleLiterals.toString());
        assertEquals("[3]", complementaryLiterals.toString());
        assertEquals(c7.structure,ClauseStructure.TAUTOLOGY);
        assertEquals("4: 3,-3",c7.toNumbers());

        Clause c8 = c6.removeMultipleAndComplementaryLiterals(null,doubleLiterals,complementaryLiterals);
        assertSame(c6,c8);
        assertEquals(c6.structure,ClauseStructure.TAUTOLOGY);

        Clause c9 = new Clause(1, Connective.INTERVAL, 2,3, 3,3,-3);
        Clause c10 = c9.removeMultipleAndComplementaryLiterals(nextInt,doubleLiterals,complementaryLiterals);
        assertNotSame(c9,c10);
        assertEquals("[]", doubleLiterals.toString());
        assertEquals("[3]", complementaryLiterals.toString());
        assertEquals(c10.structure,ClauseStructure.CONTRADICTORY);
        assertEquals("L-5: [2,1]: 3",c10.toNumbers());

        Clause c11 = new Clause(1, Connective.INTERVAL, 2,3, 3,4,5,-3,-4,6,7);
        Clause c12 = c11.removeMultipleAndComplementaryLiterals(nextInt,doubleLiterals,complementaryLiterals);
        assertNotSame(c11,c12);
        assertEquals("[]", doubleLiterals.toString());
        assertEquals("[3, 4]", complementaryLiterals.toString());
        assertEquals(c12.structure,ClauseStructure.POSITIVE);
        assertEquals("L-6: [2,3]: 5,6,7",c12.toNumbers());

    }
    */

}