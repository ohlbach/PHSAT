package Solvers.Resolution;

import Datastructures.Clauses.Quantifier;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import junit.framework.TestCase;

import java.util.ArrayList;
import java.util.function.IntSupplier;

public class ClauseTest extends TestCase {
    static int cOr = Quantifier.OR.ordinal();
    static int cAtleast = Quantifier.ATLEAST.ordinal();

    static ArrayList<Literal> removedLiterals = new ArrayList<>();


    static Symboltable symboltable = new Symboltable(10);
    static {
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"s");
    }

    public void testConstructorInputClause1() {
        System.out.println("constructor InputClause Disjunction");
        int[] c1 = new int[]{10,cOr,1,2,-3};
        Clause clause1 = new Clause(c1);
        //System.out.println(clause1.toString());
        //System.out.println(clause1.toString(symboltable,10));
        assertEquals(10,clause1.identifier);
        assertEquals(Quantifier.OR,clause1.quantifier);
        assertTrue(clause1.exists);
        assertTrue(clause1.isDisjunction);
        assertEquals(1,clause1.limit);
        assertEquals(3,clause1.size());
        assertEquals(3,clause1.expandedSize);
        assertEquals(3,clause1.expandedSize());
        assertFalse(clause1.hasMultiplicities);
        assertNull(clause1.previousClause);
        assertNull(clause1.nextClause);
        assertEquals(0,clause1.timestamp1);
        assertEquals("Input Clause Id: 10",clause1.inferenceStep.toString(symboltable));

        assertEquals(1,clause1.findLiteral(1).literal);
        assertEquals(2,clause1.findLiteral(2).literal);
        assertEquals(-3,clause1.findLiteral(-3).literal);
        assertNull(clause1.findLiteral(3));
        assertEquals(clause1,clause1.findLiteral(1).clause);
        assertEquals(ClauseType.POSITIVENEGATIVE,clause1.clauseType);
    }

    public void testConstructorInputClause2() {
        System.out.println("constructor InputClause Atleast");
        int[] c1 = new int[]{11, cAtleast, 2, 1, 2, 1, -3, 2, -3,4};
        Clause clause = new Clause(c1);
        //System.out.println(clause.toString());
        //System.out.println(clause.toString(symboltable,10));
        assertEquals("11: >= 2 p^2,q^2,-r^2,s",clause.toString(symboltable,0));
        assertEquals(11, clause.identifier);
        assertEquals(Quantifier.ATLEAST, clause.quantifier);
        assertEquals(4,clause.size());
        assertEquals(7,clause.expandedSize());
        assertFalse(clause.isDisjunction);
        assertTrue(clause.hasMultiplicities);
        assertEquals(ClauseType.POSITIVENEGATIVE,clause.clauseType);
    }


    public void testConstructorOr() {
        System.out.println("constructor Or");
        Clause clause = new Clause(10, 1, 2, -3);
        assertEquals("10: 1v2v-3", clause.toString());
        assertEquals(3,clause.expandedSize);
    }

    public void testDivideByGCD() {
        System.out.println("divide by GCD");
        Clause clause = new Clause(new int[]{10, cAtleast, 2, 1,1,1,1,2,2,2,2,2,2});
        clause.divideByGCD();
        assertEquals("10: 1v2", clause.toString());
        clause = new Clause(new int[]{11, cAtleast, 2, 1,1,2,2,3,3});
        clause.divideByGCD();
        assertEquals("11: 1v2v3", clause.toString());
        clause = new Clause(new int[]{12, cAtleast, 4, 1,1,1,1,2,2,3,3});
        clause.divideByGCD();
        assertEquals("12: >= 2 1^2,2,3", clause.toString());}


    IntArrayList trueLiterals = new IntArrayList();
    public void testReduceByTrueLiterals() {
        System.out.println("reduceByTrueLiterals");
        removedLiterals.clear();
        trueLiterals.clear();
        Clause clause = new Clause(new int[]{1, cAtleast, 3, 1, 2, 3});
        assertEquals(0,Clause.reduceByTrueLiterals(3,3,clause.literals,(l -> removedLiterals.add(l)),
                (l -> trueLiterals.add(l))));
        assertEquals("[1, 2, 3]",trueLiterals.toString());
        assertEquals(3,removedLiterals.size());

        removedLiterals.clear();
        trueLiterals.clear();
        clause = new Clause(new int[]{2, cAtleast, 2, 1, 2, 3});
        assertEquals(2,Clause.reduceByTrueLiterals(2,3,clause.literals,(l -> removedLiterals.add(l)),
                (l -> trueLiterals.add(l))));
        assertEquals("[]",trueLiterals.toString());
        assertEquals(0,removedLiterals.size());

        removedLiterals.clear();
        trueLiterals.clear();
        clause = new Clause(new int[]{3, cAtleast, 5, 1, 1, 3,4, 2, 2});
        assertEquals(1,Clause.reduceByTrueLiterals(5,6,clause.literals,(l -> removedLiterals.add(l)),
                (l -> trueLiterals.add(l))));
        assertEquals("[1, 2]",trueLiterals.toString());
        assertEquals(2,removedLiterals.size());
        assertEquals("3,4",Literal.toString(clause.literals,null));

        removedLiterals.clear();
        trueLiterals.clear();
        clause = new Clause(new int[]{4, cAtleast, 4, 1, 1, 3,4, 2, 2});
        assertEquals(4,Clause.reduceByTrueLiterals(4,6,clause.literals,(l -> removedLiterals.add(l)),
                (l -> trueLiterals.add(l))));
        assertEquals("[]",trueLiterals.toString());
        assertEquals(0,removedLiterals.size());
        assertEquals("1^2,3,4,2^2",Literal.toString(clause.literals,null));

        removedLiterals.clear();
        trueLiterals.clear();
        clause = new Clause(new int[]{5, cAtleast, 3, 1, 1, 2});
        assertEquals(0,Clause.reduceByTrueLiterals(3,3,clause.literals,(l -> removedLiterals.add(l)),
                (l -> trueLiterals.add(l))));
        assertEquals("[1, 2]",trueLiterals.toString());
        assertEquals(2,removedLiterals.size());
        assertTrue(clause.literals.isEmpty());}

    public void testReduceToEssentialLiterals() {
        System.out.println("reduceToEssentialLiterals");
        trueLiterals.clear();
        removedLiterals.clear();
        Clause clause = new Clause(new int[]{1, cAtleast, 2, 1, 1, 3, 2, 2});
        assertEquals(2,Clause.reduceToEssentialLiterals(2,clause.literals,(l -> removedLiterals.add(l)),
                (l -> trueLiterals.add(l))));
        assertEquals("1,2", Literal.toString(clause.literals,null));
        assertEquals("3", Literal.toString(removedLiterals,null));
        assertEquals("[]",trueLiterals.toString());

        removedLiterals.clear();
        clause = new Clause(new int[]{2, cAtleast, 2, 1, 1, 3, 4, 2, 2});
        assertEquals(6,Clause.reduceToEssentialLiterals(2,clause.literals,(l -> removedLiterals.add(l)),
                (l -> trueLiterals.add(l))));
        assertEquals("1^2,3,4,2^2", Literal.toString(clause.literals,null));
        assertTrue(removedLiterals.isEmpty());
        assertTrue(trueLiterals.isEmpty());
    }


    public void testRemoveLiterals() {
        System.out.println("removeLiterals");
        removedLiterals.clear();
        trueLiterals.clear();
        Clause clause = new Clause(new int[]{1, cOr, 1, 2, 3});
        assertEquals(0, clause.removeLiterals((l-> {return 0;}),(l -> removedLiterals.add(l)), (l->trueLiterals.add(l)) ));
        assertEquals("1: 1v2v3",clause.toString());

        assertEquals(0, clause.removeLiterals((l-> {return (l.literal == 2)? -1: 0;}),(l -> removedLiterals.add(l)), (l->trueLiterals.add(l)) ));
        assertEquals("1: 1v3",clause.toString());
        assertEquals("2",Literal.toString(removedLiterals,null));

        removedLiterals.clear();
        trueLiterals.clear();
        assertEquals(1, clause.removeLiterals((l-> {return (l.literal == 3)? -1: 0;}),(l -> removedLiterals.add(l)), (l->trueLiterals.add(l)) ));
        assertEquals("3,1",Literal.toString(removedLiterals,null));
        assertEquals("[1]",trueLiterals.toString());

        removedLiterals.clear();
        trueLiterals.clear();
        clause = new Clause(new int[]{1, cOr, 1, 2, 3});
        assertEquals(1, clause.removeLiterals((l-> {return (l.literal == 3)? 1: 0;}),(l -> removedLiterals.add(l)), (l->trueLiterals.add(l)) ));
        assertEquals("3,1,2",Literal.toString(removedLiterals,null));
        assertEquals("[]",trueLiterals.toString());

        removedLiterals.clear();
        trueLiterals.clear();
        clause = new Clause(new int[]{1, cOr, 1, 2, 3});
        assertEquals(1, clause.removeLiterals((l-> {return (l.literal == 3|| l.literal == 1)? -1: 0;}),(l -> removedLiterals.add(l)), (l->trueLiterals.add(l)) ));
        assertEquals("1,3,2",Literal.toString(removedLiterals,null));
        assertEquals("[2]",trueLiterals.toString());

        removedLiterals.clear();
        trueLiterals.clear();
        clause = new Clause(new int[]{1, cOr, 1, 2, 3});
        assertEquals(-1, clause.removeLiterals((l-> {return -1;}),(l -> removedLiterals.add(l)), (l->trueLiterals.add(l)) ));
        assertEquals("1,2,3",Literal.toString(removedLiterals,null));
        assertEquals("[]",trueLiterals.toString());

        removedLiterals.clear();
        trueLiterals.clear();
        clause = new Clause(new int[]{1, cAtleast, 3, 1, 2, 3});
        assertEquals(1, clause.removeLiterals((l-> {return 0;}),(l -> removedLiterals.add(l)), (l->trueLiterals.add(l)) ));
        assertEquals("1,2,3",Literal.toString(removedLiterals,null));
        assertEquals("[1, 2, 3]",trueLiterals.toString());

        removedLiterals.clear();
        trueLiterals.clear();   // Example:  >= 2 p^2,q,r. and false(q) -> >= 2 p^2,r -> p must be true.
        clause = new Clause(new int[]{1, cAtleast, 2, 1, 1, 2, 3});
        assertEquals(1, clause.removeLiterals((l-> {return (l.literal == 2)? -1: 0;}),(l -> removedLiterals.add(l)), (l->trueLiterals.add(l)) ));
        assertEquals("2,1^2,3",Literal.toString(removedLiterals,null));
        assertEquals("[1]",trueLiterals.toString());

        removedLiterals.clear();
        trueLiterals.clear();   // Example: >= 2 p,q^2,r^2,s and false(p) -> >= 2 q^2,r^2,s -> q,r
        clause = new Clause(new int[]{1, cAtleast, 2, 1, 2, 2, 3, 3, 4});
        assertEquals(0, clause.removeLiterals((l-> {return (l.literal == 1)? -1: 0;}),(l -> removedLiterals.add(l)), (l->trueLiterals.add(l)) ));
        assertEquals("1,4",Literal.toString(removedLiterals,null));
        assertEquals("[]",trueLiterals.toString());
        assertEquals("1: 2v3",clause.toString());
        assertEquals(ClauseType.POSITIVE,clause.clauseType);

        removedLiterals.clear();
        trueLiterals.clear();   // Example: >= 2 p,q^2,r^2,s and false(p) -> >= 2 q^2,r^2,s -> q,r
        clause = new Clause(new int[]{1, cAtleast, 2, -1, 2, 2, 3, 3, 4});
        assertEquals(ClauseType.MIXEDPOSITIVE,clause.clauseType);
        assertEquals(0, clause.removeLiterals((l-> {return (l.literal == -1)? -1: 0;}),(l -> removedLiterals.add(l)), (l->trueLiterals.add(l)) ));
        assertEquals("-1,4",Literal.toString(removedLiterals,null));
        assertEquals("[]",trueLiterals.toString());
        assertEquals("1: 2v3",clause.toString());
        assertEquals(ClauseType.POSITIVE,clause.clauseType);
    }


    private Literal getLit(Clause clause, int index) {
        return clause.literals.get(index);}

    public void testResolve1()  {
        System.out.println("resolve between Or-Clauses");
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];

        Clause clause1 = new Clause(new int[]{1, cOr, 1, 2, 3});
        Clause clause2 = new Clause(new int[]{2, cOr, 1, -2, 3});
        Clause resolvent1 = Clause.resolve(getLit(clause1, 1), getLit(clause2, 1), nextId, null);
        assertEquals("11: 1v3", resolvent1.toString());

        Clause clause3 = new Clause(new int[]{3, cOr, 4, -2, 5});
        Clause resolvent2 = Clause.resolve(getLit(clause1, 1), getLit(clause3, 1), nextId, null);
        assertEquals("12: 1v3v4v5", resolvent2.toString());

        Clause clause4 = new Clause(new int[]{4, cOr, 4, -2, -1});
        assertNull(Clause.resolve(getLit(clause1, 1), getLit(clause4, 1), nextId, null));

        Clause clause5 = new Clause(new int[]{5, cOr, 1, -2 });
        Clause clause6 = new Clause(new int[]{6, cOr, 2, 3});
        Clause resolvent3 = Clause.resolve(getLit(clause5, 1), getLit(clause6, 0), nextId, null);
        assertEquals("13: 1v3", resolvent3.toString());

        Clause clause7 = new Clause(new int[]{7, cOr, 2, 1});
        IntArrayList trueLits = new IntArrayList();
        assertNull(Clause.resolve(getLit(clause5, 1), getLit(clause7, 0), nextId, (lit -> trueLits.add(lit))));
        assertEquals("[1]",trueLits.toString());
    }
    public void testResolve2() {
        System.out.println("resolve with Atleast-Clauses");
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        IntArrayList trueLits = new IntArrayList();

        Clause clause1 = new Clause(new int[]{1, cAtleast, 2, 1, 2, 3});
        Clause clause2 = new Clause(new int[]{2, cAtleast, 2, 4, -2, 5});
        Clause resolvent1 = Clause.resolve(getLit(clause1, 1), getLit(clause2, 1), nextId, (lit -> trueLits.add(lit)));
        assertTrue(trueLits.isEmpty());
        assertEquals("11: >= 3 1,3,4,5", resolvent1.toString());

        Clause clause3 = new Clause(new int[]{3, cAtleast, 2, 4, -2, 1});
        Clause resolvent2 = Clause.resolve(getLit(clause1, 1), getLit(clause3, 1), nextId, (lit -> trueLits.add(lit)));
        assertEquals("12: 3v4", resolvent2.toString()); // atleast 3 1^2,3,4
        assertEquals("[1]",trueLits.toString());

        trueLits.clear();
        Clause clause4 = new Clause(new int[]{4, cAtleast, 2, 3, -2, 1}); // atleast 3 1^2,3^2
        assertNull(Clause.resolve(getLit(clause1, 1), getLit(clause4, 1), nextId, (lit -> trueLits.add(lit))));
        assertEquals("[1, 3]",trueLits.toString());

        trueLits.clear();
        Clause clause5 = new Clause(new int[]{5, cAtleast, 2, 3, 2, 1});
        Clause clause6 = new Clause(new int[]{6, cAtleast, 2, 4, -2, -1,-1,-1});
        Clause resolvent56 = Clause.resolve(getLit(clause5, 1), getLit(clause6, 1), nextId, (lit -> trueLits.add(lit)));
        assertEquals("13: >= 2 3,-1^2,4", resolvent56.toString());
        Clause resolvent65 = Clause.resolve(getLit(clause6, 1), getLit(clause5, 1), nextId, (lit -> trueLits.add(lit)));
        assertEquals("14: >= 2 4,-1^2,3", resolvent65.toString());

        Clause clause7 = new Clause(new int[]{7, cAtleast, 2, 4, 2, 1,1,1});
        assertNull(Clause.resolve(getLit(clause6, 1), getLit(clause7, 1), nextId, (lit -> trueLits.add(lit))));
    }

    public void testResolve3() {
        System.out.println("resolve with Or and Atleast-Clauses");
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        IntArrayList trueLits = new IntArrayList();

        // Example: -p,q and >= 2 p,q,r  ->  >= 2 q^2,r.  q must be true.
        Clause clause1 = new Clause(new int[]{1, cOr, -1, 2});
        Clause clause2 = new Clause(new int[]{2, cAtleast, 2, 1, 2, 3});
        assertNull(Clause.resolve(getLit(clause1, 0), getLit(clause2, 0), nextId, (lit -> trueLits.add(lit))));
        assertEquals("[2]", trueLits.toString());

        // Example: -p,q and >= 2 p,q,r^2,s  -> >= 2 q^2,r^2,s -> p,q
        Clause clause3 = new Clause(new int[]{3, cAtleast, 2, 1, 2, 3, 3, 4});
        Clause resolvent13 = Clause.resolve(getLit(clause1, 0), getLit(clause3, 0), nextId, (lit -> trueLits.add(lit)));
        assertEquals("11: 2v3",resolvent13.toString());
    }
    }