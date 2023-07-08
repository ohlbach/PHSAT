package Solvers.Resolution;

import Datastructures.Clauses.Quantifier;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Management.Monitor.Monitor;
import Management.Monitor.MonitorLife;
import junit.framework.TestCase;

import java.util.function.IntSupplier;

public class LiteralsTest extends TestCase {
    static Symboltable symboltable = new Symboltable(10);
    static {
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
    }

    static String toString(Literals literals, int literal) {
        StringBuilder st = new StringBuilder();
        Literal l = literals.getFirstLiteralObject(1);
        while(l != null) {
            st.append(l.toString(symboltable)).append(",");
            l = l.nextLiteral;}
        return st.toString();}

    public void testAddLiteral() {
        System.out.println("addLiteral");
        Literals literals = new Literals("Test",10);
        Literal l1 = new Literal(1,1);
        assertTrue(literals.isEmpty(1));
        assertEquals(0,literals.size(1));
        assertNull(literals.getFirstLiteralObject(1));
        //System.out.println(literals.toString(symboltable));

        literals.addLiteral(l1);
        assertFalse(literals.isEmpty(1));
        assertEquals(1,literals.size(1));
        assertEquals(l1, literals.getFirstLiteralObject(1));
        assertNull(literals.getFirstLiteralObject(-1));
        //System.out.println(literals.toString(symboltable));
        //System.out.println(literals.toString());

        Literal lm1 = new Literal(-1,1);
        literals.addLiteral(lm1);
        assertEquals(1,literals.size(1));
        assertEquals(1,literals.size(-1));
        assertEquals(l1, literals.getFirstLiteralObject(1));
        assertEquals(lm1, literals.getFirstLiteralObject(-1));
        //System.out.println(literals.toString(symboltable));

        Literal l12 = new Literal(1,2);
        literals.addLiteral(l12);
        assertEquals(2,literals.size(1));
        //System.out.println(literals.toString(symboltable));

        Literal l31 = new Literal(3,1);
        literals.addLiteral(l31);
        assertEquals(2,literals.size(1));
        assertEquals(-1,literals.size(1,1));
        assertEquals(2,literals.size(1,2));
        assertEquals(1,literals.size(3));
        assertEquals(3,literals.size());

        //System.out.println(literals.toString(symboltable));
    }

    public void testWhile() {
        System.out.println("while");
        Literals literals = new Literals("Test",10);
        Literal l1 = new Literal(1,1);
        literals.addLiteral(l1);
        assertEquals("p,",toString(literals,1));

        Literal l2 = new Literal(1,2);
        literals.addLiteral(l2);
        assertEquals("p^2,p,",toString(literals,1));

        Literal l3 = new Literal(1,3);
        literals.addLiteral(l3);
        assertEquals("p^3,p^2,p,",toString(literals,1));

    }

    public void testRemoveLiteral() {
        System.out.println("remove");
        Literals literals = new Literals("Test",10);
        Literal l1 = new Literal(1,1);
        literals.addLiteral(l1);
        assertTrue(literals.contains(l1));
        literals.removeLiteral(l1);
        assertFalse(literals.contains(l1));
        assertNull(literals.getFirstLiteralObject(1));
        assertEquals(0,literals.size(1));

        literals.addLiteral(l1);
        assertNull(l1.nextLiteral);
        assertNull(l1.previousLiteral);

        Literal l2 = new Literal(1,2);
        literals.addLiteral(l2);
        assertEquals(l1, l2.nextLiteral);
        assertEquals(l2,l1.previousLiteral);

        literals.removeLiteral(l2); // the first literal
        assertEquals(1,literals.size(1));
        assertNull(l1.nextLiteral);
        assertNull(l1.previousLiteral);
        assertEquals(l1, literals.getFirstLiteralObject(1));

        literals.addLiteral(l2);
        Literal l3 = new Literal(1,3);
        literals.addLiteral(l3);
        assertEquals("p^3,p^2,p,",toString(literals,1));

        literals.removeLiteral(l2);
        assertEquals("p^3,p,",toString(literals,1));
        literals.removeLiteral(l1);
        assertEquals("p^3,",toString(literals,1));
        literals.removeLiteral(l3);
        assertEquals("",toString(literals,1));

    }

    public void testRemovePredicate() {
        System.out.println("removePredicate");
        Literals literals = new Literals("Test",10);
        Literal l1 = new Literal(1,1);
        literals.addLiteral(l1);
        Literal l2 = new Literal(1,2);
        literals.addLiteral(l2);
        assertEquals("p^2,p,",toString(literals,1));
        literals.removePredicate(-1);
        assertEquals("",toString(literals,1));
    }

    public void testIterating() {
        System.out.println("iterating");
        Clause clause = new Clause(new int[]{1, Quantifier.OR.ordinal(),1,2,3});
        Literals literals = new Literals("Test",10);
        Literal l1 = new Literal(1, 1);l1.clause = clause;
        literals.addLiteral(l1);
        Literal l2 = new Literal(1, 2);l2.clause = clause;
        literals.addLiteral(l2);
        Literal l3 = new Literal(1, 3);l3.clause = clause;
        literals.addLiteral(l3);
        Literal l4 = new Literal(1, 4);l4.clause = clause;
        literals.addLiteral(l4);

        String s = "";
        Literal lit = literals.getFirstLiteralObject(1);
        while(lit != null) {s += lit.multiplicity; lit =lit.nextLiteral;}
        assertEquals("4321",s);

        s = "";
        lit = literals.getFirstLiteralObject(1).nextLiteral;
        literals.removeLiteral(l3);
        while(lit != null) {
            if(lit.clause == null) {lit =lit.nextLiteral; continue;}
            s += lit.multiplicity; lit =lit.nextLiteral;}
        assertEquals("21",s);

        s = "";
        lit = literals.getFirstLiteralObject(1).nextLiteral;
        literals.removeLiteral(l2);
        while(lit != null) {
            if(lit.clause == null) {lit =lit.nextLiteral; continue;}
            s += lit.multiplicity; lit =lit.nextLiteral;}
        assertEquals("1",s);

        s = "";
        lit = literals.getFirstLiteralObject(1);

        while(lit != null) {
            if(lit.clause == null) {lit =lit.nextLiteral; continue;}
            s += lit.multiplicity; lit =lit.nextLiteral;}
        assertEquals("41",s);

    }

    int cOr = Quantifier.OR.ordinal();
    boolean monitoring = true;
    public void testForAllLiterals() throws Unsatisfiable {
        System.out.println("forAllLiterals");
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(10, monitor, true, nextId);
        assertFalse(resolution.literalIndexTwo.forAllLiterals(1,null,
                (literalObject -> {id[0] = 20; return true;})));

        Clause clause1 = new Clause(new int[]{1, cOr, 3,4});
        resolution.insertClause(clause1);
        assertTrue(resolution.literalIndexTwo.forAllLiterals(3,null,
                (literalObject -> {id[0] = 20; return true;})));
        assertEquals(20,id[0]);

        Clause clause2 = new Clause(new int[]{2, cOr, 4,5});
        resolution.insertClause(clause2);
        assertTrue(resolution.literalIndexTwo.forAllLiterals(3,null,
                (literalObject -> {++id[0]; return true;})));
        assertEquals(21,id[0]);
        assertFalse(resolution.literalIndexTwo.forAllLiterals(4,null,
                (literalObject -> {++id[0]; return false;})));
        assertEquals(23,id[0]);

        assertFalse(resolution.literalIndexTwo.forAllLiterals(4,
                (literalObject -> literalObject.clause.identifier == 2),
                (literalObject -> {++id[0]; return false;})));
        assertEquals(24,id[0]);

        try{resolution.literalIndexTwo.forAllLiterals(4,null,
                (literalObject -> {throw new UnsatEmptyClause("test",null,null,null);}));
            assertFalse(true);}
        catch(Unsatisfiable uns) {
            if(monitoring) System.out.println(uns);
        }
    }

    public void testTimestampClauses()  {
        System.out.println("timestampClauses");
        Monitor monitor = monitoring ? new MonitorLife() : null;
        int[] id = new int[]{10};
        IntSupplier nextId = () -> ++id[0];
        Resolution resolution = new Resolution(10, monitor, true, nextId);

        assertFalse(resolution.literalIndexMore.timestampClauses(1,null,5,true));
        Clause clause1 = new Clause(new int[]{1, cOr, 1, 2, 3});
        resolution.insertClause(clause1);

        assertFalse(resolution.literalIndexMore.timestampClauses(1,
                (litObject -> litObject.clause.identifier == 2),
                5,true));
        assertTrue(resolution.literalIndexMore.timestampClauses(2,
                (litObject -> litObject.clause.identifier == 1),
                5,true));
        assertEquals(5,clause1.timestamp1);
        assertTrue(resolution.literalIndexMore.timestampClauses(3,
                (litObject -> litObject.clause.identifier == 1),
                6,false));
        assertEquals(6,clause1.timestampSubsumption);

        Clause clause2 = new Clause(new int[]{2, cOr, 3,4,5});
        resolution.insertClause(clause2);
        assertTrue(resolution.literalIndexMore.timestampClauses(3,
                (litObject -> litObject.clause.identifier == 1),
                7,true));
        assertEquals(7,clause1.timestamp1);
        assertEquals(0,clause2.timestamp1);

        assertTrue(resolution.literalIndexMore.timestampClauses(3,
                (litObject -> litObject.clause.identifier >= 1),
                8,true));
        assertEquals(8,clause1.timestamp1);
        assertEquals(8,clause2.timestamp1);
    }




}