package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Management.Controller;
import Management.GlobalParameters;
import Management.ProblemSupervisor;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
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
    int itv = Connective.INTERVAL.ordinal();

    short one = (short)1;

    @Test
    public void constructorOR() throws Unsatisfiable {
        System.out.println("Constructor OR");
        Clause clause = new Clause(1,Connective.OR,(short)1,
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

        clause = new Clause(2,Connective.OR,(short)1,
                IntArrayList.wrap(new int[]{2,-3,4,2,-3,4,4}));
        assertEquals("2: 2,-3,4",clause.toNumbers());
        assertEquals("2: 2,-3,4 [2]",clause.infoString(0,null));
        assertEquals("Input: Clause 2",clause.inferenceStep.toString(symboltable));
    }

    @Test
    public void constructorATLEAST() throws Unsatisfiable  {
        System.out.println("Constructor ATLEAST");
        Clause clause = new Clause(1,Connective.ATLEAST,one,
                IntArrayList.wrap(new int[]{2,3,4}));
        assertEquals("1: 2,3,4",clause.toNumbers());
        assertEquals("1: q,r,s",clause.toString(0,symboltable));
        assertEquals(Connective.OR,clause.connective);
        assertEquals(clause.structure,ClauseStructure.POSITIVE);

        clause = new Clause(1,Connective.ATLEAST,(short)3,
                IntArrayList.wrap(new int[]{-2,-3,-4}));
        assertEquals("A-1: -2&-3&-4",clause.toNumbers());
        assertEquals("A-1: -q&-r&-s",clause.toString(0,symboltable));
        assertEquals(Connective.AND,clause.connective);
        assertEquals(clause.structure,ClauseStructure.NEGATIVE);

        clause = new Clause(1,Connective.ATLEAST,(short)4,
                IntArrayList.wrap(new int[]{-2,-3,-4}));
        assertEquals(clause.structure,ClauseStructure.CONTRADICTORY);

        clause = new Clause(2,Connective.ATLEAST,(short)2,
                IntArrayList.wrap(new int[]{2,3,4,2,3,2,2,3,4,-5,4,4}));
        assertEquals("L-2: 2: 2^2,3^2,4^2,-5",clause.toNumbers());
        assertEquals("L-2: 2: 2^2,3^2,4^2,-5 [2]",clause.infoString(0,null));
        assertEquals("Input: Clause 2",clause.inferenceStep.toString(symboltable));
    }


    @Test
    public void constructorATMOST()  throws Unsatisfiable {
        System.out.println("Constructor Atmost");
        Clause clause = new Clause(1,Connective.ATMOST,one,
                IntArrayList.wrap(new int[]{2,3,4}));
        assertEquals("L-1: 2: -2,-3,-4",clause.toNumbers());
        assertEquals("L-1: 2: -q,-r,-s",clause.toString(0,symboltable));
        assertEquals(Connective.ATLEAST,clause.connective);
        assertEquals(clause.structure,ClauseStructure.NEGATIVE);
        assertEquals("L-1: 2: -q,-r,-s [1]",clause.infoString(0,symboltable));
        assertEquals("1: atmost 1: q,r,s -> L-1: 2: -q,-r,-s",clause.inferenceStep.toString(symboltable));

        clause = new Clause(1,Connective.ATMOST,(short)0,
                IntArrayList.wrap(new int[]{2,3,4}));
        assertEquals("A-1: -2&-3&-4",clause.toNumbers());
        assertEquals(Connective.AND,clause.connective);
        assertEquals(clause.structure,ClauseStructure.NEGATIVE);

        clause = new Clause(1,Connective.ATMOST,(short)3,
                IntArrayList.wrap(new int[]{2,3,4}));
        assertEquals(clause.structure,ClauseStructure.TAUTOLOGY);
    }

    @Test
    public void constructorBCOr()  throws Unsatisfiable {
        System.out.println("Constructor BC or");
        int[] bc = new int[]{1, or, 2, 3, 4};
        Clause cl = new Clause(bc);
        assertEquals("1: 2,3,4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.POSITIVE);
        assertEquals(1,cl.minLimit);

        bc = new int[]{2, or, 2, 3, 4,3,3,4};
        cl = new Clause(bc);
        assertEquals("2: 2,3,4", cl.toNumbers());
    }

    @Test
    public void constructorBCAtleast()  throws Unsatisfiable {
        System.out.println("Constructor BC atleast");
        int[] bc = new int[]{1, atl, 2, 2, 3, 4};
        Clause cl = new Clause(bc);
        assertEquals("L-1: 2: 2,3,4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.POSITIVE);
        assertEquals(2,cl.minLimit);

        bc = new int[]{2, atl, 3, 2, 3, 4};
        cl = new Clause(bc);
        assertEquals("A-2: 2&3&4", cl.toNumbers());
        assertEquals(-1,cl.minLimit);

        bc = new int[]{3, atl, 0, 2, 3, 4};
        cl = new Clause(bc);
        assertEquals(cl.structure,ClauseStructure.TAUTOLOGY);

        bc = new int[]{4, atl, 1, -2, -3, -4};
        cl = new Clause(bc);
        assertEquals("4: -2,-3,-4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.NEGATIVE);

        bc = new int[]{5, atl, 2, 2,2,3,3,3,2,4,4,4,5};
        cl = new Clause(bc);
        assertEquals("L-5: 2: 2^2,3^2,4^2,5", cl.toNumbers());

    }

    @Test
    public void constructorBCAtmost()  throws Unsatisfiable {
        System.out.println("Constructor BC atmost");
        int[] bc = new int[]{1, atm, 2, 2, 3, 4};
        Clause cl = new Clause(bc);
        assertEquals("1: -2,-3,-4", cl.toNumbers());
        assertEquals(cl.connective, Connective.OR);
        assertEquals(cl.structure,ClauseStructure.NEGATIVE);
        assertEquals("1: -q,-r,-s [1]",cl.infoString(0,symboltable));
        assertEquals("M-1: 2 q,r,s -> 1: -q,-r,-s",cl.inferenceStep.toString(symboltable));

        bc = new int[]{2, atm, 0, 2, 3, 4};
        cl = new Clause(bc);
        assertEquals("A-2: -2&-3&-4", cl.toNumbers());
        bc = new int[]{2, atm, 3, 2, 3, 4};
        cl = new Clause(bc);
        assertEquals(cl.structure,ClauseStructure.TAUTOLOGY);

        bc = new int[]{3, atm, 2, 2,2,3,3,4, 3, 4};
        cl = new Clause(bc);
        assertEquals("L-3: 5: -2^2,-3^3,-4^2", cl.toNumbers());

        bc = new int[]{4, atm, 5, 2,2,3,3,4, 3, 4};
        cl = new Clause(bc);
        assertEquals("L-4: 2: -2^2,-3^2,-4^2", cl.toNumbers());

    }
    @Test
    public void constructorBCAnd()  throws Unsatisfiable {
        System.out.println("Constructor BC and");
        int[] bc = new int[]{1, and, 2, 3, 4};
        Clause cl = new Clause(bc);
        assertEquals("A-1: 2&3&4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.POSITIVE);
        assertTrue(cl.minLimit < 0);
    }

    @Test
    public void constructorBCEquiv()  throws Unsatisfiable {
        System.out.println("Constructor BC equiv");
        int[] bc = new int[]{1, eqv, 2, 3, 4};
        Clause cl = new Clause(bc);
        assertEquals("E-1: 2=3=4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.POSITIVE);
        assertTrue(cl.minLimit < 0);
    }


    @Test
    public void constructorLitOr()  throws Unsatisfiable {
        System.out.println("Constructor Lit or");
        Clause cl = new Clause(1,Connective.OR,2,3,4);
        assertEquals("1: 2,3,4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.POSITIVE);
        assertEquals(1,cl.minLimit);

        cl = new Clause(2,Connective.OR,2,3,2,3,2,4);
        assertEquals("2: 2,3,4", cl.toNumbers());
    }

    @Test
    public void constructorLitAtleast()  throws Unsatisfiable {
        System.out.println("Constructor Lit atleast");
        Clause cl = new Clause(1,Connective.ATLEAST,1,2,3,4);
        assertEquals("1: 2,3,4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.POSITIVE);
        assertEquals(1,cl.minLimit);
        cl = new Clause(1,Connective.ATLEAST,3,2,3,4);
        assertEquals("A-1: 2&3&4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.POSITIVE);
        assertEquals(-1,cl.minLimit);
        cl = new Clause(2,Connective.ATLEAST,2,2,3,2,2,2,4);
        assertEquals("L-2: 2: 2^2,3,4", cl.toNumbers());
    }
    @Test
    public void constructorLitAtmost()  throws Unsatisfiable {
        System.out.println("Constructor Lit atmost");
        Clause cl = new Clause(1, Connective.ATMOST, 2, 2, 3, 4, 5, 6);
        assertEquals("L-1: 3: -2,-3,-4,-5,-6", cl.toNumbers());
        assertEquals(cl.structure, ClauseStructure.NEGATIVE);
        assertEquals("L-1: 3: -q,-r,-s,-t,-u [1]",cl.infoString(0,symboltable));
        assertEquals("1: atmost 2: q,r,s,t,u -> L-1: 3: -q,-r,-s,-t,-u",cl.inferenceStep.toString(symboltable));
    }


    @Test
    public void constructorLitAnd()  throws Unsatisfiable {
        System.out.println("Constructor Lit and");
        Clause cl = new Clause(1,Connective.AND,-2,-3,-4);
        assertEquals("A-1: -2&-3&-4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.NEGATIVE);
        assertEquals(-1,cl.minLimit);
    }

    @Test
    public void constructorLitEquiv()  throws Unsatisfiable {
        System.out.println("Constructor Lit equiv");
        Clause cl = new Clause(1,Connective.EQUIV,-2,-3,-4);
        assertEquals("E-1: -2=-3=-4", cl.toNumbers());
        assertEquals(cl.structure,ClauseStructure.NEGATIVE);
        assertEquals(-1,cl.minLimit);
    }

    @Test
    public void intervalClause() throws Unsatisfiable  {
        System.out.println("intervalClause");
        int[] id = new int[]{1};
        int[] bc = new int[]{1, itv, 2, 4, 1, 2, 3, 4, 5};
        ArrayList<Clause> cls = Clause.intervalClause(() -> ++id[0], bc);
        assertEquals(2, cls.size());
        assertEquals("L-2: 2: 1,2,3,4,5",cls.get(0).toNumbers());
        assertEquals("Input: Clause 2",cls.get(0).inferenceStep.toString(null));
        assertEquals("3: -1,-2,-3,-4,-5",cls.get(1).toNumbers());
        assertEquals("3: atmost 4: 1,2,3,4,5 -> 3: -1,-2,-3,-4,-5",cls.get(1).inferenceStep.toString(null));

        bc = new int[]{1, itv, 0, 4, 1, 2, 3, 4, 5};
        cls = Clause.intervalClause(() -> ++id[0], bc);
        assertEquals("4: -1,-2,-3,-4,-5",cls.get(0).toNumbers());

        bc = new int[]{1, itv, 2, 5, 1, 2, 3, 4, 5};
        cls = Clause.intervalClause(() -> ++id[0], bc);
        assertEquals("L-5: 2: 1,2,3,4,5",cls.get(0).toNumbers());
    }

    @Test
    public void cloneTest()  throws Unsatisfiable {
        System.out.println("clone");
        Clause c1 = new Clause(1, Connective.ATLEAST, 2, 2, -3, 4, 5);
        Clause c2 = c1.clone(1);
        assertEquals(c1.toNumbers(), c2.toNumbers());
        Clause c3 = new Clause(2, Connective.AND,  2, -3, 4, 5);
        Clause c4 = c3.clone(2);
        assertEquals(c3.toNumbers(), c4.toNumbers());
        Clause c5 = c3.clone(3);
        assertEquals("A-3: 2&-3&4&5",c5.toNumbers());
    }

    @Test
    public void toAtmost()  throws Unsatisfiable {
        System.out.println("toAtmost");
        Clause c1 = new Clause(1, Connective.ATLEAST, 2, 2, -3, 4, 5,6);
        Clause c2 = c1.toAtmost(2);
        assertEquals("M-2: 3: -2,3,-4,-5,-6",c2.toNumbers());
        assertEquals("M-2: 3: -q,r,-s,-t,-u [1]",c2.infoString(0,symboltable));
        assertEquals("L-1: 2: q,-r,s,t,u -> M-2: 3: -q,r,-s,-t,-u",c2.inferenceStep.toString(symboltable));
        assertEquals("Atleast to Atmost:\n" +
                "atleast n l_1,...,l_k -> atmost k-n -l_1,...,-l_k",c2.inferenceStep.rule());
    }
    @Test
    public void toCNF()  throws Unsatisfiable {
        System.out.println("toCNF");
        int[] ids = new int[]{0};
        Clause c1 = new Clause(new int[]{1, atl, 3, 1,1,2,2,3,4});
        ArrayList<Clause> cnf = c1.toCNF(()->++ids[0],true);
        for(Clause cl :cnf) System.out.println(cl.toNumbers());
        assertEquals("1: 1,2",cnf.get(0).toNumbers());
        assertEquals("2: 1,3,4",cnf.get(1).toNumbers());
        assertEquals("3: 2,3,4",cnf.get(2).toNumbers());
        assertEquals("Atleast-Clause to Conjunctive Normal Form\n" +
                "L-1: 3: 1^2,2^2,3,4 -> 3: 2,3,4",cnf.get(2).inferenceStep.toString());

    }


    @Test
    public void removeComplementaryLiterals()  throws Unsatisfiable {
        System.out.println("removeComplementaryLiterals");
        int[] ids = new int[]{0};
        Clause c1 = new Clause(new int[]{1, atl, 3, 1,2,3,4});
        Clause c2 = c1.removeComplementaryLiterals(()->++ids[0]);
        assertSame(c1,c2);
        c1 = new Clause(new int[]{1, atl, 3, 1,2,3,-2,-1});
        c2 = c1.removeComplementaryLiterals(null);
        assertSame(c1,c2);
        assertEquals("A-1: 3",c1.toNumbers());

        c1 = new Clause(new int[]{1, atl, 3, 1,2,3,-2,-1,2});
        c2 = c1.removeComplementaryLiterals(()->++ids[0]);
        assertNotSame(c1,c2);
        assertEquals("L-1: 3: 1,2^2,3,-2,-1",c1.toNumbers());
        assertEquals("1: 2,3",c2.toNumbers());
        assertEquals("Complementary Literals:\n" +
                "L-1: 3: 1,2^2,3,-2,-1 at 2,1 -> 1: 2,3",c2.inferenceStep.toString());

        c1 = new Clause(new int[]{1, atl, 3, 1,-2,3,-2,-1,2});
        c2 = c1.removeComplementaryLiterals(()->++ids[0]);
        assertNotSame(c1,c2);
        assertEquals("2: -2,3",c2.toNumbers());

    }
    @Test
    public void removeTrueFalseLiterals()  throws Unsatisfiable {
        System.out.println("removeTrueFalseLiterals");
        int[] ids = new int[]{1};
        IntUnaryOperator status = (int literal) -> {return literal > 3 ? (literal % 2 == 0 ? 1 : -1) : 0;};
        Clause c1 = new Clause(new int[]{1, atl, 3, 1, 4, 2, 6, 3, 5});
        Clause c2 = c1.removeTrueFalseLiterals(status,()->++ids[0]);
        assertEquals("L-1: 3: 1,4,2,6,3,5",c1.toNumbers());
        assertEquals("2: 1,2,3",c2.toNumbers());
        assertEquals("Removed Literals with Truth Value:\n" +
                "L-1: 3: 1,4,2,6,3,5 and true(4,6) and false(5) -> 2: 1,2,3",c2.inferenceStep.toString());
        c2 = c1.removeTrueFalseLiterals(status,null);
        assertEquals("1: 1,2,3",c1.toNumbers());}


    EquivalenceClasses prepare() {
        GlobalParameters globalParameters=new GlobalParameters();
        HashMap<String,Object> problemParameters=new HashMap<>();
        problemParameters.put("name","test");

        Controller controller= null; //new Controller(null,null,null);
        ProblemSupervisor problemSupervisor= null;//new ProblemSupervisor(controller,globalParameters,problemParameters,null);
        problemSupervisor.model=new Model(20);
        return new EquivalenceClasses(problemSupervisor);
        }

    @Test
    public void replaceEquivalences() throws Unsatisfiable {
        System.out.println("replaceEquivalences");
        EquivalenceClasses eqc = prepare();
        eqc.addBasicEquivalenceClause(new int[]{10,eqv,1,3,5});
        int[] id = new int[]{1};
        Clause c1 = new Clause(1, Connective.ATLEAST, 3, 6,5,4,3,2,1);
        Clause c2 = c1.replaceEquivalences(eqc, ()->++id[0]);
        assertEquals("L-1: 3: 6,5,4,3,2,1",c1.toNumbers());
        assertEquals("L-2: 3: 6,1^3,4,2",c2.toNumbers());

        c1 = new Clause(1, Connective.ATLEAST, 2, 6,5,4,3,2,1);
        c2 = c1.replaceEquivalences(eqc, ()->++id[0]);
        assertEquals("L-1: 2: 6,5,4,3,2,1",c1.toNumbers());
        assertEquals("L-3: 2: 6,1^2,4,2",c2.toNumbers());
        c2 = c1.replaceEquivalences(eqc,null);
        assertEquals("L-1: 2: 6,1^2,4,2",c1.toNumbers());
    }
    @Test
    public void splitOffMultiples()  throws Unsatisfiable {
        System.out.println("splitOffMultiples");
        int[] id = new int[]{1};
        Clause c1 = new Clause(1, Connective.ATLEAST, 3, 1,1,2,2,3);
        ArrayList<Clause> clauses = c1.splitOffMultiples(()->++id[0],true);
        assertEquals("2: 1,2",clauses.get(0).toNumbers());
        assertEquals("Extract CNF from Multiple Literals:\n" +
                "L-1: 3: 1^2,2^2,3 -> 2: 1,2",clauses.get(0).inferenceStep.toString());

        c1 = new Clause(2, Connective.ATLEAST, 4, 1,1,2,2,3,3,3,3,5);
        clauses = c1.splitOffMultiples(()->++id[0],true);
        assertEquals("3: 1,3",clauses.get(0).toNumbers());
        assertEquals("4: 2,3",clauses.get(1).toNumbers());
        assertEquals("Extract CNF from Multiple Literals:\n" +
                "L-2: 4: 1^2,2^2,3^4,5 -> 3: 1,3",clauses.get(0).inferenceStep.toString());
        //System.out.println(clauses.get(0).inferenceStep.rule());

    }



}