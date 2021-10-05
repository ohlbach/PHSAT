package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Management.Controller;
import Management.GlobalParameters;
import Management.Monitor;
import Management.ProblemSupervisor;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 26.08.2018.
 */
public class ClauseTest {

    private static int counter = 1;
    private static final ClauseType type = ClauseType.OR;
    StringBuffer errors = new StringBuffer();
    StringBuffer warnings = new StringBuffer();

    private Clause make(int... literals) {
        Clause cl = new Clause(counter++,type,literals.length);
        int i = -1;
        for(int l:literals) {
            cl.add(new CLiteral(l,cl,++i));}
        cl.setStructure();
        return cl;}

    private Clause make(int id, ClauseType type, int... literals) {
        Clause cl = new Clause(id,type,literals.length);
        int i = -1;
        for(int l:literals) {
            cl.add(new CLiteral(l,cl,++i));}
        cl.setStructure();
        return cl;}

    private Symboltable symboltable = new Symboltable(10);
    private void prepare() {
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"s");
        symboltable.setName(5,"t");
        symboltable.setName(6,"u");

    }

    @Test
    public void constructOR() {
        System.out.println("constructOR");
        prepare();
        Clause clause = make(1, ClauseType.OR, 1, 2);
        assertEquals("1: 1,2",clause.toString());
        assertEquals("1: 1,2",clause.toNumbers());
        assertEquals("1:  p,q",clause.toString(4,symboltable));
       // System.out.println(clause.toString());
    }

    @Test
    public void constructAND() {
        System.out.println("constructAND");
        prepare();
        Clause clause = make(1, ClauseType.AND, 1, 2);
        assertEquals("A-1: 1&2",clause.toString());
        assertEquals("A-1: 1&2",clause.toNumbers());
        assertEquals("A-1:  p&q",clause.toString(4,symboltable));
       // System.out.println(clause.toString());
    }

    @Test
    public void constructXOR() {
        System.out.println("constructXOR");
        prepare();
        Clause clause = make(1, ClauseType.XOR, 1, 2);
        assertEquals("X-1: 1 x 2",clause.toString());
        assertEquals("X-1: 1 x 2",clause.toNumbers());
        assertEquals("X-1:  p x q",clause.toString(4,symboltable));
        //System.out.println(clause.toString());
    }

    @Test
    public void constructDISJ() {
        System.out.println("constructDISJ");
        prepare();
        Clause clause = make(1, ClauseType.DISJOINT, 1, 2);
        assertEquals("D-1: 1!=2",clause.toString());
        assertEquals("D-1: 1!=2",clause.toNumbers());
        assertEquals("D-1:  p!=q",clause.toString(4,symboltable));
        //System.out.println(clause.toString());
    }

    @Test
    public void constructEQV() {
        System.out.println("constructEQV");
        prepare();
        Clause clause = make(1, ClauseType.EQUIV, 1, 2);
        assertEquals("E-1: 1=2",clause.toString());
        assertEquals("E-1: 1=2",clause.toNumbers());
        assertEquals("E-1:  p=q",clause.toString(4,symboltable));
        //System.out.println(clause.toString());
    }


    @Test
    public void addCLiteral() throws Exception {
        System.out.println("add");
        Clause cl = new Clause(1,type);
        assertEquals(0, cl.size());
        CLiteral lit = new CLiteral(5);
        cl.add(lit);
        assertEquals(1, cl.size());
        CLiteral lit1 = new CLiteral(5);
        cl.add(lit1);
        assertEquals(2, cl.size());
        CLiteral lit2 = new CLiteral(-5);
        cl.add(lit2);
        cl.setStructure();
        assertEquals(3, cl.size());
        assertEquals("1:   5,5,-5",cl.toString(5,null));
        assertEquals(ClauseStructure.MIXED,cl.structure);
        assertTrue(cl.hasDoubles());
        assertTrue(cl.hasComplementaries());
        assertEquals(3,cl.size());
    }


    @Test
    public void clause() throws Exception {
        System.out.println("clause");
        ArrayList<CLiteral> lits = new ArrayList<>();
        lits.add(new CLiteral(1));
        lits.add(new CLiteral(2));
        Clause cl = new Clause(1,type,lits);
        assertEquals("1: 1,2",cl.toString());
    }
    @Test
    public void make() throws Exception {
        System.out.println("make");
        counter = 1;
        Clause c1 = make(1, 2, 3);
        Clause c2 = make(-1, -2, 3);
        Clause c3 = make(-1, -2, -3);
        assertEquals("1: 1,2,3",c1.toString());
        assertEquals("2: -1,-2,3",c2.toString());
        assertEquals("3: -1,-2,-3",c3.toString());
        assertEquals(ClauseStructure.POSITIVE,c1.structure);
        assertEquals(ClauseStructure.MIXED,c2.structure);
        assertEquals(ClauseStructure.NEGATIVE,c3.structure);
    }

    @Test
    public void removeLiteral() throws Exception {
        System.out.println("remove");
        counter = 1;
        Clause c1 = make(1, 2, 3);
        c1.remove(c1.getCLiteral(1));
        assertEquals("1: 1,3",c1.toString());
        c1.remove(c1.getCLiteral(1));
        assertEquals("1: 1",c1.toString());
        c1.remove(c1.getCLiteral(0));
        assertEquals("1: ",c1.toString());
        assertTrue(c1.isEmpty());

        c1 = make(1, 2, 3);
        c1.remove(c1.getCLiteral(2));
        assertEquals("2: 1,2",c1.toString());
        c1.remove(c1.getCLiteral(0));
        assertEquals("2: 2",c1.toString());
    }

    @Test
    public void removeAtPosition() throws Exception {
        System.out.println("removeAtPosition");
        counter = 1;
        Clause c1 = make(1, 2, 3);
        c1.removeAtPosition(1);
        assertEquals("1: 1,3",c1.toString());
        CLiteral lit = c1.getCLiteral(1);
        assertEquals(1,lit.clausePosition);
    }


    @Test
    public void contains() throws Exception {
        System.out.println("getLiteral, contains");
        counter = 1;
        Clause cl = make(5, -6, 7);

        assertEquals(+1, cl.contains(5));
        assertEquals(-1, cl.contains(-5));
        assertEquals(+1, cl.contains(-6));
        assertEquals(+1, cl.contains(7));
        assertEquals(-1, cl.contains(6));

        assertEquals(5,cl.getLiteral(0));
        assertEquals(-6,cl.getLiteral(1));
        assertEquals(7,cl.getLiteral(2));
    }

    @Test
    public void isSubset() {
        System.out.println("isSubset");

        counter = 1;
        Clause cl1 = make(5, -6, 7);
        Clause cl2 = make(5, -6, 7);
        assertTrue(cl1.isSubset(cl2));
        Clause cl3 = make(7,5);
        assertTrue(cl3.isSubset(cl1));
        assertFalse(cl1.isSubset(cl3));
        Clause cl4 = make(5, 6, 7);
        assertFalse(cl1.isSubset(cl4));




    }



    @Test
    public void symboltable() throws Exception {
        System.out.println("symboltable");
        Clause cl1 = new Clause(1,type, 3);
        CLiteral lit1 = new CLiteral(5);
        cl1.add(lit1);
        CLiteral lit2 = new CLiteral(-6);
        cl1.add(lit2);
        CLiteral lit3 = new CLiteral(7);
        cl1.add(lit3);
        Symboltable st = new Symboltable(10);
        st.setName(5, "five");
        st.setName(6, "six");
        st.setName(7, "seven");
        assertEquals("1: five,-six,seven", cl1.toString(0,st));
    }

    @Test
    public void doubles() throws Exception {
        System.out.println("double tautology");
        counter = 1;
        Clause c1 = make(5, -6, -5, -6, -6);
        assertTrue(c1.hasDoubles());
        assertTrue(c1.hasComplementaries());
        assertTrue(c1.removeDoubles());
        assertEquals("1: 5,-6,-5",c1.toString());

    }

    @Test
    public void iterator() throws Exception {
        System.out.println("iterator");
        counter = 1;
        Clause c1 = make(5, -6, -5, -6, -6);
        String st = "";
        for(CLiteral lit : c1) {st += lit.toString();}
        assertEquals("5-6-5-6-6",st);
    }



    GlobalParameters globalParameters = new GlobalParameters();
    Controller controller = new Controller(null,null,null);
    ProblemSupervisor problemSupervisor;
    boolean monitoring = false;
    Model model;
    EquivalenceClasses eqClasses;

    private void prepareEq() {
        globalParameters.monitor = !monitoring ? null : new Monitor(null,"mixed",errors,warnings);
        HashMap<String,Object> problemParameters = new HashMap<>();
        problemParameters.put("name","test");
        problemSupervisor = new ProblemSupervisor(controller,globalParameters,problemParameters,null);
        symboltable = new Symboltable(10);
        symboltable.setName(1,"p");
        symboltable.setName(2,"q");
        symboltable.setName(3,"r");
        symboltable.setName(4,"a");
        symboltable.setName(5,"b");
        symboltable.setName(6,"c");
        model = new Model(10,symboltable);
        problemSupervisor.model = model;
        eqClasses = new EquivalenceClasses(problemSupervisor);
        problemSupervisor.equivalenceClasses = eqClasses;
    }


    }