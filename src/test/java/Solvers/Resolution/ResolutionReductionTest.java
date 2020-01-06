package Solvers.Resolution;

import Coordinator.Tasks.TaskQueue;
import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.EquivalenceClasses;
import Datastructures.Theory.Model;
import Management.Controller;
import Management.Monitor;
import Management.ProblemSupervisor;
import Utilities.BucketSortedIndex;
import Utilities.BucketSortedList;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 05.01.2020.
 */
public class ResolutionReductionTest {

    static Controller cntr = new Controller(null,null,null);
    static HashMap<String,Object> problemParameters = new HashMap<>();
    static ProblemSupervisor sup = new ProblemSupervisor(cntr,null,problemParameters,null);
    static {problemParameters.put("name","test");}

    class RRTest extends ResolutionReduction {

        public RRTest() {
            super(1,null,sup);
            predicates = 20;
            literalIndex = new BucketSortedIndex<CLiteral<Clause>>(predicates+1,
                    (cLiteral->cLiteral.literal),
                    (cLiteral->cLiteral.clause.size()));
            statistics = new ResolutionStatistics("test");
            taskQueue = new TaskQueue("test",null);
            equivalenceClasses = new EquivalenceClasses(null,contradictionHandler);
            model = new Model(predicates);
        }

        BucketSortedList<Clause> clauses = new BucketSortedList<Clause>((clause->clause.size()));



        @Override
        void initializeData() {}

        @Override
        Result doTheWork() throws InterruptedException {
            return null;}

        @Override
        BucketSortedList<Clause> getClauseList(Clause clause) {
            return clauses;}

        @Override
        void replaceClause(Clause clause, Clause subsumer) {
            removeClause(clause,0);}

        @Override
        void check(String info) {}

        @Override
        public String toString(Symboltable symboltable) {
            return null;}

        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();

        public void monitor() {

            monitor = new Monitor(null,"mixed",errors,warnings);
            monitoring = true;
        }

        public void addSymboltable() {
            symboltable = new Symboltable(predicates+1);
            for(int i = 1; i <= predicates; ++i) {symboltable.setName(i, "p"+i);}
        }

        public Symboltable st() {return symboltable;}
    }

    private Clause make(int counter, int... literals) {
        Clause cl = new Clause(counter,literals.length);
        int i = -1;
        for(int l:literals) {
            cl.add(new CLiteral(l,cl,++i));}
        return cl;}

    @Test
    public void backwardSubsumption() throws Exception {
        System.out.println("backwardSubsumption");
        RRTest rrtest = new RRTest();
        rrtest.predicates = 10;
        Clause c1 = make(1,1, 2, 3);
        Clause c2 = make(2,-1, -2, -3);
        Clause c3 = make(3,3, 2, 4, 1);
        Clause c4 = make(4,3, 5, 4, 1);
        Clause c5 = make(5,-3, -2);
        Clause c6 = make(6,-3, -2,4,-1);
        Clause c7 = make(7,-2, -3);
        rrtest.insertClause(c1);
        rrtest.insertClause(c2);
        Clause subsumer = rrtest.backwardSubsumption(c3);
        assertNotNull(subsumer);
        assertEquals(c1,subsumer);
        assertNull(rrtest.backwardSubsumption(c4));
        assertNull(rrtest.backwardSubsumption(c5));
        assertEquals(c2,rrtest.backwardSubsumption(c6));
        rrtest.insertClause(c7);
        assertEquals(c7,rrtest.backwardSubsumption(c5));
    }

    @Test
    public void backwardReplacmentResolution() throws Exception {
        System.out.println("backwardReplacementResolution");
        RRTest rrtest = new RRTest();
        rrtest.predicates = 10;
        Clause c1 = make(1, 1, 2, 3, 4);
        Clause c2 = make(2, -1, 2);
        Clause c3 = make(3, -3, 4);
        rrtest.insertClause(c2);
        rrtest.insertClause(c3);
        assertTrue(rrtest.backwardReplacementResolution(c1));
        assertEquals("1:(2,4)",c1.toString());

        Clause c4 = make(4, 10, 11,12);
        Clause c5 = make(5, -11, 10);
        Clause c6 = make(6, -12, 10);
        rrtest.insertClause(c5);
        rrtest.insertClause(c6);
        assertTrue(rrtest.backwardReplacementResolution(c4));
        assertEquals("4:(10)",c4.toString());

        Clause c7 = make(7, 10, -12);
        assertFalse(rrtest.backwardReplacementResolution(c7));
    }

    @Test
    public void forwardSubsumption1() throws Exception {
        System.out.println("forwardSubsumption1");
        RRTest rrtest = new RRTest();
        rrtest.predicates = 10;
        Clause c1 = make(1,1, 2, 3);
        Clause c2 = make(2,-1, -2, -3);
        Clause c3 = make(3,3, 2, 4, 1);
        Clause c4 = make(4,3, 5, 4, 1);
        Clause c5 = make(5,-3, -2);
        Clause c6 = make(6,-3, -2,4,-1);
        Clause c7 = make(7,-2, -3);
        rrtest.insertClause(c2);
        rrtest.insertClause(c3);
        rrtest.insertClause(c4);
        rrtest.insertClause(c5);
        rrtest.insertClause(c6);
        rrtest.forwardSubsumption(c7);
        assertEquals("Bucket 4\n" +
                "  3:(3,2,4,1)\n" +
                "  4:(3,5,4,1)\n",rrtest.clauses.toString());
    }
    @Test
    public void forwardSubsumption2() throws Exception {
        System.out.println("forwardSubsumption2");
        RRTest rrtest = new RRTest();
        rrtest.predicates = 10;
        Clause c1 = make(1,1, 2, 3);
        Clause c2 = make(2,-1, -2, -3);
        Clause c3 = make(3,3, 2, 4, 1);
        Clause c4 = make(4,3, 5, 4, 1);
        Clause c5 = make(5,-3, -2);
        Clause c6 = make(6,-3, -2,4,-1);
        Clause c7 = make(7,-2, -3);
        rrtest.insertClause(c2);
        rrtest.insertClause(c3);
        rrtest.insertClause(c4);
        rrtest.insertClause(c5);
        rrtest.insertClause(c6);
        rrtest.insertClause(c7);
        rrtest.forwardSubsumption(c1);
        assertEquals("Bucket 2\n" +
                "  5:(-3,-2)\n" +
                "  7:(-2,-3)\n" +
                "Bucket 3\n" +
                "  2:(-1,-2,-3)\n" +
                "Bucket 4\n" +
                "  6:(-3,-2,4,-1)\n" +
                "  4:(3,5,4,1)\n",rrtest.clauses.toString());
    }
    @Test
    public void forwardReplacementResolution() throws Exception {
        System.out.println("forwardReplacementResolution");
        RRTest rrtest = new RRTest();
        //rrtest.monitor();
        Clause c1 = make(1,1, 2, 3);
        Clause c2 = make(2,-1, -2, -3);
        Clause c3 = make(3,3, 2, 4, 1);
        Clause c4 = make(4,3, 5, 4, 1);
        Clause c5 = make(5,-3, -2);
        Clause c6 = make(6,-3, -2,4,-1);
        Clause c7 = make(7,-2, 3);
        rrtest.insertClause(c1);
        rrtest.insertClause(c2);
        rrtest.insertClause(c3);
        rrtest.insertClause(c4);
        rrtest.insertClause(c5);
        rrtest.insertClause(c6);
        rrtest.forwardReplacementResolution(c7);
        assertEquals("Bucket 2\n" +
                "  1:(1,3)\n" +
                "  2:(-1,-2)\n" +
                "Bucket 3\n" +
                "  3:(3,4,1)\n" +
                "  6:(-2,4,-1)\n" +
                "Bucket 4\n" +
                "  4:(3,5,4,1)\n",rrtest.clauses.toString());
        rrtest.taskQueue.run();
        assertEquals("Bucket 2\n" +
                "  1:(1,3)\n",rrtest.clauses.toString());
    }

    @Test
    public void processTrueLiteral() throws Exception {
        System.out.println("processTrueLiteral");
        RRTest rrtest = new RRTest();
        rrtest.monitor();
        Clause c1 = make(1, 1, 2, 3);
        Clause c2 = make(2, -1, -2, -3);
        Clause c3 = make(3, 3, 2, 4, 1);
        Clause c4 = make(4, 3, 5, 4, 1);
        Clause c5 = make(5, -3, -2);
        Clause c6 = make(6, -3, -2, 4, -1);
        Clause c7 = make(7, -2, 3);
        rrtest.insertClause(c1);
        rrtest.insertClause(c2);
        rrtest.insertClause(c3);
        rrtest.insertClause(c4);
        rrtest.insertClause(c5);
        rrtest.insertClause(c6);
        rrtest.insertClause(c7);
        rrtest.processTrueLiteral(2);
        assertEquals("Bucket 2\n" +
                "  2:(-1,-3)\n" +
                "Bucket 3\n" +
                "  6:(-3,4,-1)\n" +
                "Bucket 4\n" +
                "  4:(3,5,4,1)\n", rrtest.clauses.toString());
        Result result = rrtest.taskQueue.run();
        assertEquals(Unsatisfiable.class,result.getClass());
        assertEquals("Bucket 3\n" +
                "  4:(5,4,1)\n", rrtest.clauses.toString());
    }


    }