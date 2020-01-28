package Solvers.Resolution;

import Coordinator.Tasks.TaskQueue;
import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.Clause;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Result;
import Datastructures.Statistics.Statistic;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import Management.Controller;
import Management.ProblemSupervisor;
import Utilities.BucketSortedIndex;
import Utilities.BucketSortedList;
import org.junit.Test;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Random;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 09.12.2019.
 */
public class ResolutionTest {

    static Method getMethod(String name, Class<?>... classes) {
        try {
            Class clazz = Class.forName("Solvers.Resolution.Resolution");
            Method method = clazz.getDeclaredMethod(name, classes);
            method.setAccessible(true);
            return method;
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
        return null;}

    static Field getField(String name) {
        try {
            Class clazz = Class.forName("Solvers.Resolution.Resolution");
            Field field = clazz.getDeclaredField(name);
            field.setAccessible(true);
            return field;
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
        return null;}

    static Field getSField(String name) {
        try {
            Class clazz = Class.forName("Solvers.Solver");
            Field field = clazz.getDeclaredField(name);
            field.setAccessible(true);
            return field;
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
        return null;}




    private static int counter = 1;

    private Clause make(int... literals) {
        Clause cl = new Clause(counter++,literals.length);
        int i = -1;
        for(int l:literals) {
            cl.add(new CLiteral(l,cl,++i));}
        return cl;}


    @Test
    public void parseParameters1() throws Exception {
        System.out.println("Parse Parameters 1");
        HashMap<String,String> parameters = new HashMap<>();
        parameters.put("strategy","POSITIVE");
        parameters.put("seed","10");
        parameters.put("limit", "20");
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        ArrayList<HashMap<String,Object>> pars = Resolution.parseParameters(parameters,errors,warnings);
        assertEquals("[{seed=10, limit=20, name=Resolution_1, strategy=POSITIVE, percentageOfSOSClauses=50}]",pars.toString());
    }
    @Test
    public void parseParameters2() throws Exception {
        System.out.println("Parse Parameters 2");
        HashMap<String,String> parameters = new HashMap<>();
        parameters.put("strategy","POSITIVE, SOS");
        parameters.put("seed","10,20");
        parameters.put("limit", "20 to 22");
        parameters.put("percentageOfSOSClauses","80");
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        ArrayList<HashMap<String,Object>> pars = Resolution.parseParameters(parameters,errors,warnings);
        //System.out.println(pars);
        System.out.println(errors.toString());
        System.out.println(warnings.toString());
        assertEquals("[{seed=10, limit=20, name=Resolution_1, strategy=POSITIVE, percentageOfSOSClauses=80}, {seed=10, limit=20, name=Resolution_2, strategy=SOS, percentageOfSOSClauses=80}, {seed=20, limit=20, name=Resolution_3, strategy=POSITIVE, percentageOfSOSClauses=80}, {seed=20, limit=20, name=Resolution_4, strategy=SOS, percentageOfSOSClauses=80}, {seed=10, limit=21, name=Resolution_5, strategy=POSITIVE, percentageOfSOSClauses=80}, {seed=10, limit=21, name=Resolution_6, strategy=SOS, percentageOfSOSClauses=80}, {seed=20, limit=21, name=Resolution_7, strategy=POSITIVE, percentageOfSOSClauses=80}, {seed=20, limit=21, name=Resolution_8, strategy=SOS, percentageOfSOSClauses=80}, {seed=10, limit=22, name=Resolution_9, strategy=POSITIVE, percentageOfSOSClauses=80}, {seed=10, limit=22, name=Resolution_10, strategy=SOS, percentageOfSOSClauses=80}, {seed=20, limit=22, name=Resolution_11, strategy=POSITIVE, percentageOfSOSClauses=80}, {seed=20, limit=22, name=Resolution_12, strategy=SOS, percentageOfSOSClauses=80}]",pars.toString());
    }



    @Test
    public void help() throws Exception {
        //System.out.println(Resolution.help());

    }

    @Test
    public void isPrimary() throws Exception {
        System.out.println("isPrimary");
        Controller cntr = new Controller(null,null,null);
        HashMap<String,Object> problemParameters = new HashMap<>();
        problemParameters.put("name","test");
        ProblemSupervisor sup = new ProblemSupervisor(cntr,null,problemParameters,null);
        Resolution res = new Resolution(1,null,sup);
        Field strategy = getField("strategy");
        Clause c1 = make(1,2,3);
        Clause c2 = make(-1,-2,-3);
        Clause c3 = make(1,-2,3);
        Method isPrimary = getMethod("isPrimary",Clause.class,boolean.class);

        strategy.set(res,ResolutionStrategy.INPUT);
        assertTrue((boolean)isPrimary.invoke(res,c1,true));
        assertFalse((boolean)isPrimary.invoke(res,c1,false));

        strategy.set(res,ResolutionStrategy.POSITIVE);
        assertTrue((boolean)isPrimary.invoke(res,c1,true));
        assertFalse((boolean)isPrimary.invoke(res,c2,true));
        assertFalse((boolean)isPrimary.invoke(res,c3,true));


        strategy.set(res,ResolutionStrategy.NEGATIVE);
        assertFalse((boolean)isPrimary.invoke(res,c1,true));
        assertTrue((boolean)isPrimary.invoke(res,c2,true));
        assertFalse((boolean)isPrimary.invoke(res,c3,true));

        Field percentageOfSOSClauses = getField("percentageOfSOSClauses");
        Field random = getField("random");
        percentageOfSOSClauses.set(res,99);
        random.set(res,new Random(0));

        strategy.set(res,ResolutionStrategy.SOS);
        assertTrue((boolean)isPrimary.invoke(res,c1,false));

        percentageOfSOSClauses.set(res,100);
        assertTrue((boolean)isPrimary.invoke(res,c1,true));
        percentageOfSOSClauses.set(res,0);
        assertFalse((boolean)isPrimary.invoke(res,c1,true));
    }


    @Test
    public void insertClause() throws Exception {
        counter = 1;
        System.out.println("insertClause");
        Controller cntr = new Controller(null, null, null);
        HashMap<String, Object> problemParameters = new HashMap<>();
        problemParameters.put("name", "test");
        ProblemSupervisor sup = new ProblemSupervisor(cntr, null, problemParameters, null);
        Resolution res = new Resolution(1, null, sup);
        Field statistics = getField("statistics");
        statistics.set(res,new ResolutionStatistics("test"));
        Field primaryClauses = getField("primaryClauses");
        primaryClauses.set(res,new BucketSortedList<Clause>(clause->clause.size()));
        Field secondaryClauses = getField("secondaryClauses");
        secondaryClauses.set(res,new BucketSortedList<Clause>(clause->clause.size()));
        Field literalIndex = getField("literalIndex");
        literalIndex.set(res,new BucketSortedIndex<CLiteral>(10,
                (cLiteral->cLiteral.literal),
                (cLiteral->cLiteral.clause.size())));
        Field taskQueue = getField("taskQueue");
        taskQueue.set(res,new TaskQueue("test",null));

        Method insertClause = getMethod("insertClause", Clause.class, boolean.class,String.class);

        Clause c1 = make(1, 2, 3);
        Clause c2 = make(-1, -2, -3);
        Clause c3 = make(4, -5, 6);
        insertClause.invoke(res,c1,true,"initial");
        insertClause.invoke(res,c2,false,"initial");
        insertClause.invoke(res,c3,false,"initial");
        assertEquals("Bucket 3\n" +
                "  1:(1,2,3)\n",primaryClauses.get(res).toString());
        assertEquals("Bucket 3\n" +
                "  2:(-1,-2,-3)\n" +
                "  3:(4,-5,6)\n",secondaryClauses.get(res).toString());
        assertEquals(" 1: 1,\n" +
                "-1: -1,\n" +
                " 2: 2,\n" +
                "-2: -2,\n" +
                " 3: 3,\n" +
                "-3: -3,\n" +
                " 4: 4,\n" +
                "-5: -5,\n" +
                " 6: 6,\n",literalIndex.get(res).toString());
        Field clauseCounter = getField("clauseCounter");
        assertEquals(3,clauseCounter.get(res));

        Clause c4 = make(7);
        insertClause.invoke(res,c4,true,"initial");
        Clause c5 = make(-7);
        insertClause.invoke(res,c5,true,"initial");
        assertEquals("1. P1: initial: 7\n" +
                "2. P1: initial: -7\n",taskQueue.get(res).toString());
        assertEquals(2,((ResolutionStatistics)statistics.get(res)).derivedUnitClauses);
    }

    @Test
    public void removeClause() throws Exception {
        counter = 1;
        System.out.println("removeClause");
        Controller cntr = new Controller(null, null, null);
        HashMap<String, Object> problemParameters = new HashMap<>();
        problemParameters.put("name", "test");
        ProblemSupervisor sup = new ProblemSupervisor(cntr, null, problemParameters, null);
        Resolution res = new Resolution(1, null, sup);
        Field statistics = getField("statistics");
        statistics.set(res, new ResolutionStatistics("test"));
        Field primaryClauses = getField("primaryClauses");
        primaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field secondaryClauses = getField("secondaryClauses");
        secondaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field literalIndex = getField("literalIndex");
        literalIndex.set(res, new BucketSortedIndex<CLiteral>(10,
                (cLiteral -> cLiteral.literal),
                (cLiteral -> cLiteral.clause.size())));
        Field taskQueue = getField("taskQueue");
        taskQueue.set(res, new TaskQueue("test", null));

        Method insertClause = getMethod("insertClause", Clause.class, boolean.class,String.class);

        Clause c1 = make(1, 2, 3);
        Clause c2 = make(-4, -5, -6);
        Clause c3 = make(7, -5, 6);
        insertClause.invoke(res, c1, true,"initial");
        insertClause.invoke(res, c2, false,"initial");
        insertClause.invoke(res, c3, false,"initial");
        assertEquals(" 1: 1,\n" +
                " 2: 2,\n" +
                " 3: 3,\n" +
                "-4: -4,\n" +
                "-5: -5,-5,\n" +
                " 6: 6,\n" +
                "-6: -6,\n" +
                " 7: 7,\n",literalIndex.get(res).toString());

        Method removeClause = getMethod("removeClause", Clause.class, int.class);

        removeClause.invoke(res,c2,0);
        assertEquals(" 1: 1,\n" +
                " 2: 2,\n" +
                " 3: 3,\n" +
                "-5: -5,\n" +
                " 6: 6,\n" +
                " 7: 7,\n",literalIndex.get(res).toString());

        removeClause.invoke(res,c1,2);
        assertEquals(" 2: 2,\n" +
                "-5: -5,\n" +
                " 6: 6,\n" +
                " 7: 7,\n",literalIndex.get(res).toString());

        assertEquals("Bucket 3\n" +
                "  3:(7,-5,6)\n",secondaryClauses.get(res).toString());
        assertEquals("",primaryClauses.get(res).toString());
    }

    @Test
    public void removeLiteral() throws Exception {
        counter = 1;
        System.out.println("removeLiteral");
        Controller cntr = new Controller(null, null, null);
        HashMap<String, Object> problemParameters = new HashMap<>();
        problemParameters.put("name", "test");
        ProblemSupervisor sup = new ProblemSupervisor(cntr, null, problemParameters, null);
        Resolution res = new Resolution(1, null, sup);
        Field statistics = getField("statistics");
        statistics.set(res, new ResolutionStatistics("test"));
        Field primaryClauses = getField("primaryClauses");
        primaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field secondaryClauses = getField("secondaryClauses");
        secondaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field literalIndex = getField("literalIndex");
        literalIndex.set(res, new BucketSortedIndex<CLiteral>(10,
                (cLiteral -> cLiteral.literal),
                (cLiteral -> cLiteral.clause.size())));
        Field taskQueue = getField("taskQueue");
        taskQueue.set(res, new TaskQueue("test", null));

        Method insertClause = getMethod("insertClause", Clause.class, boolean.class,String.class);

        Clause c1 = make(1, 2, 3);
        Clause c2 = make(-4, -5, -6);
        Clause c3 = make(7, -5, 6);
        insertClause.invoke(res, c1, true,"initial");
        insertClause.invoke(res, c2, false,"initial");
        insertClause.invoke(res, c3, false,"initial");

        Method removeLiteral = getMethod("removeLiteral", CLiteral.class);

        removeLiteral.invoke(res,c1.getCLiteral(1));
        assertEquals("1:(1,3)",c1.toString());
        assertEquals(" 1: 1,\n" +
                " 3: 3,\n" +
                "-4: -4,\n" +
                "-5: -5,-5,\n" +
                " 6: 6,\n" +
                "-6: -6,\n" +
                " 7: 7,\n",literalIndex.get(res).toString());

        removeLiteral.invoke(res,c2.getCLiteral(1));
        assertEquals("2:(-4,-6)",c2.toString());
        assertEquals("3:(7,-5,6)",c3.toString());
        assertEquals(" 1: 1,\n" +
                " 3: 3,\n" +
                "-4: -4,\n" +
                "-5: -5,\n" +
                " 6: 6,\n" +
                "-6: -6,\n" +
                " 7: 7,\n",literalIndex.get(res).toString());
        assertFalse((boolean)removeLiteral.invoke(res,c2.getCLiteral(1)));
        assertEquals("1. P1: New true literal derived: -4\n",taskQueue.get(res).toString());
        assertEquals(1,((ResolutionStatistics)statistics.get(res)).derivedUnitClauses);

        assertEquals("Bucket 2\n" +
                "  1:(1,3)\n",primaryClauses.get(res).toString());
        assertEquals("Bucket 3\n" +
                "  3:(7,-5,6)\n",secondaryClauses.get(res).toString());
    }


    @Test
    public void replaceClause() throws Exception {
        counter = 1;
        System.out.println("replaceClause");
        Controller cntr = new Controller(null, null, null);
        HashMap<String, Object> problemParameters = new HashMap<>();
        problemParameters.put("name", "test");
        ProblemSupervisor sup = new ProblemSupervisor(cntr, null, problemParameters, null);
        Resolution res = new Resolution(1, null, sup);
        Field statistics = getField("statistics");
        statistics.set(res, new ResolutionStatistics("test"));
        Field primaryClauses = getField("primaryClauses");
        primaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field secondaryClauses = getField("secondaryClauses");
        secondaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field literalIndex = getField("literalIndex");
        literalIndex.set(res, new BucketSortedIndex<CLiteral>(10,
                (cLiteral -> cLiteral.literal),
                (cLiteral -> cLiteral.clause.size())));
        Field taskQueue = getField("taskQueue");
        taskQueue.set(res, new TaskQueue("test", null));

        Method insertClause = getMethod("insertClause", Clause.class, boolean.class,String.class);

        Clause c1 = make(1, 2, 3);
        Clause c2 = make(-4, -5, -6);
        Clause c3 = make(1,3);
        insertClause.invoke(res, c1, true,"initial");
        insertClause.invoke(res, c2, false,"initial");
        insertClause.invoke(res, c3, false,"initial");

        Method replaceClause = getMethod("replaceClause", Clause.class, Clause.class);
        replaceClause.invoke(res,c1,c3);
        assertEquals("Bucket 2\n" +
                "  3:(1,3)\n",primaryClauses.get(res).toString());
        assertEquals("Bucket 3\n" +
                "  2:(-4,-5,-6)\n",secondaryClauses.get(res).toString());
    }

    @Test
    public void checkPurity() throws Exception {
        counter = 1;
        System.out.println("checkPurity");
        Controller cntr = new Controller(null, null, null);
        HashMap<String, Object> problemParameters = new HashMap<>();
        problemParameters.put("name", "test");
        ProblemSupervisor sup = new ProblemSupervisor(cntr, null, problemParameters, null);
        Resolution res = new Resolution(1, null, sup);
        Field statistics = getField("statistics");
        statistics.set(res, new ResolutionStatistics("test"));
        Field primaryClauses = getField("primaryClauses");
        primaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field secondaryClauses = getField("secondaryClauses");
        secondaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field literalIndex = getField("literalIndex");
        literalIndex.set(res, new BucketSortedIndex<CLiteral>(10,
                (cLiteral -> cLiteral.literal),
                (cLiteral -> cLiteral.clause.size())));
        Field taskQueue = getField("taskQueue");
        taskQueue.set(res, new TaskQueue("test", null));

        Method insertClause = getMethod("insertClause", Clause.class, boolean.class,String.class);

        Clause c1 = make(1, 2, 3);
        Clause c2 = make(-4, -5, -6);
        Clause c3 = make(1, 3);
        //insertClause.invoke(res, c1, true);
        insertClause.invoke(res, c2, false,"initial");
        insertClause.invoke(res, c3, false,"initial");

        Method checkPurity = getMethod("checkPurity", Clause.class);
        checkPurity.invoke(res,c1);
        assertEquals("1. P1: Pure literal: -2\n",taskQueue.get(res).toString());
    }

    @Test
    public void completeModel() throws Exception {
        counter = 1;
        System.out.println("completeModel");
        Controller cntr = new Controller(null, null, null);
        HashMap<String, Object> problemParameters = new HashMap<>();
        problemParameters.put("name", "test");
        ProblemSupervisor sup = new ProblemSupervisor(cntr, null, problemParameters, null);
        Resolution res = new Resolution(1, null, sup);
        Field statistics = getField("statistics");
        statistics.set(res, new ResolutionStatistics("test"));
        Field primaryClauses = getField("primaryClauses");
        primaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field secondaryClauses = getField("secondaryClauses");
        secondaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field literalIndex = getField("literalIndex");
        literalIndex.set(res, new BucketSortedIndex<CLiteral>(10,
                (cLiteral -> cLiteral.literal),
                (cLiteral -> cLiteral.clause.size())));
        Field taskQueue = getField("taskQueue");
        taskQueue.set(res, new TaskQueue("test", null));

        Field strategy = getField("strategy");
        strategy.set(res,ResolutionStrategy.NEGATIVE);
        Method insertClause = getMethod("insertClause", Clause.class, boolean.class,String.class);

        Field predicates = getSField("predicates");
        predicates.setInt(res,10);

        Clause c1 = make(1, 2, 3);
        Clause c2 = make(-4, -5, 6);
        Clause c3 = make(-5,1, 3);
        ((BucketSortedList<Clause>)secondaryClauses.get(res)).add(c1);
        ((BucketSortedList<Clause>)secondaryClauses.get(res)).add(c2);
        ((BucketSortedList<Clause>)secondaryClauses.get(res)).add(c3);

        Method completeModel = getMethod("completeModel");
        Field model = getSField("model");
        model.set(res, new Model(10));
        assertEquals("Satisfiable with model: [1, 6]",completeModel.invoke(res).toString());

        strategy.set(res,ResolutionStrategy.POSITIVE);
        model.set(res, new Model(10));
        ((BucketSortedList<Clause>)secondaryClauses.get(res)).remove(c1);
        Clause c4 = make(-1, -2, -3);
        ((BucketSortedList<Clause>)secondaryClauses.get(res)).add(c4);
        assertEquals("Satisfiable with model: [-5, -1]",completeModel.invoke(res).toString());


    }

    @Test
    public void processTrueLiteral1() throws Exception {
        counter = 1;
        System.out.println("processTrueLiteral, no propagation");
        Controller cntr = new Controller(null, null, null);
        HashMap<String, Object> problemParameters = new HashMap<>();
        problemParameters.put("name", "test");
        ProblemSupervisor sup = new ProblemSupervisor(cntr, null, problemParameters, null);
        Resolution res = new Resolution(1, null, sup);
        Field statistics = getField("statistics");
        statistics.set(res, new ResolutionStatistics("test"));
        Field primaryClauses = getField("primaryClauses");
        primaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field secondaryClauses = getField("secondaryClauses");
        secondaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field literalIndex = getField("literalIndex");
        literalIndex.set(res, new BucketSortedIndex<CLiteral>(10,
                (cLiteral -> cLiteral.literal),
                (cLiteral -> cLiteral.clause.size())));
        Field taskQueue = getField("taskQueue");
        taskQueue.set(res, new TaskQueue("test", null));

        Method insertClause = getMethod("insertClause", Clause.class, boolean.class, String.class);

        Field predicates = getSField("predicates");
        predicates.setInt(res,10);
        Field symboltable = getSField("symboltable");
        Symboltable stb = new Symboltable(10);
        symboltable.set(res,stb);

        Clause c1 = make(1, 2);    insertClause.invoke(res,c1,true,"initial");
        Clause c2 = make(-2,1,3);  insertClause.invoke(res,c2,false,"initial");
        Clause c3 = make(3,4);     insertClause.invoke(res,c3,true,"initial");
        Clause c4 = make(2, -1,4); insertClause.invoke(res,c4,true,"initial");
        Clause c5 = make(-2,-1,3); insertClause.invoke(res,c5,false,"initial");
        Clause c6 = make(5,-6);    insertClause.invoke(res,c6,true,"initial");

        Field model = getSField("model");
        model.set(res, new Model(10));

        Method processTrueLiteral = getMethod("processTrueLiteral", int.class);
        Result result = (Result)processTrueLiteral.invoke(res,1);

        stb.setName(1,"p");
        stb.setName(4,"q");
        //System.out.println(res.toString(stb));
        assertEquals("Resolution:\n" +
                "Primary Clauses:\n" +
                "Bucket 2\n" +
                "  6:(5,-6)\n" +
                "  3:(3,q)\n" +
                "  4:(2,q)\n" +
                "\n" +
                "Secondary Clauses:\n" +
                "Bucket 2\n" +
                "  5:(-2,3)\n" +
                "\n" +
                "Model:\n" +
                "p\n" +
                "\n" +
                "Literal Index:\n" +
                " 2: 2@4,\n" +
                "-2: -2@5,\n" +
                " 3: 3@3,3@5,\n" +
                " 4: q@3,q@4,\n" +
                " 5: 5@6,\n" +
                "-6: -6@6,\n",res.toString(stb));
    }



    @Test
    public void processTrueLiteral2() throws Exception {
        counter = 1;
        System.out.println("processTrueLiteral, with propagation");
        Controller cntr = new Controller(null, null, null);
        HashMap<String, Object> problemParameters = new HashMap<>();
        problemParameters.put("name", "test");
        ProblemSupervisor sup = new ProblemSupervisor(cntr, null, problemParameters, null);
        Resolution res = new Resolution(1, null, sup);
        Field statistics = getField("statistics");
        statistics.set(res, new ResolutionStatistics("test"));
        Field primaryClauses = getField("primaryClauses");
        primaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field secondaryClauses = getField("secondaryClauses");
        secondaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field literalIndex = getField("literalIndex");
        literalIndex.set(res, new BucketSortedIndex<CLiteral>(10,
                (cLiteral -> cLiteral.literal),
                (cLiteral -> cLiteral.clause.size())));
        Field taskQueue = getField("taskQueue");
        taskQueue.set(res, new TaskQueue("test", null));

        Method insertClause = getMethod("insertClause", Clause.class, boolean.class, String.class);

        Field predicates = getSField("predicates");
        predicates.setInt(res,10);

        Clause c1 = make(1, 2);    insertClause.invoke(res,c1,true,"initial");
        Clause c2 = make(-2,1,3);  insertClause.invoke(res,c2,false,"initial");
        Clause c3 = make(-3,4);     insertClause.invoke(res,c3,true,"initial");
        Clause c4 = make(2, -1); insertClause.invoke(res,c4,true,"initial");
        Clause c5 = make(-2,-1,-3); insertClause.invoke(res,c5,false,"initial");
        Clause c6 = make(-1,-6);    insertClause.invoke(res,c6,true,"initial");
        Clause c7 = make(-5,3);    insertClause.invoke(res,c7,true,"initial");
        Field model = getSField("model");
        model.set(res, new Model(10));

        Method processTrueLiteral = getMethod("processTrueLiteral", int.class);
        Result result = (Result)processTrueLiteral.invoke(res,1);

        //System.out.println(res.toString());
        assertEquals("Resolution:\n" +
                "Primary Clauses:\n" +
                "Bucket 2\n" +
                "  7:(-5,3)\n" +
                "  3:(-3,4)\n" +
                "\n" +
                "Secondary Clauses:\n" +
                "Bucket 2\n" +
                "  5:(-2,-3)\n" +
                "\n" +
                "Model:\n" +
                "[1]\n" +
                "\n" +
                "Literal Index:\n" +
                "-2: -2@5,\n" +
                " 3: 3@7,\n" +
                "-3: -3@3,-3@5,\n" +
                " 4: 4@3,\n" +
                "-5: -5@7,\n" +
                "\n" +
                "Task Queue:\n" +
                "1. P1: New true literal derived: 2\n" +
                "2. P1: New true literal derived: -6\n",res.toString());
    }
    @Test
    public void processTrueLiteral3() throws Exception {
        counter = 1;
        System.out.println("processTrueLiteral, with purity");
        Controller cntr = new Controller(null, null, null);
        HashMap<String, Object> problemParameters = new HashMap<>();
        problemParameters.put("name", "test");
        ProblemSupervisor sup = new ProblemSupervisor(cntr, null, problemParameters, null);
        Resolution res = new Resolution(1, null, sup);
        Field statistics = getField("statistics");
        statistics.set(res, new ResolutionStatistics("test"));
        Field primaryClauses = getField("primaryClauses");
        primaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field secondaryClauses = getField("secondaryClauses");
        secondaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field literalIndex = getField("literalIndex");
        literalIndex.set(res, new BucketSortedIndex<CLiteral>(10,
                (cLiteral -> cLiteral.literal),
                (cLiteral -> cLiteral.clause.size())));
        Field taskQueue = getField("taskQueue");
        taskQueue.set(res, new TaskQueue("test", null));

        Method insertClause = getMethod("insertClause", Clause.class, boolean.class, String.class);

        Field predicates = getSField("predicates");
        predicates.setInt(res,10);

        Clause c1 = make(1, 2);     insertClause.invoke(res,c1,true,"initial");
        Clause c2 = make(-2,1,3);   insertClause.invoke(res,c2,false,"initial");
        Clause c3 = make(-3,4);     insertClause.invoke(res,c3,true,"initial");
        Clause c4 = make(2, -1);    insertClause.invoke(res,c4,true,"initial");
        Clause c5 = make(-2,-1,-3); insertClause.invoke(res,c5,false,"initial");
        Clause c6 = make(-1,-6);    insertClause.invoke(res,c6,true,"initial");
        Field model = getSField("model");
        model.set(res, new Model(10));

        Method processTrueLiteral = getMethod("processTrueLiteral", int.class);
        Result result = (Result)processTrueLiteral.invoke(res,1);

        //System.out.println(res.toString());
        assertEquals("Resolution:\n" +
                "Primary Clauses:\n" +
                "Bucket 2\n" +
                "  3:(-3,4)\n" +
                "\n" +
                "Secondary Clauses:\n" +
                "Bucket 2\n" +
                "  5:(-2,-3)\n" +
                "\n" +
                "Model:\n" +
                "[1]\n" +
                "\n" +
                "Literal Index:\n" +
                "-2: -2@5,\n" +
                "-3: -3@3,-3@5,\n" +
                " 4: 4@3,\n" +
                "\n" +
                "Task Queue:\n" +
                "1. P1: Pure literal: -3\n" +
                "2. P1: New true literal derived: 2\n" +
                "3. P1: New true literal derived: -6\n",res.toString());
    }
    @Test
    public void processTrueLiteral4() throws Exception {
        counter = 1;
        System.out.println("processTrueLiteral, with model completion");
        Controller cntr = new Controller(null, null, null);
        HashMap<String, Object> problemParameters = new HashMap<>();
        problemParameters.put("name", "test");
        ProblemSupervisor sup = new ProblemSupervisor(cntr, null, problemParameters, null);
        Resolution res = new Resolution(1, null, sup);
        Field statistics = getField("statistics");
        statistics.set(res, new ResolutionStatistics("test"));
        Field primaryClauses = getField("primaryClauses");
        primaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field secondaryClauses = getField("secondaryClauses");
        secondaryClauses.set(res, new BucketSortedList<Clause>(clause -> clause.size()));
        Field literalIndex = getField("literalIndex");
        literalIndex.set(res, new BucketSortedIndex<CLiteral>(10,
                (cLiteral -> cLiteral.literal),
                (cLiteral -> cLiteral.clause.size())));
        Field taskQueue = getField("taskQueue");
        taskQueue.set(res, new TaskQueue("test", null));

        Field strategy = getField("strategy");
        strategy.set(res,ResolutionStrategy.POSITIVE);

        Method insertClause = getMethod("insertClause", Clause.class, boolean.class, String.class);

        Field predicates = getSField("predicates");
        predicates.setInt(res,10);

        Clause c1 = make(1, 2);     insertClause.invoke(res,c1,true,"initial");
        Clause c2 = make(-2,1,3);   insertClause.invoke(res,c2,true,"initial");
        Clause c3 = make(-3,4);     insertClause.invoke(res,c3,false,"initial");
        Clause c4 = make(2, -1);    insertClause.invoke(res,c4,true,"initial");
        Clause c5 = make(-2,-1,-3); insertClause.invoke(res,c5,false,"initial");
        Clause c6 = make(-1,-6);    insertClause.invoke(res,c6,true,"initial");
        Field model = getSField("model");
        model.set(res, new Model(10));

        Method processTrueLiteral = getMethod("processTrueLiteral", int.class);
        Result result = (Result)processTrueLiteral.invoke(res,1);

        //System.out.println(res.toString());
        assertEquals("Resolution:\n" +
                "Secondary Clauses:\n" +
                "Bucket 2\n" +
                "  3:(-3,4)\n" +
                "  5:(-2,-3)\n" +
                "\n" +
                "Model:\n" +
                "[1, -3]\n" +
                "\n" +
                "Literal Index:\n" +
                "-2: -2@5,\n" +
                "-3: -3@3,-3@5,\n" +
                " 4: 4@3,\n" +
                "\n" +
                "Task Queue:\n" +
                "1. P1: Pure literal: -3\n" +
                "2. P1: New true literal derived: 2\n" +
                "3. P1: New true literal derived: -6\n",res.toString());
    }

        @Test
    public void initializeData() throws Exception {
            System.out.println("initializeData");
            Controller cntr = new Controller(null, null, null);
            HashMap<String, Object> problemParameters = new HashMap<>();
            problemParameters.put("name", "test");
            HashMap<String, Object> solverParameters = new HashMap<>();
            ArrayList<HashMap<String, Object>> svp = new ArrayList<>();
            solverParameters.put("strategy", ResolutionStrategy.INPUT);
            solverParameters.put("seed", 1);
            solverParameters.put("percentage", 50);
            ProblemSupervisor sup = new ProblemSupervisor(cntr, null, problemParameters, svp);
            Resolution res = new Resolution(1, solverParameters, sup);
            Field predicates = getSField("predicates");
            predicates.set(res, 10);
            Method initializeData = getMethod("initializeData");
            initializeData.invoke(res);
            assertEquals(ResolutionStrategy.INPUT, getField("strategy").get(res));
            assertEquals(ResolutionStatistics.class, getField("statistics").get(res).getClass());
            assertEquals(Random.class, getField("random").get(res).getClass());
            assertEquals(50, getField("percentageOfSOSClauses").get(res));
            assertEquals(BucketSortedList.class, getField("primaryClauses").get(res).getClass());
            assertEquals(BucketSortedList.class, getField("secondaryClauses").get(res).getClass());
            assertEquals(BucketSortedIndex.class, getField("literalIndex").get(res).getClass());
        }

    /* private Result initializeClauses() throws InterruptedException {
        if(basicClauseList.equivalences != null) {
            equivalenceClasses = Transformers.prepareEquivalences(basicClauseList,contradictionHandler);
            if(equivalenceClasses == null) {return null;}}

        Transformers.prepareConjunctions(basicClauseList,equivalenceClasses,
                (literal-> addTrueLiteralTask(literal, "Initial Conjunction: ")));
        if(Thread.interrupted()) {throw new InterruptedException();}
        Transformers.prepareDisjunctions(basicClauseList,equivalenceClasses,insertHandler);
        Transformers.prepareXors     (basicClauseList,equivalenceClasses,insertHandler);
        Transformers.prepareDisjoints(basicClauseList,equivalenceClasses,insertHandler);
        int limit = (int)solverParameters.get("limit");
        resolutionLimit = (limit == Integer.MAX_VALUE) ? limit : limit * clauseCounter;
        initializing = false;
        if(Thread.interrupted()) {throw new InterruptedException();}
        return null;}*/

    @Test
    public void initializeClauses1() throws Exception {
        System.out.println("initializeClauses standard clauses, no equivalences");
        Controller cntr = new Controller(null, null, null);
        HashMap<String, Object> problemParameters = new HashMap<>();
        problemParameters.put("name", "test");
        HashMap<String, Object> solverParameters = new HashMap<>();
        ArrayList<HashMap<String, Object>> svp = new ArrayList<>();
        solverParameters.put("strategy", ResolutionStrategy.INPUT);
        solverParameters.put("seed", 1);
        solverParameters.put("percentage", 50);
        solverParameters.put("limit", 10);
        ProblemSupervisor sup = new ProblemSupervisor(cntr, null, problemParameters, svp);
        Resolution res = new Resolution(1, solverParameters, sup);
        Field predicates = getSField("predicates");
        predicates.set(res, 20);
        Method initializeData = getMethod("initializeData");

        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,0, 1,2};      clauses.addClause(clause1); // or
        int[] clause2 = {2,0, 3,4,-5};   clauses.addClause(clause2);
        int[] clause3 = {3,1, 5,6};      clauses.addClause(clause3);  // and
        int[] clause4 = {4,1, 7,8};      clauses.addClause(clause4);
        int[] clause5 = {5,2, 9,10};     clauses.addClause(clause5);  // xor
        int[] clause6 = {6,2, 11,12};    clauses.addClause(clause6);
        int[] clause7 = {7,3, 13,14};    clauses.addClause(clause7);  // disjoint
        int[] clause8 = {8,3, 15,16,17}; clauses.addClause(clause8);

        Field bcl = getSField("basicClauseList");
        bcl.set(res,clauses);
        initializeData.invoke(res);

        Method initializeClauses = getMethod("initializeClauses");
        initializeClauses.invoke(res);
        //System.out.println(res.toString());
        assertEquals("Resolution:\n" +
                "Primary Clauses:\n" +
                "Bucket 2\n" +
                "  1:(1,2)\n" +
                "  X5:(9,10)\n" +
                "  D5_1:(-9,-10)\n" +
                "  X6:(11,12)\n" +
                "  D6_1:(-11,-12)\n" +
                "  D7_1:(-13,-14)\n" +
                "  D8_1:(-15,-16)\n" +
                "  D8_2:(-15,-17)\n" +
                "  D8_3:(-16,-17)\n" +
                "Bucket 3\n" +
                "  2:(3,4,-5)\n" +
                "\n" +
                "Literal Index:\n" +
                " 1: 1@1,\n" +
                " 2: 2@1,\n" +
                " 3: 3@2,\n" +
                " 4: 4@2,\n" +
                "-5: -5@2,\n" +
                " 9: 9@X5,\n" +
                "-9: -9@D5_1,\n" +
                " 10: 10@X5,\n" +
                "-10: -10@D5_1,\n" +
                " 11: 11@X6,\n" +
                "-11: -11@D6_1,\n" +
                " 12: 12@X6,\n" +
                "-12: -12@D6_1,\n" +
                "-13: -13@D7_1,\n" +
                "-14: -14@D7_1,\n" +
                "-15: -15@D8_1,-15@D8_2,\n" +
                "-16: -16@D8_1,-16@D8_3,\n" +
                "-17: -17@D8_2,-17@D8_3,\n" +
                "\n" +
                "Task Queue:\n" +
                "1. P1: Initial Conjunction: 5\n" +
                "2. P1: Initial Conjunction: 6\n" +
                "3. P1: Initial Conjunction: 7\n" +
                "4. P1: Initial Conjunction: 8\n" +
                "5. P2: Simplify initial clause 2:(3,4,-5)\n" +
                "6. P3: Simplify initial clause 1:(1,2)\n" +
                "7. P3: Simplify initial clause X5:(9,10)\n" +
                "8. P3: Simplify initial clause D5_1:(-9,-10)\n" +
                "9. P3: Simplify initial clause X6:(11,12)\n" +
                "10. P3: Simplify initial clause D6_1:(-11,-12)\n" +
                "11. P3: Simplify initial clause D7_1:(-13,-14)\n" +
                "12. P3: Simplify initial clause D8_1:(-15,-16)\n" +
                "13. P3: Simplify initial clause D8_2:(-15,-17)\n" +
                "14. P3: Simplify initial clause D8_3:(-16,-17)\n",res.toString());
    }

    @Test
    public void initializeClauses2() throws Exception {
        System.out.println("initializeClauses redundant clauses, no equivalences");
        Controller cntr = new Controller(null, null, null);
        HashMap<String, Object> problemParameters = new HashMap<>();
        problemParameters.put("name", "test");
        HashMap<String, Object> solverParameters = new HashMap<>();
        ArrayList<HashMap<String, Object>> svp = new ArrayList<>();
        solverParameters.put("strategy", ResolutionStrategy.INPUT);
        solverParameters.put("seed", 1);
        solverParameters.put("percentage", 50);
        solverParameters.put("limit", 10);
        ProblemSupervisor sup = new ProblemSupervisor(cntr, null, problemParameters, svp);
        Resolution res = new Resolution(1, solverParameters, sup);
        Field predicates = getSField("predicates");
        predicates.set(res, 20);
        Method initializeData = getMethod("initializeData");

        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,0, 1,2,3,-2};     clauses.addClause(clause1); // or
        int[] clause2 = {2,0, 3,4,-5,3,4};  clauses.addClause(clause2);
        int[] clause3 = {3,1, 5,6};          clauses.addClause(clause3);  // and
        int[] clause4 = {4,1, -6,7};         clauses.addClause(clause4);
        int[] clause5 = {5,2, 9,-9};         clauses.addClause(clause5);  // xor
        int[] clause6 = {6,2, 11,12,11};     clauses.addClause(clause6);
        int[] clause7 = {7,3, 13,-13,14};    clauses.addClause(clause7);  // disjoint
        int[] clause8 = {8,3, 15,16,15};     clauses.addClause(clause8);

        Field bcl = getSField("basicClauseList");
        bcl.set(res,clauses);
        initializeData.invoke(res);

        Method initializeClauses = getMethod("initializeClauses");
        initializeClauses.invoke(res);
        //System.out.println(res.toString());
        assertEquals("Resolution:\n" +
                "Primary Clauses:\n" +
                "Bucket 2\n" +
                "  X6:(11,12)\n" +
                "  D6_1:(-11,-12)\n" +
                "  D6_3:(-12,-11)\n" +
                "  D7_1:(-13,-14)\n" +
                "  D7_2:(13,-14)\n" +
                "  D8_1:(-15,-16)\n" +
                "  D8_3:(-16,-15)\n" +
                "Bucket 3\n" +
                "  2:(3,4,-5)\n" +
                "\n" +
                "Literal Index:\n" +
                " 3: 3@2,\n" +
                " 4: 4@2,\n" +
                "-5: -5@2,\n" +
                " 11: 11@X6,\n" +
                "-11: -11@D6_1,-11@D6_3,\n" +
                " 12: 12@X6,\n" +
                "-12: -12@D6_1,-12@D6_3,\n" +
                " 13: 13@D7_2,\n" +
                "-13: -13@D7_1,\n" +
                "-14: -14@D7_1,-14@D7_2,\n" +
                "-15: -15@D8_1,-15@D8_3,\n" +
                "-16: -16@D8_1,-16@D8_3,\n" +
                "\n" +
                "Task Queue:\n" +
                "1. P1: Initial Conjunction: 5\n" +
                "2. P1: Initial Conjunction: 6\n" +
                "3. P1: Initial Conjunction: -6\n" +
                "4. P1: Initial Conjunction: 7\n" +
                "5. P1: Initial clause: -15\n" +
                "6. P1: Initial clause: -11\n" +
                "7. P4: Simplify initial clause 2:(3,4,-5)\n" +
                "8. P5: Simplify initial clause D6_1:(-11,-12)\n" +
                "9. P5: Simplify initial clause D6_3:(-12,-11)\n" +
                "10. P5: Simplify initial clause D7_1:(-13,-14)\n" +
                "11. P5: Simplify initial clause D7_2:(13,-14)\n" +
                "12. P5: Simplify initial clause D8_1:(-15,-16)\n" +
                "13. P5: Simplify initial clause X6:(11,12)\n" +
                "14. P5: Simplify initial clause D8_3:(-16,-15)\n",res.toString());}

    @Test
    public void initializeClauses3() throws Exception {
        System.out.println("initializeClauses redundant clauses, with equivalences");
        Controller cntr = new Controller(null, null, null);
        HashMap<String, Object> problemParameters = new HashMap<>();
        problemParameters.put("name", "test");
        HashMap<String, Object> solverParameters = new HashMap<>();
        ArrayList<HashMap<String, Object>> svp = new ArrayList<>();
        solverParameters.put("strategy", ResolutionStrategy.INPUT);
        solverParameters.put("seed", 1);
        solverParameters.put("percentage", 50);
        solverParameters.put("limit", 10);
        ProblemSupervisor sup = new ProblemSupervisor(cntr, null, problemParameters, svp);
        Resolution res = new Resolution(1, solverParameters, sup);
        Field predicates = getSField("predicates");
        predicates.set(res, 30);
        Method initializeData = getMethod("initializeData");

        BasicClauseList clauses = new BasicClauseList();
        int[] clause1 = {1,0, 1,20,3,-21};   clauses.addClause(clause1); // or
        int[] clause2 = {2,0, 3,21,-5,3,22}; clauses.addClause(clause2);
        int[] clause3 = {3,1, 5,21};         clauses.addClause(clause3);  // and
        int[] clause4 = {4,1, -6,22};        clauses.addClause(clause4);
        int[] clause5 = {5,2, 21,-22};       clauses.addClause(clause5);  // xor
        int[] clause6 = {6,2, 21,12,22};     clauses.addClause(clause6);
        int[] clause7 = {7,3, 22,-21,14};    clauses.addClause(clause7);  // disjoint
        int[] clause8 = {8,3, 22,16,20};     clauses.addClause(clause8);
        int[] clause9 = {9,4, 20,21,22};     clauses.addClause(clause9);  // eqv

        Field bcl = getSField("basicClauseList");
        bcl.set(res,clauses);
        initializeData.invoke(res);

        Method initializeClauses = getMethod("initializeClauses");
        initializeClauses.invoke(res);
        //System.out.println(res.toString());
        assertEquals("Resolution:\n" +
                "Primary Clauses:\n" +
                "Bucket 2\n" +
                "  X6:(20,12)\n" +
                "  D6_1:(-20,-12)\n" +
                "  D6_3:(-12,-20)\n" +
                "  D7_1:(-20,-14)\n" +
                "  D7_2:(20,-14)\n" +
                "  D8_1:(-20,-16)\n" +
                "  D8_3:(-16,-20)\n" +
                "Bucket 3\n" +
                "  2:(3,20,-5)\n" +
                "\n" +
                "Literal Index:\n" +
                " 3: 3@2,\n" +
                "-5: -5@2,\n" +
                " 12: 12@X6,\n" +
                "-12: -12@D6_1,-12@D6_3,\n" +
                "-14: -14@D7_1,-14@D7_2,\n" +
                "-16: -16@D8_1,-16@D8_3,\n" +
                " 20: 20@X6,20@D7_2,20@2,\n" +
                "-20: -20@D6_1,-20@D6_3,-20@D7_1,-20@D8_1,-20@D8_3,\n" +
                "\n" +
                "Task Queue:\n" +
                "1. P1: Initial Conjunction: 5\n" +
                "2. P1: Initial Conjunction: 20\n" +
                "3. P1: Initial Conjunction: -6\n" +
                "4. P1: Initial Conjunction: 20\n" +
                "5. P1: Initial clause: -20\n" +
                "6. P1: Initial clause: -20\n" +
                "7. P4: Simplify initial clause 2:(3,20,-5)\n" +
                "8. P5: Simplify initial clause D6_1:(-20,-12)\n" +
                "9. P5: Simplify initial clause D6_3:(-12,-20)\n" +
                "10. P5: Simplify initial clause D7_1:(-20,-14)\n" +
                "11. P5: Simplify initial clause D7_2:(20,-14)\n" +
                "12. P5: Simplify initial clause D8_1:(-20,-16)\n" +
                "13. P5: Simplify initial clause X6:(20,12)\n" +
                "14. P5: Simplify initial clause D8_3:(-16,-20)\n",res.toString());}

    @Test
    public void resolve() throws Exception {

    }

    @Test
    public void selectParentLiterals() throws Exception {

    }

    @Test
    public void simplifyForward() throws Exception {

    }

    @Test
    public void addTrueLiteralTask() throws Exception {

    }

}