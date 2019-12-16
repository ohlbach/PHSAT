package Solvers.Resolution;

import Coordinator.Tasks.TaskQueue;
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
        Clause cl = new Clause(Integer.toString(counter++),literals.length);
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
        literalIndex.set(res,new BucketSortedIndex<CLiteral<Clause>>(10,
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
        assertEquals("[Task: New true literal derived: 7, Task: New true literal derived: -7]",taskQueue.get(res).toString());
        assertEquals(2,((ResolutionStatistics)statistics.get(res)).unitClauses);
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
        literalIndex.set(res, new BucketSortedIndex<CLiteral<Clause>>(10,
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
        literalIndex.set(res, new BucketSortedIndex<CLiteral<Clause>>(10,
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
        assertEquals("[Task: New true literal derived: -4]",taskQueue.get(res).toString());
        assertEquals(1,((ResolutionStatistics)statistics.get(res)).unitClauses);

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
        literalIndex.set(res, new BucketSortedIndex<CLiteral<Clause>>(10,
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
        literalIndex.set(res, new BucketSortedIndex<CLiteral<Clause>>(10,
                (cLiteral -> cLiteral.literal),
                (cLiteral -> cLiteral.clause.size())));
        Field taskQueue = getField("taskQueue");
        taskQueue.set(res, new TaskQueue("test", null));

        Method insertClause = getMethod("insertClause", Clause.class, boolean.class,String.class);

        Clause c1 = make(1, 2, 3);
        Clause c2 = make(-4, -5, -6);
        Clause c3 = make(1, 3);
        //insertClause.invoke(res, c1, true);
        insertClause.invoke(res, c2, false,"initial","initial");
        insertClause.invoke(res, c3, false,"initial","initial");

        Method checkPurity = getMethod("checkPurity", Clause.class);
        checkPurity.invoke(res,c1);
        assertEquals("[Task: New true literal derived: -2]",taskQueue.get(res).toString());
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
        literalIndex.set(res, new BucketSortedIndex<CLiteral<Clause>>(10,
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
        literalIndex.set(res, new BucketSortedIndex<CLiteral<Clause>>(10,
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
        literalIndex.set(res, new BucketSortedIndex<CLiteral<Clause>>(10,
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
        Clause c3 = make(-3,4);     insertClause.invoke(res,c3,true,"initial");
        Clause c4 = make(2, -1); insertClause.invoke(res,c4,true,"initial");
        Clause c5 = make(-2,-1,-3); insertClause.invoke(res,c5,false,"initial");
        Clause c6 = make(-1,-6);    insertClause.invoke(res,c6,true,"initial");

        Field model = getSField("model");
        model.set(res, new Model(10));

        Method processTrueLiteral = getMethod("processTrueLiteral", int.class);
        Result result = (Result)processTrueLiteral.invoke(res,1);

        stb.setName(1,"p");
        stb.setName(4,"q");
        System.out.println(res.toString(stb));
        assertEquals("",res.toString(stb));
    }

        @Test
    public void initializeData() throws Exception {

    }

    @Test
    public void initializeClauses() throws Exception {

    }

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