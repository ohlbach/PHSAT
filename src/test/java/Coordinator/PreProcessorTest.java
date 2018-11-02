package Coordinator;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.ClauseType;
import Datastructures.Results.Result;
import Datastructures.Results.Unsatisfiable;
import Management.GlobalParameters;
import Management.ProblemSupervisor;
import org.junit.Test;

import java.util.HashMap;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 02.11.2018.
 */
public class PreProcessorTest {
    static HashMap<String, String> pars = new HashMap<>();
    static StringBuffer errors = new StringBuffer();
    static StringBuffer warnings = new StringBuffer();
    static GlobalParameters glb;
    static HashMap<String,Object> probPars = new HashMap<>();
    static BasicClauseList bcl = new BasicClauseList();
    static ProblemSupervisor psu;
    static PreProcessor prep;

    static void initialize() {
        pars.put("monitor","true");
        glb = new GlobalParameters(pars,errors,warnings);
        probPars.put("name","test");
        bcl.predicates = 10;
        psu = new ProblemSupervisor(1,glb,probPars,null);
        prep = new PreProcessor(psu,probPars,bcl);
    }

    @Test
    public void prepareClauses() throws Exception {

    }

    @Test
    public void addConjunction() throws Exception {
        System.out.println("addConjunction");
        initialize();
        Result result = prep.addConjunction(new int[]{1,ClauseType.AND.ordinal(),1,2,-3});
        assertNull(result);
        assertEquals("[1, 2, -3]",prep.model.toString());

        prep.addConjunction(new int[]{2,ClauseType.AND.ordinal(),1,2,4});
        assertEquals("[1, 2, -3, 4]",prep.model.toString());

        result = prep.addConjunction(new int[]{1,ClauseType.AND.ordinal(),5,6,-5});
        assertTrue(result instanceof Unsatisfiable);

        result = prep.addConjunction(new int[]{1,ClauseType.AND.ordinal(),-1,2});
        assertTrue(result instanceof Unsatisfiable);
        //System.out.println(result);
        //System.out.println(prep.model.toString());
    }

    @Test
    public void addDisjunction() throws Exception {
        System.out.println("addDisjunction");
        initialize();
        Result result = prep.addDisjunction(new int[]{1,ClauseType.OR.ordinal(),1,2,-3});
        assertNull(result);
        assertEquals("1: (1,2,-3)\n",prep.clauses.toString());
        result = prep.addDisjunction(new int[]{2,ClauseType.OR.ordinal(),1,2,3,4});
        assertNull(result);
        assertEquals("1: (1,2,-3)\n" +
                "2: (1,2,4)\n",prep.clauses.toString());
        result = prep.addDisjunction(new int[]{3,ClauseType.OR.ordinal(),1,2,4,5});
        assertNull(result);

        System.out.println("CL\n"+prep.clauses.toString());
        System.out.println(prep.statistics.toString());


    }

    @Test
    public void addXor() throws Exception {

    }

    @Test
    public void addDisjoint() throws Exception {

    }

}