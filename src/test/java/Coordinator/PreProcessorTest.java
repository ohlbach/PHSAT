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
    @Test
    public void prepareClauses() throws Exception {

    }

    @Test
    public void addConjunction() throws Exception {
        System.out.println("addConjunction");
        HashMap<String, String> pars = new HashMap<>();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        GlobalParameters glb = new GlobalParameters(pars,errors,warnings);
        HashMap<String,Object> probPars = new HashMap<>();
        probPars.put("name","test");
        BasicClauseList bcl = new BasicClauseList(); bcl.predicates = 10;
        ProblemSupervisor psu = new ProblemSupervisor(1,glb,probPars,null);
        PreProcessor prep = new PreProcessor(psu,probPars,bcl);
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

    }

    @Test
    public void addXor() throws Exception {

    }

    @Test
    public void addDisjoint() throws Exception {

    }

}