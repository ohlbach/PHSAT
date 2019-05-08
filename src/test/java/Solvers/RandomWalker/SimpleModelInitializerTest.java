package Solvers.RandomWalker;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Utilities.Utilities;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 07.05.2019.
 */
public class SimpleModelInitializerTest {
    @Test
    public void initializeModel() throws Exception {
        ClauseList clauses = new ClauseList(10, 10);
        clauses.addClause(Utilities.makeClause("1","1,-3"));
        clauses.addClause(Utilities.makeClause("2","1,-3,4"));
        clauses.addClause(Utilities.makeClause("3","-1,3,4"));
        SimpleModelInitializer smi = new SimpleModelInitializer(clauses);
        RWModel model = new RWModel(10);
        smi.initializeModel(model);
        assertEquals("1,-3,4,",model.toString());
    }

}