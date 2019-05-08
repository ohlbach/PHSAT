package Datastructures.Clauses;

import Datastructures.Theory.ImplicationDAG;
import Solvers.RandomWalker.RWModel;
import Utilities.Utilities;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 08.05.2019.
 */
public class IDModelInitializerTest {
    @Test
    public void initializeModel1() throws Exception {
        System.out.println("initializeModel no implications");
        ClauseList clauses = new ClauseList(10, 10);
        clauses.addClause(Utilities.makeClause("1","1,-3"));
        clauses.addClause(Utilities.makeClause("2","1,-3,4"));
        clauses.addClause(Utilities.makeClause("3","-1,3,4"));
        ImplicationDAG id = new ImplicationDAG();
        IDModelInitializer imi = new IDModelInitializer(clauses,id);
        RWModel model = new RWModel(10);
        assertEquals(0,imi.initializeModel(model,0));
        assertEquals("1,-3,4,",model.toString());
    }

    @Test
    public void initializeModel2() throws Exception {
        System.out.println("initializeModel with implications");
        ClauseList clauses = new ClauseList(10, 10);
        clauses.addClause(Utilities.makeClause("1","1,-3"));
        clauses.addClause(Utilities.makeClause("2","1,-3,4"));
        clauses.addClause(Utilities.makeClause("3","-1,3,4"));
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1,3);
        IDModelInitializer imi = new IDModelInitializer(clauses,id);
        RWModel model = new RWModel(10);
        assertEquals(2,imi.initializeModel(model,0));
        assertEquals("-1,-3,4,",model.toString());
        id.addClause(-1,-4);
        model = new RWModel(10);
        model.makeTrue(6);
        assertEquals(4,imi.initializeModel(model,0));
        assertEquals("-1,-3,4,6,",model.toString());
    }


    @Test
    public void getOccurrences1() throws Exception {
        System.out.println("getOccurrences no implications");
        ClauseList clauses = new ClauseList(10, 10);
        clauses.addClause(Utilities.makeClause("1","1,-3"));
        clauses.addClause(Utilities.makeClause("2","1,-3,4"));
        clauses.addClause(Utilities.makeClause("3","-1,3,4"));
        ImplicationDAG id = new ImplicationDAG();
        IDModelInitializer imi = new IDModelInitializer(clauses,id);
        assertEquals(2,imi.getOccurrences(1));
        assertEquals(2,imi.getOccurrences(4));
        assertEquals(1,imi.getOccurrences(3));

    }

    @Test
    public void getOccurrences2() throws Exception {
        System.out.println("getOccurrences with implications");
        ClauseList clauses = new ClauseList(10, 10);
        clauses.addClause(Utilities.makeClause("1","1,-3"));
        clauses.addClause(Utilities.makeClause("2","1,-3,4"));
        clauses.addClause(Utilities.makeClause("3","-1,3,4"));
        ImplicationDAG id = new ImplicationDAG();
        id.addClause(-1,3);
        IDModelInitializer imi = new IDModelInitializer(clauses,id);
        assertEquals(3,imi.getOccurrences(1));
        assertEquals(2,imi.getOccurrences(4));
        assertEquals(3,imi.getOccurrences(-3));
        id.addClause(-1,4);
        assertEquals(3,imi.getOccurrences(1));
    }

}