package Datastructures.Clauses;

import Datastructures.LocalModel;
import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.*;

/**
 * Created by Ohlbach on 03.09.2018.
 */
public class BasicClauseListTest {
    @Test
    public void falseClauses() throws Exception {
        System.out.println("falseClauses");
        BasicClauseList clauses = new BasicClauseList(false);
        int[] clause1 = {3,-2,1};
        int[] clause2 = {-3,1,2};
        clauses.disjunctions.add(clause1);
        clauses.disjunctions.add(clause2);
        assertEquals("[3, -2, 1]\n[-3, 1, 2]\n",clauses.toString());

        LocalModel model = new LocalModel(10);
        model.push(-2); model.push(-1); model.push(3);
        ArrayList<int[]> falseClauses = clauses.falseClauses(model);
        assertEquals(1,falseClauses.size());
        assertEquals(clause2,falseClauses.get(0));
       // System.out.println(disjunctions.toString());
    }

}