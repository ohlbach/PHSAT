package Datastructures.Clauses;

import Datastructures.Model;
import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 26.08.2018.
 */
public class ClauseSetGeneratorTest {
    @Test
    public void simplify() throws Exception {
        System.out.println("simplify");
        Model model = new Model(10);
        ArrayList<Integer> literals = new ArrayList<>();
        literals.add(3); literals.add(5);literals.add(-2);
        int status = ClauseSetGenerator.simplify(literals,model);
        assertEquals(0,status);
        assertEquals("[3, 5, -2]",literals.toString());

        model.push(5);
        status = ClauseSetGenerator.simplify(literals,model);
        assertEquals(1,status);
        assertEquals("[3, 5, -2]",literals.toString());

        model.pop();
        model.push(-3);
        status = ClauseSetGenerator.simplify(literals,model);
        assertEquals(0,status);
        assertEquals("[5, -2]",literals.toString());

        model.push(-5);model.push(2);
        status = ClauseSetGenerator.simplify(literals,model);
        assertEquals(-1,status);
        assertEquals("[]",literals.toString());


    }

}