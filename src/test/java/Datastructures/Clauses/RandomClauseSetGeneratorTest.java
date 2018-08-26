package Datastructures.Clauses;

import Datastructures.Literals.LiteralGenerator;
import Datastructures.Status;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 26.08.2018.
 */
public class RandomClauseSetGeneratorTest {
    @Test
    public void generate1() throws Exception {
        System.out.println("generate");
        LiteralGenerator lg = new LiteralGenerator();
        ClauseGenerator cg = new ClauseGenerator(lg);
        ClauseSetGenerator csg = new RandomClauseSetGenerator(cg,1,10,10,3,false);
        Status st = csg.generate();
        System.out.println(st.clauseList);
        csg = new RandomClauseSetGenerator(cg,1,10,10,3,true);
        st = csg.generate();
        System.out.println(st.clauseList);
        System.out.println(st.seed);
        System.out.println(st.unsatisfiable);
        System.out.println(st.toBeExamined);

        csg = new RandomClauseSetGenerator(cg,2,10,10,2,false);
        st = csg.generate();
        System.out.println(st.clauseList);
        System.out.println(st.unsatisfiable);
    }

}