package Datastructures.Clauses;

import Datastructures.Status;
import Generators.StringClauseSetGenerator;
import org.junit.Test;

/**
 * Created by ohlbach on 27.08.2018.
 */
public class StringClauseSetGeneratorTest {
    @Test
    public void generate1() throws Exception {
        System.out.println("generate 1");
        String clauses =
                "p,q\n" +
                "-p,q";
        StringClauseSetGenerator cg = new StringClauseSetGenerator(clauses);
        Status status = cg.generate();
        System.out.println(status.clauseList);
    }

    @Test
    public void generate2() throws Exception {
        System.out.println("generate 2");
        String clauses =
                "p\n"+
                "p, qq, -rr  \n" +
                "-p,q,-p";
        StringClauseSetGenerator cg = new StringClauseSetGenerator(clauses);
        Status status = cg.generate();
        System.out.println(status.clauseList.toString(false));
    }

}