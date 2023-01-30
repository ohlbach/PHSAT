package ProblemGenerators;

import junit.framework.TestCase;

import java.util.Arrays;

public class ProblemGeneratorTest extends TestCase {

    public void testIsProblemGenerator() {
    }

    public void testGeneratorClass() {
    }

    public void testHelp() {
        //System.out.println(ProblemGenerator.help());
    }

    public void testTestHelp() {
        System.out.println(ProblemGenerator.help("string"));
    }

    public void testMakeProblemGenerator() {
    }

    public void testGetParameters() {
    }

    public void testGenerateProblem() {
    }

    public void testParseClauses() {
    }

    public void testGetFirstLiteral() {
        System.out.println("getFirstLiteral");
        assertEquals("p",ProblemGenerator.getFirstLiteral("p ,q , r"));
        assertEquals("p",ProblemGenerator.getFirstLiteral("& p ,q , r"));
        assertEquals("p",ProblemGenerator.getFirstLiteral("e p ,q , r"));
        assertEquals("p",ProblemGenerator.getFirstLiteral("= 2 p ,q , r"));
        assertEquals("p",ProblemGenerator.getFirstLiteral("<= 2 p ,q , r"));
        assertEquals("p",ProblemGenerator.getFirstLiteral(">= 2 p ,q , r"));
        assertEquals("p",ProblemGenerator.getFirstLiteral("[2,4] p ,q , r"));
        assertNull(ProblemGenerator.getFirstLiteral("[2,4 p ,q , r"));
    }

    public void testParseLine() {
    }

    public void testParseOr() {
        System.out.println("parseOr");

    }

    public void testParseAndEquiv() {
    }

    public void testParseWithQuantification() {
    }

    public void testParseInterval() {
    }
//arseLiterals(String[] clause, int startIndex, int[] inputClause,
//                                         int startIndexClause, Symboltable symboltable, StringBuilder errors) {
    public void testParseLiterals() {
        System.out.println("parseLiterals");
        StringBuilder errors = new StringBuilder();
        String clause = "1,2,3";
        int[] inputClause = new int[5];
        ProblemGenerator.parseLiterals(clause.split("\\s*[, ]\\s*"),
                0,inputClause,2,null,"prefix",errors);
        System.out.println(errors.toString());
        System.out.println(Arrays.toString(inputClause));

    }
}