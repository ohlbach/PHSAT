package ProblemGenerators;

import Datastructures.Clauses.Connective;
import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import Utilities.StringIterator;
import Utilities.Utilities;
import junit.framework.TestCase;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;

public class ProblemGeneratorTest extends TestCase {


    public void testHelp() {
        System.out.println(ProblemGenerator.help());
    }

    public void testTestHelp() {
        //System.out.println(ProblemGenerator.help("string"));
    }

    public void testMakeProblemGenerator() {
        System.out.println("makeProblemGenerator");
        ArrayList<HashMap<String,String>> parameters = new ArrayList<>();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();

        HashMap<String,String> params = new HashMap<>();
        params.put("generator","pigeonhole");
        params.put("holes","4");
        params.put("pigeons", "5");
        params.put("capacities", "[1,2], >= 2");
        parameters.add(params);

        params = new HashMap<>();
        params.put("generator","random");
        params.put("predicates","10"); // illegal key, missing predicates
        params.put("lengths","3");
        params.put("ors", "5");
        params.put("intervals", "4");
        params.put("atleasts", "5");
        params.put("precises","true,false");
        parameters.add(params);

        String test = "# test1\n" +
                "# test2\n" +
                "% test3\n" +
                "p cnf 10 5\n" +
                "1 -5 3 0\n"+
                "-5 7 1  1 7 0\n" +
                "4 5 6 0\n" +
                "& 4 -5 0\n" +
                "e 5 2 0\n" +
                "<= 2 1,2,3 0\n" +
                ">= 3 4,5,6,7 0\n" +
                "= 2 -2,-4 5 0\n"+
                "[2,4] 3 4 5 6 0";
        File file = Utilities.writeTmpFile("Generator", "test.cnf", test);

        params = new HashMap<>();
        params.put("generator","file");
        params.put("file",file.getAbsolutePath());
        parameters.add(params);

        ArrayList<ProblemGenerator> generators = ProblemGenerator.makeProblemGenerator(parameters,errors,warnings);
        System.out.println(errors);
        System.out.println(warnings);
        assertEquals(5,generators.size());
        for(ProblemGenerator generator : generators) {
            System.out.println(generator.toString());
            InputClauses inputClauses = generator.generateProblem(null);
            System.out.println(inputClauses.toString());
        }
        file.delete();

    }

    public void testParseClauses() {
        System.out.println("parseClauses");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String clauses = "Info1\n" +
                "Info2\n" +
                "% test\n" +
                "# comment\n" +
                "\n" +
                "p cnf 20\n" +
                "p,q,r\n" +
                "# Info3 \n"+
                "% testttt\n"+
                "& a,b,-c\n" +
                "e r s t\n" +
                "<= 2 -p -q\n" +
                ">= 3 -a -b -c -d\n" +
                "= 2 x y\n" +
                "[2,3] a -b -c -d";
        Iterator iterator = new StringIterator(clauses, "\n");
        InputClauses inputClauses = ProblemGenerator.parseClauses("Myproblem",iterator,errors,warnings);
        System.out.println(errors.toString());
        //System.out.println(inputClauses.toString());
        assertEquals("Problem Myproblem\n" +
                "Info1\n" +
                "Info2\n" +
                " comment\n" +
                " Info3\n" +
                "\n" +
                "Disjunctions:\n" +
                "      1: p,q,r\n" +
                "Conjunctions:\n" +
                "    A-2: a&b&-c\n" +
                "Equivalences:\n" +
                "    E-3: r=s=t\n" +
                "Atleasts:\n" +
                "    L-5: >= 3 -a,-b,-c,-d\n" +
                "Atmosts:\n" +
                "    M-4: <= 2 -p,-q\n" +
                "Exactlys:\n" +
                "    X-6: = 2 x,y\n" +
                "Intervals:\n" +
                "    I-7: 2-3: a,-b,-c,-d\n",inputClauses.toString());


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
        System.out.println("parseLine");
        StringBuilder errors = new StringBuilder();
        String line = "1,-2 3";
        int[] clause = ProblemGenerator.parseLine(line,10,null,"prefix: ",errors);
        //System.out.println(errors.toString());
        assertEquals("[10, 0, 1, -2, 3]",Arrays.toString(clause));

        Symboltable symboltable = new Symboltable(5);
        line = "p,-q 3r 0";
        clause = ProblemGenerator.parseLine(line,11,symboltable,"prefix: ",errors);
        assertEquals("[11, 0, 1, -2, 3]",Arrays.toString(clause));

        line = "& 1,-2 3";
        clause = ProblemGenerator.parseLine(line,10,null,"prefix: ",errors);
        //System.out.println(errors.toString());
        assertEquals("[10, 1, 1, -2, 3]",Arrays.toString(clause));

        line = "& p,-q 3r 0";
        clause = ProblemGenerator.parseLine(line,11,symboltable,"prefix: ",errors);
        assertEquals("[11, 1, 1, -2, 3]",Arrays.toString(clause));

        line = "e 1,-2 3 0";
        clause = ProblemGenerator.parseLine(line,10,null,"prefix: ",errors);
        //System.out.println(errors.toString());
        assertEquals("[10, 2, 1, -2, 3]",Arrays.toString(clause));

        line = "e p,-q 3r";
        clause = ProblemGenerator.parseLine(line,11,symboltable,"prefix: ",errors);
        assertEquals("[11, 2, 1, -2, 3]",Arrays.toString(clause));

        line = "[2,3] 1,-2 3 0";
        clause = ProblemGenerator.parseLine(line,10,null,"prefix: ",errors);
        //System.out.println(errors.toString());
        assertEquals("[10, 3, 2, 3, 1, -2, 3]",Arrays.toString(clause));

        line = "[2,3] p,-q 3r";
        clause = ProblemGenerator.parseLine(line,11,symboltable,"prefix: ",errors);
        assertEquals("[11, 3, 2, 3, 1, -2, 3]",Arrays.toString(clause));

        line = ">= 2 1,-2 3 0";
        clause = ProblemGenerator.parseLine(line,10,null,"prefix: ",errors);
        //System.out.println(errors.toString());
        assertEquals("[10, 4, 2, 1, -2, 3]",Arrays.toString(clause));

        line = ">= 2 p,-q 3r";
        clause = ProblemGenerator.parseLine(line,11,symboltable,"prefix: ",errors);
        assertEquals("[11, 4, 2, 1, -2, 3]",Arrays.toString(clause));

        line = "<= 2 1,-2 3 0";
        clause = ProblemGenerator.parseLine(line,10,null,"prefix: ",errors);
        //System.out.println(errors.toString());
        assertEquals("[10, 5, 2, 1, -2, 3]",Arrays.toString(clause));

        line = "<= 2 p,-q 3r";
        clause = ProblemGenerator.parseLine(line,11,symboltable,"prefix: ",errors);
        assertEquals("[11, 5, 2, 1, -2, 3]",Arrays.toString(clause));

        line = "= 2 1,-2 3";
        clause = ProblemGenerator.parseLine(line,10,null,"prefix: ",errors);
        //System.out.println(errors.toString());
        assertEquals("[10, 6, 2, 1, -2, 3]",Arrays.toString(clause));

        line = "= 2 p,-q 3r 0";
        clause = ProblemGenerator.parseLine(line,11,symboltable,"prefix: ",errors);
        assertEquals("[11, 6, 2, 1, -2, 3]",Arrays.toString(clause));


    }

    public void testParseOr() {
        System.out.println("parseOr");
        StringBuilder errors = new StringBuilder();
        String line = "1,-2 3";
        int[] clause = ProblemGenerator.parseOr(line,10,null,"prefix: ",errors);
        //System.out.println(errors.toString());
        assertEquals("[10, 0, 1, -2, 3]",Arrays.toString(clause));

        Symboltable symboltable = new Symboltable(5);
        line = "p,-q 3r";
        clause = ProblemGenerator.parseOr(line,10,symboltable,"prefix: ",errors);
        assertEquals("[10, 0, 1, -2, 3]",Arrays.toString(clause));
    }

    public void testParseAndEquiv() {
        System.out.println("parseAndEquiv");
        StringBuilder errors = new StringBuilder();
        String line = "& 1,-2 3";
        int[] clause = ProblemGenerator.parseAndEquiv(line,10, Connective.AND,null,"prefix: ",errors);
        //System.out.println(errors.toString());
        assertEquals("[10, 1, 1, -2, 3]",Arrays.toString(clause));

        Symboltable symboltable = new Symboltable(5);
        line = "& p,-q 3r";
        clause = ProblemGenerator.parseAndEquiv(line,10,Connective.AND,symboltable,"prefix: ",errors);
        assertEquals("[10, 1, 1, -2, 3]",Arrays.toString(clause));

        line = "e 1,-2 3";
        clause = ProblemGenerator.parseAndEquiv(line,10, Connective.EQUIV,null,"prefix: ",errors);
        //System.out.println(errors.toString());
        assertEquals("[10, 2, 1, -2, 3]",Arrays.toString(clause));

        line = "e p,-q r";
        clause = ProblemGenerator.parseAndEquiv(line,10,Connective.EQUIV,symboltable,"prefix: ",errors);
        assertEquals("[10, 2, 1, -2, 4]",Arrays.toString(clause));
    }

    public void testParseWithQuantification() {
        System.out.println("parseWithQuantification");
        StringBuilder errors = new StringBuilder();
        String line = "<= 2 1,-2 3";
        int[] clause = ProblemGenerator.parseWithQuantification(line,10, Connective.ATLEAST,null,"prefix: ",errors);
        //System.out.println(errors.toString());
        assertEquals("[10, 4, 2, 1, -2, 3]",Arrays.toString(clause));

        Symboltable symboltable = new Symboltable(5);
        line = "<= 2 p,-q 3r";
        clause = ProblemGenerator.parseWithQuantification(line,11,Connective.ATLEAST,symboltable,"prefix: ",errors);
        assertEquals("[11, 4, 2, 1, -2, 3]",Arrays.toString(clause));

        line = ">= 2 1,-2 3";
        clause = ProblemGenerator.parseWithQuantification(line,10, Connective.ATMOST,null,"prefix: ",errors);
        System.out.println(errors.toString());
        assertEquals("[10, 5, 2, 1, -2, 3]",Arrays.toString(clause));

        line = ">= 2 p,-q 3r";
        clause = ProblemGenerator.parseWithQuantification(line,11,Connective.ATMOST,symboltable,"prefix: ",errors);
        assertEquals("[11, 5, 2, 1, -2, 3]",Arrays.toString(clause));

        line = "= 2 1,-2 3";
        clause = ProblemGenerator.parseWithQuantification(line,10, Connective.EXACTLY,null,"prefix: ",errors);
        System.out.println(errors.toString());
        assertEquals("[10, 6, 2, 1, -2, 3]",Arrays.toString(clause));

        line = "= 2 p,-q 3r";
        clause = ProblemGenerator.parseWithQuantification(line,11,Connective.EXACTLY,symboltable,"prefix: ",errors);
        assertEquals("[11, 6, 2, 1, -2, 3]",Arrays.toString(clause));


    }

    public void testParseInterval() {
        System.out.println("parseInterval");
        StringBuilder errors = new StringBuilder();
        String line = "[2,3] 1,-2 3";
        int[] clause = ProblemGenerator.parseInterval(line,10, null,"prefix: ",errors);
        //System.out.println(errors.toString());
        assertEquals("[10, 3, 2, 3, 1, -2, 3]",Arrays.toString(clause));

        Symboltable symboltable = new Symboltable(5);
        line = "[2,3] p,-q 3r";
        clause = ProblemGenerator.parseInterval(line,11,symboltable,"prefix: ",errors);
        assertEquals("[11, 3, 2, 3, 1, -2, 3]",Arrays.toString(clause));
    }

    public void testParseLiterals() {
        System.out.println("parseLiterals");
        StringBuilder errors = new StringBuilder();
        String clause = "1,2,3";
        int[] inputClause = new int[5];
        ProblemGenerator.parseLiterals(clause.split("\\s*[, ]\\s*"),
                0,inputClause,2,null,"prefix: ",errors);
        //System.out.println(errors.toString());
        assertEquals("[0, 0, 1, 2, 3]",Arrays.toString(inputClause));

        Symboltable symboltable = new Symboltable(5);
        clause = "p,-q,r";
        inputClause = new int[5];
        ProblemGenerator.parseLiterals(clause.split("\\s*[, ]\\s*"),
                0,inputClause,2,symboltable,"prefix: ",errors);
        //System.out.println(errors.toString());
        assertEquals("[0, 0, 1, -2, 3]",Arrays.toString(inputClause));
    }
}