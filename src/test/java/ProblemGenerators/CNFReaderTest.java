package ProblemGenerators;

import Datastructures.Clauses.InputClauses;
import Utilities.Utilities;
import org.junit.Test;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

import static org.junit.Assert.assertEquals;

/**
 * Created by ohlbach on 06.09.2018.
 */
public class CNFReaderTest {

    @Test
    public void help()  {
        //System.out.println(CNFReader.help());
    }
//  public static void makeProblemGenerator(HashMap<String,String> parameters, GlobalParameters globalParameters,
//                         ArrayList<ProblemGenerator> generators,
//                         StringBuilder errors, StringBuilder warnings){
//
    @Test
    public void makeProblemGeneratorFile() {
        System.out.println("makeProblemGenerator Single File");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        ArrayList<ProblemGenerator> generators = new ArrayList<>();
        HashMap<String,String> parameters = new HashMap<>();
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
        File file = Utilities.writeTmpFile("CNFReader", "test.cnf", test);
        parameters.put("file",file.getAbsolutePath());
        CNFReader.makeProblemGenerator(parameters,generators,errors,warnings);
        //System.out.println(errors);
        //System.out.println(warnings);
        assertEquals(1,generators.size());
        ProblemGenerator generator = generators.get(0);
        assertEquals("CNFReader for file "+file.getAbsolutePath(),generator.toString());
        InputClauses inputClauses = generator.generateProblem(null);
        assertEquals("Problem test.cnf\n" +
                " test1\n" +
                " test2\n" +
                "\n" +
                "Disjunctions:\n" +
                "      1: 1,-5,3\n" +
                "      2: -5,7,1,1,7\n" +
                "      3: 4,5,6\n" +
                "Conjunctions:\n" +
                "    A-4: 4&-5\n" +
                "Equivalences:\n" +
                "    E-5: 5=2\n" +
                "Atleasts:\n" +
                "    L-7: >= 3 4,5,6,7\n" +
                "Atmosts:\n" +
                "    M-6: <= 2 1,2,3\n" +
                "Exactlys:\n" +
                "    X-8: = 2 -2,-4,5\n" +
                "Intervals:\n" +
                "    I-9: 2-4: 3,4,5,6\n",inputClauses.toString());
        //System.out.println(inputClauses.toString());
        file.delete();}

        @Test
        public void makeProblemGeneratorFiles() {
            System.out.println("makeProblemGenerator Two Files");
            StringBuilder errors = new StringBuilder();
            StringBuilder warnings = new StringBuilder();
            ArrayList<ProblemGenerator> generators = new ArrayList<>();
            HashMap<String,String> parameters = new HashMap<>();
            String test1 = "# test1\n" +
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
            File file1 = Utilities.writeTmpFile("CNFReader", "test1.cnf", test1);

            String test2 = "# test2\n" +
                    "% comment\n" +
                    "p cnf 10 6\n" +
                    "1 2 3 0";
            File file2 = Utilities.writeTmpFile("CNFReader", "test2.cnf", test2);

            parameters.put("file",file1.getAbsolutePath()+","+file2.getAbsolutePath());
            CNFReader.makeProblemGenerator(parameters,generators,errors,warnings);
            assertEquals(2,generators.size());
            //System.out.println(errors);
            //System.out.println(warnings);
            ProblemGenerator generator1 = generators.get(0);
            ProblemGenerator generator2 = generators.get(1);

            InputClauses inputClauses1 = generator1.generateProblem(null);
            InputClauses inputClauses2 = generator2.generateProblem(null);

            assertEquals("Problem test2.cnf\n" +
                    " test2\n" +
                    "\n" +
                    "Disjunctions:\n" +
                    "      1: 1,2,3\n",inputClauses2.toString());

            file1.delete();
            file2.delete();

    }

    @Test
    public void makeProblemGeneratorDirectory() {
        System.out.println("makeProblemGenerator Directory");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        ArrayList<ProblemGenerator> generators = new ArrayList<>();
        HashMap<String,String> parameters = new HashMap<>();
        String test1 = "# test1\n" +
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
        File file1 = Utilities.writeTmpFile("CNFReader1", "test1.cnf", test1);

        String test2 = "# test2\n" +
                "% comment\n" +
                "p cnf 10 6\n" +
                "1 2 3";
        File file2 = Utilities.writeTmpFile("CNFReader1", "test2.cnf", test2);
        parameters.put("directory",file1.getParent());
        CNFReader.makeProblemGenerator(parameters,generators,errors,warnings);
        //System.out.println(errors);
        //System.out.println(warnings);
        assertEquals(2,generators.size());
        ProblemGenerator generator1 = generators.get(0);
        ProblemGenerator generator2 = generators.get(1);

        InputClauses inputClauses1 = generator1.generateProblem(null);
        InputClauses inputClauses2 = generator2.generateProblem(null);
        assertEquals("Problem test2.cnf\n" +
                " test2\n" +
                "\n" +
                "Disjunctions:\n" +
                "      1: 1,2,3\n",inputClauses2.toString());
        file1.delete();
        file2.delete();
    }

    @Test
    public void makeProblemGeneratorRegularExpression() {
        System.out.println("makeProblemGenerator Regular Expression");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        ArrayList<ProblemGenerator> generators = new ArrayList<>();
        HashMap<String,String> parameters = new HashMap<>();
        String test1 = "# test1\n" +
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
        File file1 = Utilities.writeTmpFile("CNFReader2", "test1.cnf", test1);

        String test2 = "# test2\n" +
                "% comment\n" +
                "p cnf 10 6\n" +
                "1 2 3";
        File file2 = Utilities.writeTmpFile("CNFReader2", "test2.cnf", test2);
        parameters.put("directory",file1.getParent());
        parameters.put("regExpr",".*2.*");
        CNFReader.makeProblemGenerator(parameters,generators,errors,warnings);
        //System.out.println(errors);
        //System.out.println(warnings);
        assertEquals(1,generators.size());
        ProblemGenerator generator1 = generators.get(0);
        InputClauses inputClauses1 = generator1.generateProblem(null);
        assertEquals("Problem test2.cnf\n" +
                " test2\n" +
                "\n" +
                "Disjunctions:\n" +
                "      1: 1,2,3\n",inputClauses1.toString());
        file1.delete();
        file2.delete();
    }
}