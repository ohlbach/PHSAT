package Generators;

import Datastructures.Clauses.BasicClauseList;
import Management.Controller;
import Management.GlobalParameters;
import Management.ProblemSupervisor;
import org.junit.Test;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import Utilities.Utilities;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 06.09.2018.
 */
public class CNFReaderTest {

    @Test
    public void help()  {
        System.out.println(CNFReader.help());
    }

    @Test
    public void parseProblemParameters1() {
        System.out.println("parseProblemParameters 1");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        HashMap<String,String> parameters = new HashMap<>();
        File file1 = Utilities.writeTmpFile("CNFReader", "test1.cnf", "test1");
        File file2 = Utilities.writeTmpFile("CNFReader", "test2.cnf", "test2");
        parameters.put("file",file1.getAbsolutePath()+","+file2.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        file1.delete();
        file2.delete();
        assertEquals("",errors.toString());
        assertEquals(2,pars.size());
        assertEquals(file1.getAbsolutePath(),pars.get(0).get("file").toString());
        assertEquals(file2.getAbsolutePath(),pars.get(1).get("file").toString());
    }
    @Test
    public void parseProblemParameters2() {
        System.out.println("parseProblemParameters directory");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        HashMap<String,String> parameters = new HashMap<>();
        File file1 = Utilities.writeTmpFile("CNFReader1", "test1.cnf", "test1");
        File file2 = Utilities.writeTmpFile("CNFReader1", "test2.cnf", "test2");
        parameters.put("directory",file1.getParentFile().toString());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        file1.delete();
        file2.delete();
        assertEquals("",errors.toString());
        assertEquals(2,pars.size());
        assertEquals(file1.getAbsolutePath(),pars.get(0).get("file").toString());
        assertEquals(file2.getAbsolutePath(),pars.get(1).get("file").toString());
    }
    @Test
    public void parseProblemParameters3() {
        System.out.println("parseProblemParameters regExpr");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        HashMap<String,String> parameters = new HashMap<>();
        File file1 = Utilities.writeTmpFile("CNFReader2", "test1.cnf", "test1");
        File file2 = Utilities.writeTmpFile("CNFReader2", "test2.cnf", "test2");
        File file3 = Utilities.writeTmpFile("CNFReader2", "dummy.cnf", "dummy");
        parameters.put("directory",file1.getParentFile().toString());
        parameters.put("regExpr","test.*");
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals("",errors.toString());
        assertEquals(2,pars.size());
        assertEquals(file1.getAbsolutePath(),pars.get(0).get("file").toString());
        assertEquals(file2.getAbsolutePath(),pars.get(1).get("file").toString());

        parameters.put("regExpr","du.*");
        pars = CNFReader.parseParameters(parameters,errors,warnings);
        file1.delete();
        file2.delete();
        file3.delete();
        assertEquals("",errors.toString());
        assertEquals(1,pars.size());
        assertEquals(file3.getAbsolutePath(),pars.get(0).get("file").toString());
    }


    private ProblemSupervisor prepare() {
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        Controller controller = new Controller(null,null,null);
        GlobalParameters globalParameters=new GlobalParameters();
        globalParameters.trackReasoning = true;
        HashMap<String,Object> problemParameters = new HashMap<>();
        problemParameters.put("name","test");
        return new ProblemSupervisor(controller,globalParameters,problemParameters,null);}


    @Test
    public void generateFileNotFound() {
        System.out.println("generate file not found");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String cnf = "c test1\n";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        file.delete();
        assertNull(CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings));
        assertEquals("CNFReader: File test.cnf, not found\n",errors.toString());
    }

    @Test
    public void generatePLineMissing() {
        System.out.println("generate p-line missing");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String cnf = "c test1\n" +
                "% comment\n" +
                "q cnf 10 6\n" +
                "1 2 3";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        assertNull(CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings));
        assertEquals("CNFReader: File test.cnf, Line 3: 'q cnf 10 6': p-line missing: 'p cnf predicates clauses'\n",
                errors.toString());
    }

    @Test
    public void generatePLineError() {
        System.out.println("generate p line error");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String cnf = "c test1\n" +
                "% comment\n" +
                "p cng 10 6";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        assertNull(CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings));
        assertEquals("CNFReader: File test.cnf, Line 3: 'p cng 10 6': indicates no cnf file\n",errors.toString());
    }

    @Test
    public void generatePredicateError1(){
        System.out.println("generate predicate error 1");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String cnf = "c test1\n" +
                "% comment\n" +
                "p cnf 10a 6";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        assertNull(CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings));
        assertEquals("CNFReader: File test.cnf, Line 3: 'p cnf 10a 6': '10a' is no integer\n",errors.toString());
    }
    @Test
    public void generatePredicateError2() {
        System.out.println("generate predicate error 2");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String cnf = "c test1\n" +
                "% comment\n" +
                "p cnf -10 6";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        assertNull(CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings));
        assertEquals("CNFReader: File test.cnf, Line 3: 'p cnf -10 6': Negative number of predicates: '-10'\n",errors.toString());
    }

    @Test
    public void generateSymbols()  {
        System.out.println("generate symbol error");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String cnf = "c test1\n" +
                "% comment\n" +
                "p cnf 10 6\n" +
                "p q r 0";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        BasicClauseList bcl = CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings);
        assertEquals("File test.cnf\n" +
                " test1\n" +
                "\n" +
                "Disjunctions:\n" +
                "  1: p,q,r\n",bcl.toString());
    }

    @Test
    public void generateZeroEnd()  {
        System.out.println("generate zero end");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String cnf = "c test1\n" +
                "% comment\n" +
                "p cnf 10 6\n" +
                "1 2 3\n" +
                "4 5 6 0\n" +
                "7 8 9";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        BasicClauseList bcl = CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings);
        assertEquals(
                "CNFReader: File test.cnf, Line 4: '1 2 3': does not end with '0'\n" +
                        "CNFReader: File test.cnf, Line 6: '7 8 9': does not end with '0'\n",errors.toString());
    }

    @Test
    public void generateTooLargePredicate()  {
        System.out.println("generate too large predicate");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String cnf = "c test1\n" +
                "% comment\n" +
                "p cnf 10 6\n" +
                "1 20 3 0\n" +
                "4 5a 6 0\n" +
                "7 8 -11 0\n" +
                "6, 0, 1, 0";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        BasicClauseList bcl = CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings);
        assertEquals(
                "CNFReader: File test.cnf, Line 4: '1 20 3 0': Clause [1, 0, 1, 20, 3]: Predicate 20 is not within the boundaries [1,10]\n" +
                        "CNFReader: File test.cnf, Line 5: '4 5a 6 0': Mixing symbolic an alphanumeric literals is not allowed\n" +
                        "CNFReader: File test.cnf, Line 6: '7 8 -11 0': Clause [2, 0, 7, 8, -11]: Predicate 11 is not within the boundaries [1,10]\n" +
                        "CNFReader: File test.cnf, Line 7: '6, 0, 1, 0': Predicate 0 is not allowed\n",errors.toString());
    }
    @Test
    public void generateSymbolicErrors()  {
        System.out.println("generate symbolic errors");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String cnf = "c test1\n" +
                "% comment\n" +
                "p cnf 5 6\n" +
                "a b c 0\n" +
                "-d -e -a 0\n" +
                "-f -a 0";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        BasicClauseList bcl = CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings);
        assertEquals(
                "CNFReader: File test.cnf, Line 6: '-f -a 0': Number of predicates: 5 is too small for literal 'f'\n",errors.toString());
    }

    @Test
    public void generateNumericErrors()  {
        System.out.println("generate numeric errors");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String cnf = "c test1\n" +
                "% comment\n" +
                "p cnf 10 6\n" +
                ">= 2 a b c 0\n" +
                "<= -3 -d -e -a 0\n" +
                "= -f -a 0";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        BasicClauseList bcl = CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings);
        assertEquals(
                "CNFReader: File test.cnf, Line 5: '<= -3 -d -e -a 0': Clause [2, 5, -3, -4, -5, -1]: Interval boundary: -3 < 0\n" +
                        "CNFReader: File test.cnf, Line 6: '= -f -a 0': '-f' is no integer\n",errors.toString());
    }


    @Test
    public void generateNumeric() {
        System.out.println("generate numeric");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String cnf = "c test1\n" +
                "c test2\n" +
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
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        BasicClauseList bcl = CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings);
        assertEquals("",errors.toString());
        assertEquals("File test.cnf\n" +
                " test1\n" +
                " test2\n" +
                "\n" +
                "Disjunctions:\n" +
                "  1: 1,-5,3\n" +
                "  2: -5,7,1,1,7\n" +
                "  3: 4,5,6\n" +
                "Conjunctions:\n" +
                "A-4: 4&-5\n" +
                "Equivalences:\n" +
                "E-5: 5=2\n" +
                "Quantifieds:\n" +
                "M-6: 2 1,2,3\n" +
                "L-7: 3 4,5,6,7\n" +
                "X-8: 2 -2,-4,5\n" +
                "Intervals:\n" +
                "I-9: 2-4: 3,4,5,6\n", bcl.toString());
        System.out.println(warnings);
    }

    @Test
    public void generateSymbolically() {
        System.out.println("generate symbolically");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String cnf = "c test1\n" +
                "c test2\n" +
                "% test3\n" +
                "p cnf 10 5\n" +
                "-a b 0\n"+
                "-e c d a a 0\n" +
                "e f -b 0\n" +
                "& f -f 0\n" +
                "e g c 0\n" +
                "<= 2 a,b,c 0\n" +
                ">= 3 d,e,f 0\n" +
                "= 2 -a,-b,-c 0";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        BasicClauseList bcl = CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings);
        assertEquals("",errors.toString());
        assertEquals("File test.cnf\n test1\n test2\n",bcl.info);
        assertEquals("File test.cnf\n" +
                " test1\n" +
                " test2\n" +
                "\n" +
                "Disjunctions:\n" +
                "  1: -a,b\n" +
                "  2: -e,c,d,a,a\n" +
                "Conjunctions:\n" +
                "A-4: f&-f\n" +
                "Equivalences:\n" +
                "E-3: f=-b\n" +
                "E-5: g=c\n" +
                "Quantifieds:\n" +
                "M-6: 2 a,b,c\n" +
                "L-7: 3 d,e,f\n" +
                "X-8: 2 -a,-b,-c\n", bcl.toString());
    }


}