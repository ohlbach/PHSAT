package Generators;

import Datastructures.Clauses.BasicClauseList;
import Management.Controller;
import Management.GlobalParameters;
import Management.Monitor;
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
    public void help() throws Exception {
        System.out.println(CNFReader.help());
    }

    @Test
    public void parseProblemParameters1() {
        System.out.println("parseProblemParameters 1");
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
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
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
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
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
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
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        Controller controller = new Controller(null,null,null);
        GlobalParameters globalParameters=new GlobalParameters();
        globalParameters.trackReasoning = true;
        HashMap<String,Object> problemParameters = new HashMap<>();
        problemParameters.put("name","test");
        return  new ProblemSupervisor(controller,globalParameters,problemParameters,null);}


    @Test
    public void generateFileNotFound() {
        System.out.println("generate file not found");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        String cnf = "c test1\n";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        file.delete();
        assertNull(CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings));
        assertEquals("CNFReader: File test.cnf: not found",errors.toString());
    }

    @Test
    public void generatePLineMissing() {
        System.out.println("generate p-line missing");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
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
        assertEquals("CNFReader: File test.cnf: p-line missing: 'p cnf predicates clauses [symbolic]'",
                errors.toString());
    }

    @Test
    public void generatePLineError() {
        System.out.println("generate p line error");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        String cnf = "c test1\n" +
                "% comment\n" +
                "p cng 10 6";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        assertNull(CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings));
        assertEquals("CNFReader: File test.cnf: 'p cng 10 6' indicates no cnf file",errors.toString());
    }

    @Test
    public void generatePredicateError1(){
        System.out.println("generate predicate error 1");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        String cnf = "c test1\n" +
                "% comment\n" +
                "p cnf 10a 6";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        assertNull(CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings));
        assertEquals("CNFReader: File test.cnf: 'p cnf 10a 6' '10a' is no integer",errors.toString());
    }
    @Test
    public void generatePredicateError2() {
        System.out.println("generate predicate error 2");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        String cnf = "c test1\n" +
                "% comment\n" +
                "p cnf -10 6";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        assertNull(CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings));
        assertEquals("CNFReader: File test.cnf: Negative number of predicates: '-10'",errors.toString());
    }
    @Test
    public void generateSymbolic()  {
        System.out.println("generate predicate symbolic error");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        String cnf = "c test1\n" +
                "% comment\n" +
                "p cnf 10 6 sympolic";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        assertNull(CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings));
        assertEquals("CNFReader: File test.cnf: End of line 'p cnf 10 6 sympolic' should be 'symbolic'",errors.toString());
    }

    @Test
    public void generateSymbols()  {
        System.out.println("generate symbol error");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
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
        assertEquals("CNFReader: File test.cnf: line 4: 'p' is no integer\n",errors.toString());
    }

    @Test
    public void generateZeroEnd()  {
        System.out.println("generate zero end");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
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
                "CNFReader: File test.cnf:  line 4: '1 2 3' does not end with '0'\n" +
                "CNFReader: File test.cnf:  line 6: '7 8 9' does not end with '0'\n",errors.toString());
    }

    @Test
    public void generateTooLargePredicate()  {
        System.out.println("generate too large predicate");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
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
                "CNFReader: File test.cnf: line 4: |literal| '20' > 10\n" +
                        "CNFReader: File test.cnf: line 5: '5a' is no integer\n" +
                        "CNFReader: File test.cnf: line 6: |literal| '-11' > 10\n" +
                        "CNFReader: File test.cnf: line 7: literal '0' = 0\n",errors.toString());
    }
    @Test
    public void generateSymbolicErrors()  {
        System.out.println("generate symbolic errors");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        String cnf = "c test1\n" +
                "% comment\n" +
                "p cnf 5 6 symbolic\n" +
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
                "CNFReader: File test.cnf: line 6: predicate overflow: -f\n",errors.toString());
    }

    @Test
    public void generateNumericErrors()  {
        System.out.println("generate numeric errors");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        String cnf = "c test1\n" +
                "% comment\n" +
                "p cnf 10 6 symbolic\n" +
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
                "CNFReader: File test.cnf: line 5: '<= -3 -d -e -a 0' quantifier >= 1 required\n" +
                        "CNFReader: File test.cnf: line 6: '-f' is no integer\n",errors.toString());
    }


    @Test
    public void generateNumeric() {
        System.out.println("generate numeric");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        String cnf = "c test1\n" +
                "c test2\n" +
                "% test3\n" +
                "p cnf 10 5\n" +
                "1 -5 3 0\n"+
                "-5 7 1 1 0\n" +
                "o 4 5 6 0\n" +
                "a 4 -5 0\n" +
                "e 5 2 0\n" +
                "<= 2 1,2,3 0\n" +
                ">= 3 4,5,6,7 0\n" +
                "= 2 -2,-4 5 0";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        BasicClauseList bcl = CNFReader.generate(pars.get(0),problemSupervisor,errors,warnings);
        assertEquals("",errors.toString());
        assertEquals("File test.cnf\n" +
                " test1\n" +
                " test2\n",bcl.info);
        assertEquals("File test.cnf\n"+" test1\n test2\n\n"+
                "Disjunctions:\n" +
                "   1 : 1,-5,3\n" +
                "   2 : -5,7,1,1\n" +
                "   3 : 4,5,6\n" +
                "Conjunctions:\n" +
                "   4 : 4&-5\n" +
                "Equivalences:\n" +
                "   5 : 5=2\n" +
                "Atleast:\n" +
                "   6 : ATLEAST 2 1,2,3\n" +
                "Atmost:\n" +
                "   7 : ATMOST 3 4,5,6,7\n" +
                "Exactly:\n" +
                "   8 : EXACTLY 2 -2,-4,5\n", bcl.toString());

    }

    @Test
    public void generateSymbolically() {
        System.out.println("generate symbolically");

        ProblemSupervisor problemSupervisor = prepare();
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        String cnf = "c test1\n" +
                "c test2\n" +
                "% test3\n" +
                "p cnf 10 5 symbolic\n" +
                "1 -a b 0\n"+
                "-e c d a a 0\n" +
                "o e f -b 0\n" +
                "a f -f 0\n" +
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
        assertEquals("File test.cnf\n"+" test1\n" +
                " test2\n\n"+
                "Disjunctions:\n" +
                "   1 : 1,-a,b\n" +
                "   2 : -e,c,d,a,a\n" +
                "   3 : e,f,-b\n" +
                "Conjunctions:\n" +
                "   4 : f&-f\n" +
                "Equivalences:\n" +
                "   5 : g=c\n" +
                "Atleast:\n" +
                "   6 : ATLEAST 2 a,b,c\n" +
                "Atmost:\n" +
                "   7 : ATMOST 3 d,e,f\n" +
                "Exactly:\n" +
                "   8 : EXACTLY 2 -a,-b,-c\n", bcl.toString());
    }


}