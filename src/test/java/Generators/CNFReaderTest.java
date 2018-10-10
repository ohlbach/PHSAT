package Generators;

import Datastructures.Clauses.BasicClauseList;
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
        //System.out.println(CNFReader.help());
    }

    @Test
    public void parseProblemParameters1() throws Exception {
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
    public void parseProblemParameters2() throws Exception {
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
    public void parseProblemParameters3() throws Exception {
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





    @Test
    public void generate1() throws Exception {
        System.out.println("generate disjunctions");
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        String cnf = "c test1\n" +
                "c test2\n" +
                "p cnf 10 5\n" +
                "1 -5 3 0\n"+
                "-5 7 1 1 0\n" +
                "4 5 6 0\n" +
                "4 0\n" +
                "5 0";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader3", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        BasicClauseList bcl = CNFReader.generate(pars.get(0),errors,warnings);
        System.out.println(file.delete());
        assertEquals("",errors.toString());
        assertEquals(" test1\n" +
                " test2\n" +
                "\n" +
                "Disjunctions:\n" +
                "1 o: 1,-5,3\n" +
                "2 o: -5,7,1,1\n" +
                "3 o: 4,5,6\n" +
                "4 o: 4\n" +
                "5 o: 5\n",bcl.toString());
        assertEquals(" test1\n test2\n",bcl.info);

    }

    @Test
    public void generate2() throws Exception {
        System.out.println("generate mixed");
        StringBuffer errors = new StringBuffer();
        StringBuffer warnings = new StringBuffer();
        String cnf = "c test1\n" +
                "c test2\n" +
                "p cnf 10 5\n" +
                "1 -5 3 0\n"+
                "-5 7 1 1 0\n" +
                "a 4 5 6 0\n" +
                "x 4 0\n" +
                "d 1 2 3 0\n" +
                "e 1 2 3 0";
        HashMap<String,String> parameters = new HashMap<>();
        File file = Utilities.writeTmpFile("CNFReader4", "test.cnf", cnf);
        parameters.put("file",file.getAbsolutePath());
        ArrayList<HashMap<String,Object>> pars = CNFReader.parseParameters(parameters,errors,warnings);
        assertEquals(1,pars.size());
        BasicClauseList bcl =  CNFReader.generate(pars.get(0),errors,warnings);
        file.delete();
        assertEquals("",errors.toString());
        //System.out.println(pars.get(0).get("disjunctions"));
        assertEquals(" test1\n" +
                " test2\n" +
                "\n" +
                "Disjunctions:\n" +
                "1 o: 1,-5,3\n" +
                "2 o: -5,7,1,1\n" +
                "Conjunctions:\n" +
                "3 a: 4,5,6\n" +
                "Xor:\n" +
                "4 x: 4\n" +
                "Disjoints:\n" +
                "5 d: 1,2,3\n" +
                "Equivalences:\n" +
                "6 e: 1,2,3\n",bcl.toString());
        assertEquals(" test1\n test2\n",bcl.info);

    }

}