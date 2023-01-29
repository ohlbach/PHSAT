package Utilities;

import org.junit.Test;

import java.io.File;
import java.nio.file.Paths;

import static org.junit.Assert.assertEquals;

/**
 * Created by Ohlbach on 03.09.2018.
 */
public class KVParserTest {
    @Test
    public void parseStream() throws Exception {

    }

    @Test
    public void parseString() throws Exception {
        System.out.println("parseString");
        KVParser parser = new KVParser("global","problem","series");
        String test ="info 1\n" +
                "\n" +
                "info 2" +
                "\n" +
                "global glob1 // test1\n" +
                "gkey1\n" +
                "gkey2 = 5 a\n" +
                "problem\n" +
                "\n" +
                "pkey1 : 33\n" +
                "global glob2\n" +
                "gkey3 55 // test2";
        parser.parseString(test);
        //System.out.println(parser);
        assertEquals("info 1\n" +
                "info 2" +
                "\n",parser.header.toString());
        assertEquals("55",parser.kvList.get("global").get(1).get("gkey3"));
        assertEquals("[{gkey1=true, gkey2=5 a, global=glob1}, {gkey3=55, global=glob2}]",
                parser.kvList.get("global").toString());
        assertEquals("[{problem=true, pkey1=33}]",
                parser.kvList.get("problem").toString());


    }
    @Test
    public void parseString2() throws Exception {
        System.out.println("parseString2");
        KVParser parser = new KVParser("solver", "problem");
        String test =
                "header 1 \n" +"" +
                        "problem random\n" +
                        "predicates = 100\n" +
                        "     cpRatio = 4\n" +
                        "     length = 3\n" +
                        "     precise = true\n" +
                        "     seed = 1\n" +
                        "\n" +
                        " solver walker\n" +
                        "    flips = 50000\n" +
                        "    monitor = 0\n" +
                        "    seed = 1\n" +
                        " \n" +
                        " solver reduction\n" +
                        "    strategy = INPUT\n" +
                        "    limit = 20\n" +
                        "    seed = 0\n" +
                        "    percentageOfSOSClauses = 50";
        parser.parseString(test);
        assertEquals("header 1\n",parser.header.toString());
        assertEquals("[{seed=1, flips=50000, monitor=0, solver=walker}, {seed=0, limit=20, strategy=INPUT, percentageOfSOSClauses=50, solver=reduction}]",
                parser.kvList.get("solver").toString());
        assertEquals("[{predicates=100, problem=random, seed=1, cpRatio=4, length=3, precise=true}]",
                parser.kvList.get("problem").toString());



        //System.out.println(parser.toString());

    }

    @Test
    public void parseFile() throws Exception {
        System.out.println("parseFile");
        KVParser parser = new KVParser("solver", "problem");

        String test =
                "header 1 \n" +"" +
                        "problem random\n" +
                        "predicates = 100\n" +
                        "     cpRatio = 4\n" +
                        "     length = 3\n" +
                        "     precise = true\n" +
                        "     seed = 1\n" +
                        "\n" +
                        " solver walker\n" +
                        "    flips = 50000\n" +
                        "    monitor = 0\n" +
                        "    seed = 1\n" +
                        " \n" +
                        " solver reduction\n" +
                        "    strategy = INPUT\n" +
                        "    limit = 20\n" +
                        "    seed = 0\n" +
                        "    percentageOfSOSClauses = 50";

        Utilities.writeTmpFile("KVParser","test",test);
        String tmp = System.getenv("TEMP");
        File file = Paths.get(tmp,"KVParser","test").toFile();
        parser.parseFile(file.getAbsolutePath());
        assertEquals("[{predicates=100, problem=random, seed=1, cpRatio=4, length=3, precise=true}]",
                parser.kvList.get("problem").toString());



    }

}