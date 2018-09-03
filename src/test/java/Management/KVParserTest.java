package Management;

import org.junit.Test;

import static org.junit.Assert.*;

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
                "\n" +
                "info 2" +
                "\n",parser.header.toString());
        assertEquals("55",parser.kvList.get("global").get(1).get("gkey3"));

    }

}