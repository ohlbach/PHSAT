package Management;

import Utilities.Utilities;
import com.sun.org.apache.xpath.internal.SourceTree;
import org.junit.Test;

import java.io.File;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 12.10.2018.
 */
public class MonitorTest {

    File directory = new File(System.getenv("TEMP"));

    @Test
    public void hello() throws Exception {
        System.out.println("Mixed System.out");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        Monitor m = new Monitor(directory,"mixed",errors,warnings);
        m.print("T1", "Hello");
        m.print("T2", "Me too");
        m.flush();
    }

    @Test
    public void mixedFile() throws Exception {
        System.out.println("Mixed File");
        StringBuilder errors = new StringBuilder();
        String file = Utilities.tempFile("Monitor","file1");
        StringBuilder warnings = new StringBuilder();
        Monitor m = new Monitor(null,"mixed " + file,errors,warnings);
        System.out.println(errors.toString());
        m.print("T1", "Hello");
        m.print("T2", "Me too");
        m.flush();
        assertEquals("T1: Hello\n" +
                "T2: Me too\n",Utilities.readFile(file));}

    @Test
    public void separatedOut() throws Exception {
        System.out.println("Separated System.out");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        Monitor m = new Monitor(directory,"separated",errors,warnings);
        System.out.println(errors.toString());
        m.addThread("T1", "T1-Thread");
        m.addThread("T2", "T2-Thread");
        System.out.println(m.toString());
        m.print("T1", "Hello");
        m.print("T2", "Me too");
        m.print("T1", "Also Hello");
        m.print("T2", "Me also too");
        m.flush();
    }

    @Test
    public void separatedFile() throws Exception {
        System.out.println("Separated File");
        StringBuilder errors = new StringBuilder();
        StringBuilder warnings = new StringBuilder();
        String file = Utilities.tempFile("Monitor","file1");
        Monitor m = new Monitor(null,"separated " + file,errors,warnings);
        System.out.println(errors.toString());
        m.addThread("T1", "T1-Thread");
        m.addThread("T2", "T2-Thread");
        System.out.println(m.toString());
        m.print("T1", "Hello");
        m.print("T2", "Me too");
        m.print("T1", "Also Hello");
        m.print("T2", "Me also too");
        m.flush();
        assertEquals("Monitor\n" +
                "*******\n"+
                "T1:\n" +
                "T1-Thread\n" +
                "Hello\n" +
                "Also Hello\n" +
                "T2:\n" +
                "T2-Thread\n" +
                "Me too\n" +
                "Me also too\n",Utilities.readFile(file));
    }

}