package Utilities;

import junit.framework.TestCase;

import java.io.File;
import java.io.FileNotFoundException;
import java.nio.file.Paths;

public class FileIteratorTest extends TestCase {

    public void testHasNext() throws FileNotFoundException {
        String test = "AA\nBB\n  CC";
        Utilities.writeTmpFile("FileIterator","test",test);
        String tmp = System.getenv("TEMP");
        File file = Paths.get(tmp,"FileIterator","test").toFile();
        FileIterator iterator = new FileIterator(file.getAbsolutePath());
        StringBuilder st = new StringBuilder();
        while(iterator.hasNext()) {st.append(iterator.next());}
        assertEquals("AABB  CC",st.toString());

    }
}