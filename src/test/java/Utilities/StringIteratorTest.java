package Utilities;

import junit.framework.TestCase;

public class StringIteratorTest extends TestCase {

    public void testHasNext() {
        String s = "a -  b -  cd";
        StringIterator iterator = new StringIterator(s,"\\s*-\\s*");
        StringBuilder st = new StringBuilder();
        while(iterator.hasNext()) st.append(iterator.next());
        assertEquals("abcd",st.toString());

    }
}