package Coordinator;

import org.junit.Test;

import java.util.TreeSet;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 12.09.2018.
 */
public class ChangeBlockTest {
    @Test
    public void addOneLiteralClause1() throws Exception {
        System.out.println("addOneLiteralClause 1");
        ChangeBlock block = new ChangeBlock();
        block.addOneLiteralClause(1);
        block.addOneLiteralClause(-2);
        assertEquals("[1, -2]",block.getOneLiteralClauses().toString());
    }

    @Test
    public void addOneLiteralClause2() throws Exception {
        System.out.println("addOneLiteralClause 2");
        ChangeBlock block = new ChangeBlock();
        TreeSet<Integer> implicants = new TreeSet<>();
        implicants.add(3); implicants.add(4);
        block.addImplication(1,implicants);
        implicants = new TreeSet<>();
        implicants.add(4); implicants.add(5);
        block.addImplication(2,implicants);
        block.addOneLiteralClause(-1);
        assertEquals("Units: [-1]\n" +
                "2 -> [4, 5]\n",block.toString());
        block.addOneLiteralClause(-5);
        assertEquals("Units: [-1, -5]\n" +
                "2 -> [4]\n",block.toString());

    }

    @Test
    public void addImplication() throws Exception {
        System.out.println("addImplication");
        ChangeBlock block = new ChangeBlock();
        TreeSet<Integer> implicants = new TreeSet<>();
        implicants.add(3); implicants.add(4);
        block.addImplication(1,implicants);
        assertEquals("{1=[3, 4]}",block.getImplications().toString());
        implicants = new TreeSet<>();
        implicants.add(4); implicants.add(-1);
        block.addImplication(1,implicants);
        assertEquals("{1=[-1, 3, 4]}",block.getImplications().toString());

    }

    @Test
    public void getOneLiteralClauses() throws Exception {

    }

    @Test
    public void getImplications() throws Exception {

    }

}