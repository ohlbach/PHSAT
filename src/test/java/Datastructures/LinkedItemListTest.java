package Datastructures;

import junit.framework.TestCase;

public class LinkedItemListTest extends TestCase {

    class Clause extends LinkedItem {
        int id;
        public Clause(int id) {
            this.id = id;
        }
        @Override
        public String toString(Symboltable symboltable, int size) {
            return ""+id;}}

    public void testAddToFront() {
        System.out.println("addToFront");
        LinkedItemList<Clause> linkedlist = new LinkedItemList<>(null);
        assertEquals(0,linkedlist.size());
        assertEquals("",linkedlist.toString());
        assertEquals("",linkedlist.toString(null));
        Clause c1 = new Clause(1);
        assertFalse(c1.isInList);
        linkedlist.addToFront(c1);
        assertTrue(c1.isInList);
        assertEquals(1,linkedlist.size());
        assertEquals("1\n",linkedlist.toString());
        assertEquals("1\n",linkedlist.toString(null));
        Clause c2 = new Clause(5);
        linkedlist.addToFront(c2);
        assertEquals(2,linkedlist.size());
        assertEquals("5\n1\n",linkedlist.toString());
        assertEquals("5\n1\n",linkedlist.toString(null));

        linkedlist.remove(c2);
        assertEquals(1,linkedlist.size());
        assertEquals("1\n",linkedlist.toString());
        assertEquals("1\n",linkedlist.toString(null));
        linkedlist.remove(c1);
        assertEquals(0,linkedlist.size());
        assertEquals("",linkedlist.toString());
        assertEquals("",linkedlist.toString(null));
    }

    public void testAddToBack() {
        System.out.println("addToBack");
        LinkedItemList<Clause> linkedlist = new LinkedItemList<>("Test");
        Clause c1 = new Clause(1);
        linkedlist.addToBack(c1);
        assertEquals(1,linkedlist.size());
        assertEquals("Test\n1\n",linkedlist.toString());
        assertEquals("Test\n1\n",linkedlist.toString(null));
        Clause c2 = new Clause(5);
        linkedlist.addToBack(c2);
        assertEquals(2,linkedlist.size());
        assertEquals("Test\n1\n5\n",linkedlist.toString());
        assertEquals("Test\n1\n5\n",linkedlist.toString(null));

        Clause c3 = new Clause(8);
        linkedlist.addToFront(c3);
        assertEquals(3,linkedlist.size());
        assertEquals("Test\n8\n1\n5\n",linkedlist.toString());
        assertEquals("Test\n8\n1\n5\n",linkedlist.toString(null));

        linkedlist.remove(c1);
        assertEquals(2,linkedlist.size());
        assertEquals("Test\n8\n5\n",linkedlist.toString());
        assertEquals("Test\n8\n5\n",linkedlist.toString(null));
    }

    public void testGetLinkedItem() {
        System.out.println("getLinkedItem");
        LinkedItemList<Clause> linkedlist = new LinkedItemList<>(null);
        assertNull(linkedlist.getLinkedItem(0));
        Clause c1 = new Clause(1);
        linkedlist.addToBack(c1);
        assertEquals(c1,linkedlist.getLinkedItem(0));
        Clause c2 = new Clause(2);
        linkedlist.addToBack(c2);
        assertEquals(c1,linkedlist.getLinkedItem(0));
        assertEquals(c2,linkedlist.getLinkedItem(1));
        assertNull(linkedlist.getLinkedItem(2));

    }

    public void testIteration() {
        System.out.println("iteration");
        LinkedItemList<Clause> linkedlist = new LinkedItemList<>(null);
        for(int i = 0; i <= 10; ++i) {
            Clause c = new Clause(i);
            linkedlist.addToBack(c);}
        Clause c = linkedlist.firstLinkedItem;
        while(c != null) {
            if(c.id % 2 == 0) linkedlist.remove(c);
            c = (Clause)c.nextItem;
        }
        assertEquals("1\n3\n5\n7\n9\n",linkedlist.toString());

        linkedlist = new LinkedItemList<>(null);
        for(int i = 0; i <= 10; ++i) {
            c = new Clause(i);
            linkedlist.addToBack(c);}
        c = linkedlist.firstLinkedItem;
        while(c != null) {
            if(c.id % 3 < 2) linkedlist.remove(c);
            c = (Clause)c.nextItem;
        }
        assertEquals("2\n5\n8\n",linkedlist.toString());
    }


    }