package Datastructures.Theory;

import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Created by ohlbach on 26.09.2018.
 */
public class ImplicationNodeTest {
    @Test
    public void addDownNode() throws Exception {
        System.out.println("addDownNode");
        ImplicationNode node1 = new ImplicationNode(1);
        ImplicationNode node2 = new ImplicationNode(2);
        assertEquals(0,node1.addDownNode(node2));
        assertEquals(1,node1.downNodes.size());
        assertEquals(node2,node1.downNodes.get(0));
        assertEquals(1,node2.upNodes.size());
        assertEquals(node1,node2.upNodes.get(0));

        assertEquals(1,node1.addDownNode(node2));

        ImplicationNode node3 = new ImplicationNode(-2);
        assertEquals(-1,node1.addDownNode(node3));
    }

    @Test
    public void disconnect() throws Exception {
        System.out.println("disconnect");
        ImplicationNode node1 = new ImplicationNode(1);
        ImplicationNode node2 = new ImplicationNode(2);
        ImplicationNode node3 = new ImplicationNode(3);
        ImplicationNode node4 = new ImplicationNode(4);
        ImplicationNode node5 = new ImplicationNode(5);
        ImplicationNode node6 = new ImplicationNode(6);
        ImplicationNode node = new ImplicationNode(0);
        node1.addDownNode(node);
        node2.addDownNode(node);
        node3.addDownNode(node);
        node.addDownNode(node4);
        node.addDownNode(node5);
        node.addDownNode(node6);
        assertEquals(3,node.downNodes.size());
        assertEquals(3,node.upNodes.size());
        assertEquals(1,node4.upNodes.size());
        assertEquals(1,node1.downNodes.size());
        node.disconnect();
        assertNull(node1.downNodes);
        assertNull(node5.upNodes);
        assertNull(node2.downNodes);
        assertNull(node4.upNodes);
    }


    @Test
    public void isAlmostLeafNode() throws Exception {
        System.out.println("isAlmostLeafNode");
        ImplicationNode node1 = new ImplicationNode(1);
        ImplicationNode node2 = new ImplicationNode(2);
        ImplicationNode node3 = new ImplicationNode(3);
        ImplicationNode node4 = new ImplicationNode(4);
        ImplicationNode node5 = new ImplicationNode(5);
        ImplicationNode node6 = new ImplicationNode(6);
        ImplicationNode node = new ImplicationNode(0);
        node1.addDownNode(node);
        node2.addDownNode(node);
        node3.addDownNode(node);
        node.addDownNode(node4);
        node.addDownNode(node5);
        node.addDownNode(node6);
        assertFalse(node1.isAlmostLeafNode());
        assertFalse(node6.isAlmostLeafNode());
        assertTrue(node.isAlmostLeafNode());
    }

}