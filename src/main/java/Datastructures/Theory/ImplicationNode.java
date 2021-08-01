package Datastructures.Theory;

import Datastructures.Symboltable;

import java.util.ArrayList;
import java.util.Collection;

/** This class represents the nodes in an ImplicationDAG. <br>
 * A node contains a literal and the 'downNodes' and 'upNodes'. <br>
 * The downNodes are the literals which are implied by the node's literal.<br>
 * The upNodes imply the node's literal. <br>
 * Thus, if p -&gt; q and q -&gt; r the q's node has p in its upNodes and r in its downNodes.
 *
 * Created by ohlbach on 26.09.2018.
 */
public class ImplicationNode implements Comparable<ImplicationNode> {
    public Integer literal;
    ArrayList<ImplicationNode> downNodes = null;
    ArrayList<ImplicationNode> upNodes = null;
    int timestamp = 0;

    /** constructs an ImplicationNode
     *
     * @param literal the node's literal.
     */
    public ImplicationNode(Integer literal) {
        this.literal = literal;}

    /** adds an implication 'this -&gt; downNode'
     * If downNode's literal or its negation are already in the node's downNodes list, it is not changed.
     *
     * @param downNode contains the implied literal
     * @return 1 if the literal was already in the downNodes, -1 if its negation was in the downNodes, 0 otherwise.
     */
    public int addDownNode(ImplicationNode downNode) {
        if(downNodes == null) {downNodes = new ArrayList<>();}
        else {if(downNodes.contains(downNode)) {return 1;}
              if(contains(downNodes,-downNode.literal)) {return -1;}}
        downNodes.add(downNode);
        ArrayList<ImplicationNode> upNodes = downNode.upNodes;
        if(upNodes == null) {upNodes = new ArrayList<>(); downNode.upNodes = upNodes;}
        upNodes.add(this);
        return 0;}

    /** joins a list of ImplicationNodes to the node's upNodes list.
     * The downNodes of the new upNodes get 'this' added.
     *
     * @param ups a list of new upNodes.
     */
    public void joinUps(Collection<ImplicationNode> ups) {
        if(ups.isEmpty()) {return;}
        if(upNodes == null) {upNodes = new ArrayList<>();}
        for(ImplicationNode node : ups) {
            if(upNodes.contains(node)) {continue;}
            upNodes.add(node);
            if(node.downNodes == null) {node.downNodes = new ArrayList<>();}
            if(!node.downNodes.contains(this)) {node.downNodes.add(this);}}}

    /** joins a list of ImplicationNodes to the node's downNodes list.
     * The upNodes of the new downNodes get 'this' added.
     *
     * @param downs a list of new downNodes.
     */
    public void joinDowns(Collection<ImplicationNode> downs) {
        if(downs.isEmpty()) {return;}
        if(downNodes == null) {downNodes = new ArrayList<>();}
        for(ImplicationNode node : downs) {
            if(downNodes.contains(node)) {continue;}
            downNodes.add(node);
            if(node.upNodes == null) {node.upNodes = new ArrayList<>();}
            if(!node.upNodes.contains(this)) {node.upNodes.add(this);}}}





    /** disconnects the node from its upNodes and downNodes.
     */
    public void disconnect() {
        if(upNodes != null) {
            for(ImplicationNode upNode : upNodes) {
                ArrayList nodes = upNode.downNodes;
                if(nodes == null) {continue;}
                nodes.remove(this);
                if(nodes.isEmpty()) {upNode.downNodes = null;}}}
        if(downNodes != null) {
            for(ImplicationNode downNode : downNodes) {
                ArrayList nodes = downNode.upNodes;
                if(nodes == null) {continue;}
                nodes.remove(this);
                if(nodes.isEmpty()) {downNode.upNodes = null;}}
        }}

    private boolean contains(ArrayList<ImplicationNode> nodes, Integer literal) {
        for(ImplicationNode node : nodes) {if(node.literal == literal) {return true;}}
        return false;}

    /** true if the node's downNodes are leaf nodes.
     *
     * @return true if the node's downNodes are leaf nodes.
     */
    public boolean isAlmostLeafNode() {
        if(downNodes == null) {return false;}
        for(ImplicationNode downNode : downNodes) {if(downNode.downNodes != null) {return false;}}
        return true;}


    /** compares the nodes by their literals.
     *
     * @param node a node to be compared with
     * @return -1, 0 or 1
     */
    public int compareTo(ImplicationNode node) {
        return Integer.compare(literal, node.literal);}

    /** return the literal as string
     *
     * @return the literal as string
     */
    public String toString() {return toString(null);}

    /** returns the literal or its name as string
     *
     * @param symboltable a symbol table, or null
     * @return the literal or its name as string
     */
    public String toString(Symboltable symboltable) {
        return symboltable == null ? Integer.toString(literal) : symboltable.toString(literal);}
}
