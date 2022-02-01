package Solvers.RecursiveSearch;

import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Stack;

/** An RSNode represents a node in the search tree of the recursive search.
 * The most important component are the literals.
 * The first literal in this list is the literal which is temporarily chosen to be true.
 * The remaining literals are the literals which became true because of this choice.
 *
 * In order to save space, unused RSNodes are stored in the nodeReserve stack.
 * All threads can pop RSNodes from this stack and push backtracked nodes back to the stack.
 */
public class RSNode {

    /** for storing unused RSNodes.*/
    private static final Stack<RSNode> nodeReserve = new Stack<>();

    /** either creates a new RSNode, or pops a free RSNode from the stack.
     * @param literal    the chosen literal in the search tree
     * @param superNode  null or the node's supernode
     *
     * @return a RSNode ready for use.
     */
    public static RSNode popRSNodeReserve(int literal, RSNode superNode) {
        synchronized (nodeReserve) {
            if(nodeReserve.isEmpty()) {return new RSNode(literal,superNode);}
            else {return nodeReserve.pop().initialize(literal,superNode);}}}

    /** returns a free RSNode to the stack
     *
     * @param rsNode an RSNode which is now free
     */
    public static void pushRSNodeReserve(RSNode rsNode) {
        RSNode supernode = rsNode.superNode;
        if(supernode != null) supernode.subNode = null;
        synchronized (nodeReserve) {nodeReserve.push(rsNode);}}

    protected RSNode superNode;       // the node's supernode (if there is one)
    protected RSNode subNode = null;  // the node's subnode (there is at most one)
    protected IntArrayList literals = new IntArrayList(); // the true literals

    protected ArrayList<RSLiteral> falseRSLiterals = new ArrayList<>();  // the blocked false literals
    protected ArrayList<RSLiteral> trueRSLiterals  = new ArrayList<>();  // the blocked true literals
    protected ArrayList<RSClause>  blockedRSClauses = new ArrayList<>(); // the blocked clauses

    /** creates a new RSNode
     *
     * @param literal    the chosen literal in the search tree
     * @param superNode  null or the node's supernode
     */
    public RSNode(int literal, RSNode superNode) {
        literals.add(literal);
        this.superNode = superNode;
        if(superNode != null) {superNode.subNode = this;}}

    /** initializes an RSNode which has been used earlier
     *
     * @param literal    the chosen literal in the search tree
     * @param superNode  null or the node's supernode
     *
     * @return   The rsNode itself
     */
    protected RSNode initialize(int literal, RSNode superNode) {
        literals.clear();
        literals.add(literal);
        this.superNode = superNode;
        if(superNode != null) {superNode.subNode = this;}
        falseRSLiterals.clear();
        trueRSLiterals.clear();
        blockedRSClauses.clear();
        return this;}

    /** adds a blocked false RSLiteral.
     *
     * @param rsLiteral the blocked false RSLiteral.
     */
    protected void addFalseRSLiteral(RSLiteral rsLiteral) {
        rsLiteral.blockingRSNode = this;
        falseRSLiterals.add(rsLiteral);}

    /** adds a blocked true RSLiteral.
     *
     * @param rsLiteral the blocked true RSLiteral.
     */
    protected void addTrueRSLiteral(RSLiteral rsLiteral) {
        rsLiteral.blockingRSNode = this;
        trueRSLiterals.add(rsLiteral);}

    /** adds a blocked true RSClause.
     *
     * @param rsClause the blocked true RSClause.
     */
    protected void addBlockedRSClause(RSClause rsClause) {
        rsClause.blockingRSNode = this;
        blockedRSClauses.add(rsClause);}

    /** adds a literal which became temporarily true because of the node's first literal
     *
     * @param literal a temporarily true literal
     */
    protected void addTrueLiteral(int literal) {
        literals.add(literal);}

    /** returns the literal which has been selected for this node
     *
     * @return the literal which has been selected for this node
     */
    protected int selectedLiteral() {
        return literals.getInt(0);}

    private int literalIndex = 1; // helps simulating an iterator

    /** returns the next true literal, or 0. Simulates an iterator.
     *
     * @return the next true literal, or 0
     */
    protected int nextTrueLiteral() {
        if(literalIndex == literals.size()) return 0;
        return literals.getInt(literalIndex++);}

    /** pops the node, i.e. unblocks the blocked literals and clauses and returns the node to the nodeReserve
     */
    protected void pop() {
        for(RSLiteral rsLiteral: falseRSLiterals)  {rsLiteral.unblockFalse();}
        for(RSLiteral rsLiteral: trueRSLiterals)   {rsLiteral.unblockTrue();}
        for(RSClause  rsClause:  blockedRSClauses) {rsClause.unblock();}
        nodeReserve.push(this);}

    /** turns the node into a string
     *
     * @return the node as a string.
     */
    public String toString(){
        StringBuilder st = new StringBuilder();
        st.append("Node: ").append(literals).append("\n");
        if(superNode != null) st.append("Supernode: ").append(superNode.selectedLiteral()).append("\n");
        if(subNode != null)   st.append("Subnode:   ").append(subNode.selectedLiteral()).append("\n");
        if(!falseRSLiterals.isEmpty()) {
            st.append("False Literals: ");
            for(RSLiteral rsLiteral : falseRSLiterals) st.append(rsLiteral.literal).append(",");
            st.append("\n");}
        if(!trueRSLiterals.isEmpty()) {
            st.append("True Literals:  ");
            for(RSLiteral rsLiteral : trueRSLiterals) st.append(rsLiteral.literal).append(",");
            st.append("\n");}
        if(!blockedRSClauses.isEmpty()) {
            st.append("Blocked Clauses:  ");
            for(RSClause rsClause : blockedRSClauses) st.append(rsClause.id).append(",");}
        return st.toString();}


}
