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

    protected int searchDepth = 0;    // the node's search depth
    protected long blockingNodeId = 0;
    protected RSNode superNode;       // the node's supernode (if there is one)
    protected RSNode subNode = null;  // the node's subnode (there is at most one)
    protected int selectedLiteral;
    protected ArrayList<long[]> literals = new ArrayList<>(); // the true literals

    protected ArrayList<RSLiteral> falseRSLiterals = new ArrayList<>();  // the blocked false literals
    protected ArrayList<RSLiteral> trueRSLiterals  = new ArrayList<>();  // the blocked true literals
    protected ArrayList<RSClause>  blockedRSClauses = new ArrayList<>(); // the blocked clauses

    /** creates a new RSNode
     *
     * @param selectedLiteral    the chosen literal in the search tree
     * @param superNode  null or the node's supernode
     */
    public RSNode(int selectedLiteral, RSNode superNode) {
        this.selectedLiteral = selectedLiteral;
        this.superNode = superNode;
        if(superNode != null) {
            superNode.subNode = this;
            searchDepth = superNode.searchDepth+1;
            blockingNodeId = (long)1 << searchDepth;}}

    /** initializes an RSNode which has been used earlier
     *
     * @param selectedLiteral    the chosen literal in the search tree
     * @param superNode  null or the node's supernode
     *
     * @return   The rsNode itself
     */
    protected RSNode initialize(int selectedLiteral, RSNode superNode) {
        this.selectedLiteral = selectedLiteral;
        this.superNode = superNode;
        if(superNode != null) {
            superNode.subNode = this;
            searchDepth = superNode.searchDepth+1;
            blockingNodeId = (long)1 << searchDepth;}
        literals.clear();
        falseRSLiterals.clear();
        trueRSLiterals.clear();
        blockedRSClauses.clear();
        return this;}

    /** adds a blocked false RSLiteral.
     *
     * @param rsLiteral the blocked false RSLiteral.
     */
    protected void addFalseRSLiteral(RSLiteral rsLiteral) {
        falseRSLiterals.add(rsLiteral);}

    /** adds a blocked true RSLiteral.
     *
     * @param rsLiteral the blocked true RSLiteral.
     */
    protected void addTrueRSLiteral(RSLiteral rsLiteral) {
        trueRSLiterals.add(rsLiteral);}

    /** adds a blocked true RSClause.
     *
     * @param rsClause the blocked true RSClause.
     */
    protected void addBlockedRSClause(RSClause rsClause) {
        blockedRSClauses.add(rsClause);}

    /** adds a literal which became temporarily true because of the node's first literal
     *
     */
    protected RSNode addTrueLiteral(int literal, long blockingNodeIds) {
        for(int i = 0; i < literals.size(); ++i) {
            long[] otherNodeIds = literals.get(i);
            int otherLiteral = (int)otherNodeIds[0];
            if(literal == otherLiteral){
                otherNodeIds[1] = highestBlockingNodeIds(blockingNodeIds,otherNodeIds[1]);
                return null;}
            if(literal == -otherLiteral) { // contradiction backtracking
                return backtrack(blockingNodeIds | otherNodeIds[1]);}}
        literals.add(new long[]{literal,blockingNodeIds});
        return null;}

    /** finds the literals with the highest RSNode belonging to literals1[1] and literals2[1]
     * This enables the longest backjump when backtracking
     *
     * @param blockingNodeIds1  a bitlist of node-bits for the derivation of a literal
     * @param blockingNodeIds2  a bitlist of node-bits for the derivation of a literal
     * @return the literals with the highest RSNode belonging to literals1[1] and literals2[1]
     */
    private long highestBlockingNodeIds(long blockingNodeIds1, long blockingNodeIds2) {
        RSNode supNode = superNode;
        while(supNode != null) {
            if((blockingNodeIds1 & supNode.blockingNodeId) != 0) return blockingNodeIds2;
            if((blockingNodeIds2 & supNode.blockingNodeId) != 0) return blockingNodeIds1;
            supNode = supNode.superNode;}
        return blockingNodeIds1;}


    /** backtracks to the lowest RSNode belonging to literals1[1] and literals2[1}
     *
     * @param blockingNodeIds a list of derived literals with literals1[0] the selected literal
     * @return the lowest RSNode belonging to literals1[1] and literals2[1}
     */
    protected RSNode backtrack(long blockingNodeIds) {
        RSNode supNode = superNode;
        pop();
        while((supNode.blockingNodeId & blockingNodeIds) == 0) {
            supNode.pop();
            supNode = supNode.superNode;}
        return supNode;}

    private int literalIndex = 1; // helps simulating an iterator

    /** returns the next true literal, or 0. Simulates an iterator.
     *
     * @return the next true literal, or 0
     */
    protected long[] nextTrueLiteral() {
        if(literalIndex == literals.size()) return null;
        return literals.get(literalIndex++);}

    /** pops the node, i.e. unblocks the blocked literals and clauses and returns the node to the nodeReserve
     */
    protected void pop() {
        for(RSLiteral rsLiteral: falseRSLiterals)  {rsLiteral.unblockFalse();}
        for(RSLiteral rsLiteral: trueRSLiterals)   {rsLiteral.unblockTrue();}
        for(RSClause  rsClause:  blockedRSClauses) {rsClause.unblock();}
        nodeReserve.push(this);}

    protected RSNode propagateUnits(RecursiveSearcher recursiveSearcher) {
        for(int i = 0; i  < literals.size(); ++i) {
            long[] literal = literals.get(i);
             RSNode backtrackedNode = recursiveSearcher.blockLiteral((int)literal[0],this);
             if(backtrackedNode != null) return backtrackedNode;}
        return null;}

    /** turns the node into a string
     *
     * @return the node as a string.
     */
    public String toString(){
        StringBuilder st = new StringBuilder();
        st.append("Node: ").append(literals).append("\n");
        if(superNode != null) st.append("Supernode: ").append(superNode.selectedLiteral).append("\n");
        if(subNode != null)   st.append("Subnode:   ").append(subNode.selectedLiteral).append("\n");
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
