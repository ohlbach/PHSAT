package Solvers.RecursiveSearch;

import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.Stack;

public class RSNode {

    private static Stack<RSNode> nodeReserve = new Stack<>();

    public static RSNode popRSNodeReserve() {
        synchronized (nodeReserve) {
            if(nodeReserve.isEmpty()) {return new RSNode();}
            else return nodeReserve.pop();}}

    public static void pushRSNodeReserve(RSNode rsNode) {
        synchronized (nodeReserve) {nodeReserve.push(rsNode);}}

    protected RSNode superNode = null;
    protected RSNode subNode = null;
    protected IntArrayList literals = new IntArrayList();

    protected ArrayList<RSLiteral> rsLiterals = new ArrayList<>();

    protected void initialize(int literal, RSNode superNode) {
        literals.clear();
        literals.add(literal);
        this.superNode = superNode;
        if(superNode != null) {superNode.subNode = this;}
        rsLiterals.clear();}

    protected void addRSLiteral(RSLiteral rsLiteral) {
        rsLiteral.rsNode = this;
        rsLiterals.add(rsLiteral);}

    /** adds a literal which became temporarily true because of the node's first literal
     *
     * @param literal a temporarily true literal
     */
    protected void addTrueLiteral(int literal) {
        literals.add(literal);}

    protected int selectedLiteral() {
        return literals.getInt(0);}

    private int literalIndex = 1;

    protected int nextTrueLiteral() {
        if(literalIndex == literals.size()) return 0;
        return literals.getInt(literalIndex++);}

    protected void changeLastRSLiteral(RSLiteral rsLiteral) {
        rsLiterals.set(rsLiterals.size()-1,rsLiteral);}

    protected void pop() {
        for(RSLiteral rsLiteral: rsLiterals) {rsLiteral.rsNode = null;}}

}
