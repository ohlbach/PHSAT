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

    private ArrayList<RSLiteral> rsLiterals = new ArrayList<>();

    protected void initialize(int literal, RSNode superNode) {
        literals.clear();
        literals.add(literal);
        this.superNode = superNode;
        if(superNode != null) {superNode.subNode = this;}
        rsLiterals.clear();}

    protected void addLiteral(int literal) {
        literals.add(literal);}

    protected void addRSLiteral(RSLiteral rsLiteral) {
        rsLiteral.rsNode = this;
        rsLiterals.add(rsLiteral);}

    protected void pop() {
        for(RSLiteral rsLiteral: rsLiterals) {rsLiteral.rsNode = null;}}

}
