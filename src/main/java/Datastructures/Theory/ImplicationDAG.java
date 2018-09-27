package Datastructures.Theory;

import Datastructures.Symboltable;
import com.sun.istack.internal.Pool;

import java.util.ArrayList;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Created by ohlbach on 26.09.2018.
 */
public class ImplicationDAG {
    private TreeSet<ImplicationNode> roots = new TreeSet<>();
    private TreeMap<Integer,ImplicationNode> nodesMap = new TreeMap<>();
    private ArrayList<Consumer<Integer>> trueLiteralObservers = new ArrayList<>();
    private ArrayList<BiConsumer<Integer,Integer>> implicationObservers = new ArrayList<>();
    private ArrayList<Consumer<int[]>> equivalenceObservers = new ArrayList<>();
    private int timestamp = 0;
    private final ReentrantReadWriteLock rwl = new ReentrantReadWriteLock();
    private final Lock readLock = rwl.readLock();
    private final Lock writeLock = rwl.writeLock();

    public synchronized void addTrueLiteralObserver(Consumer<Integer> observer) {trueLiteralObservers.add(observer);}
    public synchronized void addImplicationObserver(BiConsumer<Integer,Integer> observer) {implicationObservers.add(observer);}
    public synchronized void addEquivalenceObserver(Consumer<int[]> observer) {equivalenceObservers.add(observer);}


    public boolean implies(Integer from, Integer to) {
        if(from.equals(to)) {return true;}
        readLock.lock();
        try{ImplicationNode fromNode = nodesMap.get(from);
            if(fromNode == null) {return false;}
            ImplicationNode toNode = nodesMap.get(to);
            if(toNode == null) {return false;}
            ++timestamp;
            return implies(fromNode,toNode);}
        finally{readLock.unlock();}}

    private boolean implies(ImplicationNode from, ImplicationNode to) { // depth-first search
        if(from == to) {return true;}
        if(from.timestamp == timestamp) {return false;}
        from.timestamp = timestamp;
        ArrayList<ImplicationNode> downNodes = from.downNodes;
        if(downNodes == null) {return false;}
        for(ImplicationNode node : downNodes) {if(node == to || implies(node,to)) return true;}
        return false;}

    public ArrayList<Integer> rootLiterals(Integer literal) {
        readLock.lock();
        try{ImplicationNode node = nodesMap.get(literal);
            if(node == null) {return null;}
            ++timestamp;
            ArrayList<Integer> rootLiterals = new ArrayList<>();
            ArrayList<ImplicationNode> upnodes = node.upNodes;
            if(upnodes != null){for(ImplicationNode upnode : upnodes) {rootLiterals(upnode,rootLiterals);}}
            else{rootLiterals.add(literal);}
            return rootLiterals;}
        finally{readLock.unlock();}}

    private void rootLiterals(ImplicationNode node, ArrayList<Integer> rootLiterals) {
        if(node.timestamp == timestamp) {return;}
        node.timestamp = timestamp;
        ArrayList<ImplicationNode> upnodes = node.upNodes;
        if(upnodes != null){for(ImplicationNode upnode : upnodes) {rootLiterals(upnode,rootLiterals);}}
        else{rootLiterals.add(node.literal);}}


    public void addClause(Integer literal1, Integer literal2) {
        writeLock.lock();
        try{addImplication(-literal1,literal2);
            addImplication(-literal2,literal1);}
        finally{writeLock.unlock();}}

    private ImplicationNode  getNode(Integer literal) {
        ImplicationNode node = nodesMap.get(literal);
        if(node == null) {node = new ImplicationNode(literal); nodesMap.put(literal,node);}
        return node;}

    public void addImplication(Integer from, Integer to) {
        writeLock.lock();
        try{ImplicationNode fromNode = getNode(from);
            ImplicationNode toNode   = getNode(to);
            ++timestamp;
            if(implies(fromNode,toNode)) {return;}
            ++timestamp;
            if(implies(toNode,fromNode)) {newEquivalence(toNode,fromNode); return;}
            int status = fromNode.addDownNode(toNode);
            if(status == 1) {return;}
            if(status == -1) {newTrueLiteral(-from);}
            if(implies(-from,to)) {++timestamp; newTrueLiteral(toNode);}
            reportImplication(from,to);
            if(fromNode.upNodes == null) {roots.add(fromNode);}}
        finally{writeLock.unlock();}
    }

    public void newTrueLiteral(Integer trueLiteral) {
        writeLock.lock();
        try{++timestamp;
            ImplicationNode node = nodesMap.get(trueLiteral);
            if (node == null) {reportTrueLiteral(trueLiteral); removeFalseLiteral(-trueLiteral); return;}
            else {newTrueLiteral(node);}}
        finally{writeLock.unlock();}}


    private void newTrueLiteral(ImplicationNode trueNode) {
        if(trueNode.timestamp == timestamp) {return;}
        trueNode.timestamp = timestamp;
        reportTrueLiteral(trueNode.literal);
        nodesMap.remove(trueNode);
        roots.remove(trueNode);
        ArrayList<ImplicationNode> downNodes = trueNode.downNodes;
        if(downNodes != null) {for(ImplicationNode downNode : downNodes) {newTrueLiteral(downNode);}}
        trueNode.disconnect();
        removeFalseLiteral(-trueNode.literal);}

    private void newEquivalence(ImplicationNode upNode, ImplicationNode downNode) {
        TreeSet<ImplicationNode> equivalences = new TreeSet<>();
        newEquivalence(upNode, downNode, equivalences);
        mergeEquivalences(upNode, equivalences);
        ImplicationNode negUpNode = nodesMap.get(-upNode.literal);
        TreeSet<ImplicationNode> negEquivalences = new TreeSet<>();
        for(ImplicationNode node : equivalences) {negEquivalences.add(nodesMap.get(-node.literal));}
        mergeEquivalences(negUpNode,negEquivalences);
        int[] equivs = new int[equivalences.size()+1];
        equivs[0] = upNode.literal;
        int i = 0;
        for(ImplicationNode node : equivalences) {equivs[++i] = node.literal;}
        for(Consumer<int[]> observer : equivalenceObservers) {observer.accept(equivs);}
    }


    private void newEquivalence(ImplicationNode upNode, ImplicationNode downNode, TreeSet<ImplicationNode> equivalences) {
        for(ImplicationNode node : upNode.downNodes) {
            if(implies(node,downNode)) {equivalences.add(node); newEquivalence(node,downNode,equivalences);}}}

    private void mergeEquivalences(ImplicationNode upNode, TreeSet<ImplicationNode> equivalences) {
        TreeSet<ImplicationNode> upNodes = new TreeSet<>();
        TreeSet<ImplicationNode> downNodes = new TreeSet<>();
        for(ImplicationNode node : equivalences) {
            ArrayList<ImplicationNode> downs = node.downNodes;
            ArrayList<ImplicationNode> ups = node.upNodes;
            if(downs != null) {for(ImplicationNode down : downs) {if(!equivalences.contains(down)){downs.add(down);}}}
            if(ups   != null) {for(ImplicationNode   up : ups)   {if(!equivalences.contains(up)){ups.add(up);}}}}

        for(ImplicationNode node : equivalences) {node.disconnect();}
        ArrayList<ImplicationNode> ups = upNode.upNodes;
        if(ups == null) {ups = new ArrayList<>(); upNode.upNodes = ups;}
        for(ImplicationNode node : upNodes) {if(!ups.contains(node)) {ups.add(node);}}
        ArrayList<ImplicationNode> downs  = upNode.downNodes;
        for(ImplicationNode node : downNodes) {if(!downs.contains(node)) {downs.add(node);}}}


    public void apply(Integer literal, boolean down, Consumer<Integer> consumer) {
        writeLock.lock();
        ++timestamp;
        try{ImplicationNode node = nodesMap.get(literal);
            if(node != null) {++timestamp; apply(node,down,consumer);}
            else {consumer.accept(literal);}}
        finally{writeLock.unlock();}}

    private void apply(ImplicationNode node, boolean down, Consumer<Integer> consumer) {
        consumer.accept(node.literal);
        ArrayList<ImplicationNode> nodes = down ? node.downNodes : node.upNodes;
        if(nodes != null) {
            for(ImplicationNode nextNode : nodes) {
                if(nextNode.timestamp != timestamp) {
                    nextNode.timestamp = timestamp;
                    apply(nextNode,down,consumer);}}}}

    public void applyToRoots(Consumer<Integer>consumer){
        readLock.lock();
        try{for(ImplicationNode root : roots) {consumer.accept(root.literal);}}
        finally{readLock.unlock();}}

    public void applyToImplications(Integer from, Integer to, BiConsumer<Integer,Integer> consumer) {
        readLock.lock();
        try {ImplicationNode fromNode = getNode(from);
             ImplicationNode toNode = getNode(to);
             applyToImplications(fromNode,toNode,consumer);}
        finally{readLock.unlock();}}


    private void applyToImplications(ImplicationNode from, ImplicationNode to, BiConsumer<Integer,Integer> consumer) {
        consumer.accept(from.literal,to.literal);
        ArrayList<ImplicationNode> upNodes   = from.upNodes;
        ArrayList<ImplicationNode> downNodes = to.upNodes;
        if(upNodes == null) {
            if(downNodes == null) {return;}
            for(ImplicationNode downNode : downNodes) {applyToImplications(from,downNode,consumer);}}
        else {
            for(ImplicationNode upNode : upNodes) {consumer.accept(upNode.literal,to.literal);}
            if(downNodes == null) {return;}
            for(ImplicationNode upNode : upNodes) {
                for(ImplicationNode downNode : downNodes) {applyToImplications(upNode,downNode,consumer);}}}}


    private void removeFalseLiteral(Integer falseLiteral) {
        ImplicationNode falseNode = nodesMap.get(falseLiteral);
        if(falseNode == null) {return;}
        nodesMap.remove(falseNode);
        roots.remove(falseNode);
        falseNode.disconnect();
        ArrayList<ImplicationNode> downNodes = falseNode.downNodes;
        if(downNodes != null) {
            for(ImplicationNode node :downNodes) {if(node.upNodes == null) {roots.add(node);}}}}

    private void reportTrueLiteral(Integer trueLiteral) {
        for(Consumer<Integer> observer : trueLiteralObservers) {observer.accept(trueLiteral);}}

    private void reportImplication(Integer from, Integer to) {
        for(BiConsumer<Integer,Integer> observer : implicationObservers) {observer.accept(from,to);}}


    public String toString() {return toString(null);}


    public String toString(Symboltable symboltable) {
        readLock.lock();
        try{StringBuilder st = new StringBuilder();
            for(ImplicationNode node : roots) {
                toString(node,symboltable,0,st);}
        return st.toString();}
        finally{readLock.unlock();}}

    private void toString(ImplicationNode node, Symboltable symboltable, int indent, StringBuilder st) {
        if(node.downNodes == null) {return;}
        if(indent > 0) {st.append(String.format("%"+indent+"s"," "));}
        st.append(node.toString()).append( " -> ");
        for(int i = 0; i < node.downNodes.size()-1; ++i) {st.append(node.downNodes.get(i).toString()).append(",");}
        st.append(node.downNodes.get(node.downNodes.size()-1).toString()).append("\n");
        for(ImplicationNode downNode : node.downNodes) {toString(downNode,symboltable,indent+5,st);}}

}
