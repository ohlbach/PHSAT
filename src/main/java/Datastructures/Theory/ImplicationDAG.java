package Datastructures.Theory;

import Datastructures.Symboltable;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/** This class represents propositional logic ID_Implications 'p -&gt; q' as a DAG (directed acyclic graph).
 *  For an implication 'p -&gt; q' the converse '-q -&gt; -q' is also automatically inserted.
 *  The DAG avoids cycles and inconsistencies.
 *  For example, 'p -&gt; q' and '-p -&gt; q' yields 'q' as a new derived literal.
 *
 * Created by ohlbach on 26.09.2018.
 */
public class ImplicationDAG {
    private TreeSet<ImplicationNode> roots = new TreeSet<>();            // top literals in the implication hierarchy
    private TreeMap<Integer,ImplicationNode> nodesMap = new TreeMap<>(); // maps literals to ImplicationNodes
    private ArrayList<Consumer<Integer>>           trueLiteralObservers = new ArrayList<>(); // is applied when a true literal is derived
    private ArrayList<BiConsumer<Integer,Integer>> implicationObservers = new ArrayList<>(); // is applied when a new implication is derived
    private ArrayList<Consumer<int[]>>             equivalenceObservers = new ArrayList<>(); // is applied when a new equivalence class is derived
    private int timestamp = 0;

    /** clones the entire DAG
     *
     * @return the cloned DAG
     */
    public ImplicationDAG clone() {
        ImplicationDAG newDag = new ImplicationDAG();
        for(Map.Entry<Integer,ImplicationNode>  entry : nodesMap.entrySet() ) {
            newDag.nodesMap.put(entry.getKey(),new ImplicationNode(entry.getKey()));}
        for(ImplicationNode root : roots) {newDag.roots.add(newDag.getNode(root.literal));}
        for(Map.Entry<Integer,ImplicationNode>  entry : nodesMap.entrySet() ) {
            ImplicationNode oldNode = entry.getValue();
            ImplicationNode newNode = newDag.getNode(entry.getKey());
            if(oldNode.upNodes != null) {
                ArrayList<ImplicationNode> upNodes = new ArrayList<>();
                for(ImplicationNode oldUp :oldNode.upNodes) {upNodes.add(newDag.getNode(oldUp.literal));}
                newNode.upNodes = upNodes;}
            if(oldNode.downNodes != null) {
                ArrayList<ImplicationNode> downNodes = new ArrayList<>();
                for(ImplicationNode oldDown :oldNode.downNodes) {downNodes.add(newDag.getNode(oldDown.literal));}
                newNode.downNodes = downNodes;}}
        return newDag;}

    /** adds a true literal observer
     *
     * @param observer to be added*/
    public synchronized void addTrueLiteralObserver(Consumer<Integer> observer) {trueLiteralObservers.add(observer);}
    /** adds an implication observer
     *
     * @param observer to be added*/
    public synchronized void addImplicationObserver(BiConsumer<Integer,Integer> observer) {implicationObservers.add(observer);}
    /** adds an observer for equivalence classes.
     *
     * @param observer to be added*/
    public synchronized void addEquivalenceObserver(Consumer<int[]> observer) {equivalenceObservers.add(observer);}

    /** removes a true literal observer
     *
     * @param observer to be added*/
    public synchronized void removeTrueLiteralObserver(Consumer<Integer> observer) {trueLiteralObservers.remove(observer);}
    /** removes an implication observer
     *
     * @param observer to be added*/
    public synchronized void removeImplicationObserver(BiConsumer<Integer,Integer> observer) {implicationObservers.remove(observer);}
    /** removes an observer for equivalence classes.
     *
     * @param observer to be added*/
    public synchronized void removeEquivalenceObserver(Consumer<int[]> observer) {equivalenceObservers.remove(observer);}


    /** checks if there are no root nodes.
     *
     * @return true if the DAG is empty*/
    public boolean isEmpty() {return roots.isEmpty();}

    /** returns true if no implied literals have been registered
     *
     * @param literal a literal
     * @return true if no implied literals have been registered.
     */
    public boolean isEmpty(Integer literal) {
        return nodesMap.get(literal) == null;}

    /** checks if 'from -&gt; to' is a consequence of the ID_Implications in the DAG
     *
     * @param from a literal
     * @param to   a literal
     * @return true if 'from -&gt; to' is a consequence of the ID_Implications in the DAG
     */
    public boolean implies(Integer from, Integer to) {
        if(from.equals(to)) {return true;}
        ImplicationNode fromNode = nodesMap.get(from);
        if(fromNode == null) {return false;}
        ImplicationNode toNode = nodesMap.get(to);
        if(toNode == null) {return false;}
        ++timestamp;
        return implies(fromNode,toNode);}

    /** checks if 'from -&gt; to' is a consequence of the ID_Implications in the DAG
     *
     * @param from a literal
     * @param to   a literal
     * @return true if 'from -&gt; to' is a consequence of the ID_Implications in the DAG
     */
    private boolean implies(ImplicationNode from, ImplicationNode to) { // depth-first search
        if(from == to) {return true;}
        if(from.timestamp == timestamp) {return false;}
        from.timestamp = timestamp;
        ArrayList<ImplicationNode> downNodes = from.downNodes;
        if(downNodes == null) {return false;}
        for(ImplicationNode node : downNodes) {if(node == to || implies(node,to)) return true;}
        return false;}

    /** collects for a literal all root literals above the literal
     *
     * @param literal a literal
     * @return all root literals above the literal, or null if the literal is not in the DAG.
     */
    public TreeSet<Integer> rootLiterals(Integer literal) {
        ImplicationNode node = nodesMap.get(literal);
        if(node == null) {return null;}
        TreeSet<Integer> rootLiterals = new TreeSet<>();
        apply(node, false, (lit -> {if(nodesMap.get(lit).upNodes == null) {rootLiterals.add(lit);}}));
        return rootLiterals;}

    /** returns for a literal its ImplicationNode.
     * If there is none yet, a new one is created.
     *
     * @param literal a literal
     * @return the corresponding ImplicationNode
     */
    private ImplicationNode  getNode(Integer literal) {
        ImplicationNode node = nodesMap.get(literal);
        if(node == null) {node = new ImplicationNode(literal); nodesMap.put(literal,node);}
        return node;}


    /** adds a clause to the DAG.
     * For a clause p,q the two ID_Implications '-p -&gt; q' and '-q -&gt; p' are added.
     *
     * @param literal1 a literal
     * @param literal2 a literal
     */
    public void addClause(Integer literal1, Integer literal2) {
        boolean unit = addImplication(-literal1,literal2);
        if(!unit) {addImplication(-literal2,literal1);}}

    /** adds a single implication 'p -&gt; q' to the DAG
     *
     * @param from the antecedent
     * @param to the succedent
     * @return true if the implication was not added (because it was already there, or a contradiction or a cycle was generated.
     */
    public boolean addImplication(Integer from, Integer to) {
        ImplicationNode fromNode = getNode(from);
        ImplicationNode toNode   = getNode(to);
        ++timestamp;
        if(implies(fromNode,toNode)) {return false;}
        ++timestamp;
        if(implies(toNode,fromNode)) {newEquivalence(toNode,fromNode); return true;}
        int status = fromNode.addDownNode(toNode);
        if(status == 1) {return false;}
        if(status == -1) {newTrueLiteral(-from); return true;}
        if(implies(-from,to)) {++timestamp; newTrueLiteral(toNode); return true;}
        if(toNode.upNodes != null && toNode.upNodes.size() == 1) {roots.remove(toNode);}
        reportImplication(from,to);
        if(fromNode.upNodes == null) {roots.add(fromNode);}
        return false;}


    /** clears the DAG by a literal which is supposed to be true.
     *
     * @param trueLiteral a literal
     */
    public void newTrueLiteral(Integer trueLiteral) {
        ++timestamp; timestamp += 2;
        ImplicationNode node = nodesMap.get(trueLiteral);
        if (node == null) {reportTrueLiteral(trueLiteral); removeFalseLiteral(-trueLiteral); return;}
        else {newTrueLiteral(node);}}

    private void newTrueLiteral(ImplicationNode trueNode) {
        if(trueNode.timestamp == timestamp) {return;}
        trueNode.timestamp = timestamp;
        reportTrueLiteral(trueNode.literal);
        roots.remove(trueNode);
        nodesMap.remove(trueNode.literal);
        ArrayList<ImplicationNode> downNodes = trueNode.downNodes;
        if(downNodes != null) {for(Object downNode : downNodes.toArray()) {newTrueLiteral((ImplicationNode)downNode);}}
        disconnect(trueNode);
        removeFalseLiteral(-trueNode.literal);}

    /** disconnects the node form the DAG
     *
     * @param node the node to be disconnected*/
    private void disconnect(ImplicationNode node) {
        if(node.upNodes != null) {
            for(ImplicationNode upNode : node.upNodes) {
                if(upNode.downNodes.size() == 1) {roots.remove(upNode);}}}
        node.disconnect();}



    /** removes a cycle from the DAG.
     *
     * @param upNode    the upper entrance to the cycle
     * @param downNode  the lower entrance to the cycle
     */
    private void newEquivalence(ImplicationNode upNode, ImplicationNode downNode) {
        TreeSet<ImplicationNode> equivalences = new TreeSet<>();
        newEquivalence(upNode, downNode, equivalences);
        mergeEquivalences(upNode, equivalences);
        disconnectTopNode(upNode,equivalences);
        ImplicationNode negUpNode = getNode(-upNode.literal);
        TreeSet<ImplicationNode> negEquivalences = new TreeSet<>();
        for(ImplicationNode node : equivalences) {negEquivalences.add(nodesMap.get(-node.literal));}
        mergeEquivalences(negUpNode,negEquivalences);
        disconnectTopNode(negUpNode,negEquivalences);
        int[] equivs = new int[equivalences.size()+1];
        equivs[0] = upNode.literal;
        int i = 0;
        for(ImplicationNode node : equivalences) {
            roots.remove(node);
            roots.remove(getNode(-node.literal));
            equivs[++i] = node.literal;}
        for(Consumer<int[]> observer : equivalenceObservers) {observer.accept(equivs);}
    }

    /** disconnects the representative of a cycle from the rest of the cycle.
     *
     * @param topNode      the representative of the equivalence class generated by the ccycle.
     * @param equivalences the list of equivalent literals (without the representative)
     */
    private void disconnectTopNode(ImplicationNode topNode,TreeSet<ImplicationNode> equivalences) {
        ArrayList<ImplicationNode> nodes = topNode.downNodes;
        if(nodes != null) {
            for(int i = 0; i < nodes.size(); ++i) {
                ImplicationNode node = nodes.get(i);
                if(equivalences.contains(node)) {nodes.remove(i); --i;}}
            if(nodes.isEmpty()) {topNode.downNodes = null;}}
        nodes = topNode.upNodes;
        if(nodes != null) {
            for(int i = 0; i < nodes.size(); ++i) {
                ImplicationNode node = nodes.get(i);
                if(equivalences.contains(node)) {nodes.remove(i); --i;}}
            if(nodes.isEmpty()) {topNode.upNodes = null;}}
            if(topNode.upNodes == null) {roots.remove(topNode);}}


    /** collects the nodes between upNode and downNode to form a new equivalence class
     *
     * @param upNode    the upper entrance to the cycle
     * @param downNode  the lower entrance to the cycle
     * @param equivalences the list of equivalent nodes
     */
    private void newEquivalence(ImplicationNode upNode, ImplicationNode downNode, TreeSet<ImplicationNode> equivalences) {
        if(upNode == downNode) {equivalences.add(downNode); return;}
        if(upNode.downNodes != null) {
            for(ImplicationNode node : upNode.downNodes) {
                ++timestamp;
                if(implies(node,downNode)) {equivalences.add(node); newEquivalence(node,downNode,equivalences);}}}}

    /** collects all edges into the cycle and the edges out of the cycle and attaches them to the representative of the
     * equivalence class.
     *
     * @param upNode       the representative of the equivalence class
     * @param equivalences the other nodes of the equivalence class
     */
    private void mergeEquivalences(ImplicationNode upNode, TreeSet<ImplicationNode> equivalences) {
        TreeSet<ImplicationNode> upNodes = new TreeSet<>();
        TreeSet<ImplicationNode> downNodes = new TreeSet<>();
        for(ImplicationNode node : equivalences) {
            ArrayList<ImplicationNode> downs = node.downNodes;
            ArrayList<ImplicationNode> ups = node.upNodes;
            if(downs != null) {for(ImplicationNode down : downs) {if(down != upNode && !equivalences.contains(down)){downNodes.add(down);}}}
            if(ups   != null) {for(ImplicationNode   up : ups)   {if(up   != upNode && !equivalences.contains(up))  {upNodes.add(up);}}}}
        upNode.joinUps(upNodes);
        upNode.joinDowns(downNodes);
        for(ImplicationNode node : equivalences) {
            disconnect(node);
            node.upNodes = null;
            node.downNodes = null;}
    }

    /** applies the consumer to all nodes below/above the given literal
     *
     * @param literal  a literal
     * @param down     if true then the consumer is applied to all implied literals, otherwise to all literals which imply 'this'
     * @param consumer  a consumer function to be applied to literals
     */
    public void apply(Integer literal, boolean down, Consumer<Integer> consumer) {
        ImplicationNode node = nodesMap.get(literal);
        if(node != null) {apply(node,down,consumer);}
        else {consumer.accept(literal);}}

    private void apply(ImplicationNode node, boolean down, Consumer<Integer> consumer) {
        consumer.accept(node.literal);
        ArrayList<ImplicationNode> nodes = down ? node.downNodes : node.upNodes;
        if(nodes != null) {
            for(ImplicationNode nextNode : nodes) {apply(nextNode,down,consumer);}}}

    /** applies the consumer to all root literals
     *
     * @param consumer to be applied to all root literals
     */
    public void applyToRoots(Consumer<Integer>consumer){
        for(ImplicationNode root : roots) {consumer.accept(root.literal);}}

    public void applyToImplications(Integer from, Integer to, BiConsumer<Integer,Integer> consumer) {
        ImplicationNode fromNode = getNode(from);
        ImplicationNode toNode = getNode(to);
        apply(fromNode,false,(upLiteral -> apply(toNode,true,(downLiteral -> consumer.accept(upLiteral,downLiteral)))));}


    /** removes a literal which is supposed to be false
     *
     * @param falseLiteral a literal
     */
    public void removeFalseLiteral(Integer falseLiteral) {
        ImplicationNode falseNode = nodesMap.get(falseLiteral);
        if(falseNode == null) {return;}
        nodesMap.remove(falseNode.literal);
        disconnect(falseNode);
        ArrayList<ImplicationNode> downNodes = falseNode.downNodes;
        if(downNodes != null) {
            for(ImplicationNode node :downNodes) {
                if(node.upNodes == null) {
                    if(node.downNodes == null) {nodesMap.remove(node.literal);}
                    else                       {roots.add(node);}}}}
        roots.remove(falseNode);
    }

    /** completes the model such that all ID_Implications in the DAG become true.
     * Not all literals need to be assigned truth values.
     * It is, however, not clear whether the model is minimal.
     * The DAG becomes empty by this operation.
     * The trueLiteralObservers are overwritten.
     *
     * @param model a partial model.
     */
    public void completeModel(Model model) {
        trueLiteralObservers.clear();
        trueLiteralObservers.add(literal-> {model.add(literal);});
        while(!roots.isEmpty()) {
            for(Object node : roots.last().downNodes.toArray()) { // this guarantees that the implication: 'root -> node' is true.
                ++timestamp; newTrueLiteral((ImplicationNode)node);}}}



    /** calls all trueLiteralObservers
     *
     * @param trueLiteral a true literal
     */
    private void reportTrueLiteral(Integer trueLiteral) {
        for(Consumer<Integer> observer : trueLiteralObservers) {observer.accept(trueLiteral);}}

    /** calls all implicationObservers
     *
     * @param from  a literal
     * @param to    a literal
     */
    private void reportImplication(Integer from, Integer to) {
        for(BiConsumer<Integer,Integer> observer : implicationObservers) {observer.accept(from,to);}}


    /** turns the entire DAG into a String
     *
     * @return the DAG as a String
     */
    public String toString() {return toString(null);}

    /** turns the entire DAG into a string. The literal names are used instead of the numbers
     *
     * @param symboltable mapping literal numbers to literal names.
     * @return the entire DAG as a String.
     */
    public String toString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        for(ImplicationNode node : roots) {
            toString(node,symboltable,0,st);}
        return st.toString();}

    private void toString(ImplicationNode node, Symboltable symboltable, int indent, StringBuilder st) {
        if(node.downNodes == null) {return;}
        if(indent > 0) {st.append(String.format("%"+indent+"s"," "));}
        st.append(node.toString()).append( " -> ");
        for(int i = 0; i < node.downNodes.size()-1; ++i) {st.append(node.downNodes.get(i).toString()).append(",");}
        st.append(node.downNodes.get(node.downNodes.size()-1).toString()).append("\n");
        for(ImplicationNode downNode : node.downNodes) {toString(downNode,symboltable,indent+5,st);}}

}
