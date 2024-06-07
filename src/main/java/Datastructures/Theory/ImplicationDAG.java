package Datastructures.Theory;

import Datastructures.Symboltable;


import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

/** This class represents propositional logic ID_Implications 'p -&gt; q' as a DAG (directed acyclic graph).
 *  For an implication 'p -&gt; q' the converse '-q -&gt; -q' is also automatically inserted.
 *  The DAG avoids cycles and inconsistencies.
 *  For example, 'p -&gt; q' and '-p -&gt; q' yields 'q' as a new derived literal.
 *
 * Created by ohlbach on 26.09.2018.
 */
public class ImplicationDAG {
    private TreeSet<ImplicationNode> roots = new TreeSet<>();            // top predicates in the implication hierarchy
    private TreeMap<Integer,ImplicationNode> nodesMap = new TreeMap<>(); // maps predicates to ImplicationNodes
    private ArrayList<Consumer<Integer>>           trueLiteralObservers = new ArrayList<>(); // is applied when a true literal is derived
    private ArrayList<BiConsumer<ImplicationNode,ImplicationNode>> implicationObservers = new ArrayList<>(); // is applied when a new implication is derived
    private ArrayList<Consumer<int[]>>             equivalenceObservers = new ArrayList<>(); // is applied when a new equivalence class is derived
    private int timestamp = 0;
    public boolean checkConsistency = true;

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
    public synchronized void addImplicationObserver(BiConsumer<ImplicationNode,ImplicationNode> observer) {implicationObservers.add(observer);}
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
    public synchronized void removeImplicationObserver(BiConsumer<ImplicationNode,ImplicationNode> observer) {implicationObservers.remove(observer);}
    /** removes an observer for equivalence classes.
     *
     * @param observer to be added*/
    public synchronized void removeEquivalenceObserver(Consumer<int[]> observer) {equivalenceObservers.remove(observer);}


    /** checks if there are no root nodes.
     *
     * @return true if the DAG is empty*/
    public boolean isEmpty() {return roots.isEmpty();}

    /** returns true if no implied predicates have been registered
     *
     * @param literal a literal
     * @return true if no implied predicates have been registered.
     */
    public boolean isEmpty(Integer literal) {
        ImplicationNode node = nodesMap.get(literal);
        return node == null || node.downNodes == null || node.downNodes.isEmpty();}

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

    /** collects for a literal all root predicates above the literal
     *
     * @param literal a literal
     * @return all root predicates above the literal, or null if the literal is not in the DAG.
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

    public ArrayList<ImplicationNode> getSubnodes(Integer literal) {
        ImplicationNode node = nodesMap.get(literal);
        return node.downNodes;}


    /** adds a clause to the DAG.
     * For a clause p,q the two ID_Implications '-p -&gt; q' and '-q -&gt; p' are added.
     *
     * @param literal1 a literal
     * @param literal2 a literal
     */
    public void addClause(Integer literal1, Integer literal2) {
        boolean unit = addImplication(-literal1,literal2,false);
        if(!unit) {addImplication(-literal2,literal1,true);}
        if(checkConsistency) {checkConsistency();}}

    /** adds a single implication 'p -&gt; q' to the DAG
     *
     * @param from the antecedent
     * @param to the succedent
     * @return true if the implication was not added (because it was already there, or a contradiction or a cycle was generated.
     */
    public boolean addImplication(Integer from, Integer to, boolean report) {
        ImplicationNode fromNode = getNode(from);
        ImplicationNode toNode   = getNode(to);
        ++timestamp;
        if(implies(fromNode,toNode)) {return false;}
        ++timestamp;
        if(implies(toNode,fromNode)) {newEquivalence(toNode,fromNode); return true;}
        findCommonSupernodes(fromNode,toNode);
        disconnectTransitive(fromNode,toNode);
        disconnectUp(fromNode,toNode);
        disconnectDown(fromNode,toNode);
        int status = fromNode.addDownNode(toNode);
        if(status == 1) {return false;}
        if(status == -1) {newTrueLiteral(-from,report); return true;}
        if(implies(-from,to)) {++timestamp; newTrueLiteral(toNode); return true;}
        if(toNode.upNodes != null && toNode.upNodes.size() == 1) {roots.remove(toNode);}
        if(report){reportImplication(fromNode,toNode);}
        if(fromNode.upNodes == null) {roots.add(fromNode);}
        return false;}

    /** For common supernodes n for fromNode and -toNode, -n is a true literal.
     *  These predicates are searched.
     *
     * @param fromNode a node
     * @param toNode   a node
     */
    private void findCommonSupernodes(ImplicationNode fromNode, ImplicationNode toNode) {
        toNode = getNode(-toNode.literal);
        if(toNode == null || fromNode.upNodes == null || toNode.upNodes == null) {return;}
        ++timestamp;
        markSupernodes(fromNode,timestamp);
        ArrayList<ImplicationNode> trueLiterals = new ArrayList<>();
        collectMarkedLiterals(toNode,timestamp, trueLiterals);
        for(ImplicationNode literal : trueLiterals) {
            ++timestamp; newTrueLiteral(literal);}
    }

    /** marks the supernodes of fromNode with the timestamp
     *
     * @param fromNode   a node
     * @param timestamp  an integer
     */
    private void markSupernodes(ImplicationNode fromNode, int timestamp) {
        if(fromNode.upNodes != null) {
            for(ImplicationNode node : fromNode.upNodes) {
                node.timestamp = timestamp;
                markSupernodes(node,timestamp);}}}

    /** collects the supernodes of toNode which are marked with the timestampl
     *
     * @param toNode     a node
     * @param timestamp  an integer
     * @param trueLiterals for adding the negated common supernodes
     */
    private void collectMarkedLiterals(ImplicationNode toNode, int timestamp, ArrayList<ImplicationNode> trueLiterals) {
        if(toNode.upNodes != null) {
            for(ImplicationNode node : toNode.upNodes) {
                if(node.timestamp == timestamp) {trueLiterals.add(getNode(-node.literal));}
                collectMarkedLiterals(node,timestamp,trueLiterals);}}}



    /** The method searches searchNode upwards and removes toNode from its subnodes.
     *
     * @param toNode a node to be removed
     * @param searchNode a node
     */
      private void disconnectUp(ImplicationNode searchNode, ImplicationNode toNode) {
        if(searchNode.downNodes != null) {
            searchNode.downNodes.removeIf(node->{
                if(node == toNode) {
                    toNode.upNodes.remove(searchNode);
                    if(toNode.upNodes.isEmpty()) {toNode.upNodes = null;}
                    return true;}
                return false;});
            if(searchNode.downNodes.isEmpty()) {searchNode.downNodes = null;}}
        if(searchNode.upNodes != null) {
            for(ImplicationNode node : searchNode.upNodes) {disconnectUp(node,toNode);}}}

    /** The method searches searchNode downwards and removes fromNode from its upNodes.
     *
     * @param fromNode a node to be removed
     * @param searchNode a node
     */
    private void disconnectDown(ImplicationNode fromNode, ImplicationNode searchNode) {
        if(searchNode.upNodes != null) {
            searchNode.upNodes.removeIf(node->{
                if(node == fromNode) {
                    if(fromNode.downNodes != null) {
                        fromNode.downNodes.remove(searchNode);
                        if(fromNode.downNodes.isEmpty()) {fromNode.downNodes = null;}}
                    return true;}
                return false;});
            if(searchNode.upNodes.isEmpty()) {searchNode.upNodes = null;}}
        if(searchNode.downNodes != null) {
            for(ImplicationNode node : searchNode.downNodes) {disconnectDown(fromNode,node);}}}


    /** disconnects the sub-nodes of fromNode, which are already sub-nodes of toNode.
     *
     * @param fromNode a node
     * @param toNode   a node
     */
    private void disconnectTransitive(ImplicationNode fromNode, ImplicationNode toNode) {
            if(fromNode.downNodes != null) {
                fromNode.downNodes.removeIf(node->toNode.downNodes != null && toNode.downNodes.contains(node));
                if(fromNode.downNodes.isEmpty()) {fromNode.downNodes = null;}}}

    /** clears the DAG by a literal which is supposed to be true.
     *
     * @param trueLiteral a literal
     */
    public void newTrueLiteral(Integer trueLiteral, boolean report) {
        ++timestamp; timestamp += 2;
        ImplicationNode node = nodesMap.get(trueLiteral);
        if (node == null) {
            reportTrueLiteral(trueLiteral);
            removeFalseLiteral(-trueLiteral); return;}
        else {newTrueLiteral(node);}}

    private void newTrueLiteral(ImplicationNode trueNode) {
        if(trueNode.timestamp == timestamp) {return;}
        trueNode.timestamp = timestamp;
        reportTrueLiteral(trueNode.literal);
        roots.remove(trueNode);
        ArrayList<ImplicationNode> downNodes = trueNode.downNodes;
        if(downNodes != null) {for(Object downNode : downNodes.toArray()) {newTrueLiteral((ImplicationNode)downNode);}}
        disconnect(trueNode);
        trueNode.upNodes = null;
        trueNode.downNodes = null;
        removeFalseLiteral(-trueNode.literal);
    }

    /** disconnects the node form the DAG
     *
     * @param node the node to be disconnected*/
    private void disconnect(ImplicationNode node) {
        if(node.upNodes != null) {
            for(ImplicationNode upNode : node.upNodes) {
                if(upNode.downNodes != null && upNode.downNodes.size() == 1) {roots.remove(upNode);}}}
        node.disconnect();}



    /** removes a cycle from the DAG.
     *
     * @param upNode    the upper entrance to the cycle
     * @param downNode  the lower entrance to the cycle
     */
    private void newEquivalence(ImplicationNode upNode, ImplicationNode downNode) {
        TreeSet<ImplicationNode> equivalences = new TreeSet<>();
        newEquivalence(upNode, downNode, equivalences);
        TreeSet<Integer> trueLiterals = new TreeSet<>();
        mergeEquivalences(upNode, equivalences,trueLiterals);
        disconnectTopNode(upNode,equivalences);
        ImplicationNode negUpNode = getNode(-upNode.literal);
        TreeSet<ImplicationNode> negEquivalences = new TreeSet<>();
        for(ImplicationNode node : equivalences) {negEquivalences.add(nodesMap.get(-node.literal));}
        mergeEquivalences(negUpNode,negEquivalences,trueLiterals);
        disconnectTopNode(negUpNode,negEquivalences);
        int[] equivs = new int[equivalences.size()+1];
        equivs[0] = upNode.literal;
        int i = 0;
        for(ImplicationNode node : equivalences) {
            roots.remove(node);
            roots.remove(getNode(-node.literal));
            equivs[++i] = node.literal;}
        for(Consumer<int[]> observer : equivalenceObservers) {observer.accept(equivs);}
        for(Integer literal : trueLiterals) {reportTrueLiteral(literal);}
    }



    /** disconnects the representative of a cycle from the rest of the cycle.
     *
     * @param topNode      the representative of the equivalence class generated by the cycle.
     * @param equivalences the list of equivalent predicates (without the representative)
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
            if(topNode.upNodes != null) {roots.remove(topNode);}}


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
    private void mergeEquivalences(ImplicationNode upNode, TreeSet<ImplicationNode> equivalences, TreeSet<Integer> trueLiterals) {
        TreeSet<ImplicationNode> upNodes = new TreeSet<>();
        TreeSet<ImplicationNode> downNodes = new TreeSet<>();
        for(ImplicationNode node : equivalences) {
            ArrayList<ImplicationNode> downs = node.downNodes;
            ArrayList<ImplicationNode> ups = node.upNodes;
            if(downs != null) {for(ImplicationNode down : downs) {if(down != upNode && !equivalences.contains(down)){downNodes.add(down);}}}
            if(ups   != null) {for(ImplicationNode   up : ups)   {if(up   != upNode && !equivalences.contains(up))  {upNodes.add(up);}}}}
        findTrueLiterals(upNode,upNodes,downNodes,trueLiterals);

        upNode.joinUps(upNodes);
        upNode.joinDowns(downNodes);
        for(ImplicationNode node : equivalences) {
            disconnect(node);
            node.upNodes = null;
            node.downNodes = null;}
    }

    /** checks if the upNodes and downNodes have contradictory predicates.
     *  the down-predicates become true predicates
     *
     * @param upNode    the upper entrance to a cycle
     * @param upNodes   the nodes leading into the cycle
     * @param downNodes  the nodes from the cycle
     * @param trueLiterals for inserting contradictory predicates as true predicates.
     */
    private void findTrueLiterals(ImplicationNode upNode,  TreeSet<ImplicationNode> upNodes,  TreeSet<ImplicationNode> downNodes, TreeSet<Integer> trueLiterals) {
        if(upNode.upNodes != null) {
            for(ImplicationNode up : upNode.upNodes) {
                if(containsLiteral(-up.literal,downNodes)) {trueLiterals.add(-up.literal);}}}
        if(upNode.downNodes != null) {
            for(ImplicationNode down : upNode.downNodes) {
                if(containsLiteral(-down.literal,upNodes)) {trueLiterals.add(down.literal);}}}
        for(ImplicationNode up : upNodes) {
            if(containsLiteral(-up.literal,downNodes)) {trueLiterals.add(-up.literal);}}}

    /** checks if literal occurs in nodes
     * The literal is removed from nodes.
     *
     * @param literal a literal
     * @param nodes a list of nodes
     * @return true if the literal occurs in nodes.
     */
    private boolean containsLiteral(int literal, TreeSet<ImplicationNode> nodes) {
        for(ImplicationNode node : nodes) {
            if(node.literal == literal) {
                nodes.remove(node);
                return true;}}
        return false;}



    /** applies the consumer to all nodes below/above the given literal
     *
     * @param literal  a literal
     * @param down     if true then the consumer is applied to all implied predicates, otherwise to all predicates which imply 'this'
     * @param consumer  a consumer function to be applied to predicates
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

    /** searches through the DAG (up/down) to find the first item where the function returns nut null
     *
     * @param literal  a literal
     * @param down     true for down, false for up
     * @param function to be applied to a literal
     * @return         the first object for which the function returns not null
     */
    public Object find(Integer literal, boolean down, Function<Integer,Object> function) {
        ImplicationNode node = nodesMap.get(literal);
        if(node != null) {return find(node,down,function);}
        else {return function.apply(literal);}}

    private Object find(ImplicationNode node, boolean down, Function<Integer,Object> function) {
        Object result = function.apply(node.literal);
        if(result != null) {return result;}
        ArrayList<ImplicationNode> nodes = down ? node.downNodes : node.upNodes;
        if(nodes != null) {
            for(ImplicationNode nextNode : nodes) {
                result = find(nextNode,down,function);
                if(result != null) {return result;}}}
        return null;}

    /** applies the consumer to all root predicates
     *
     * @param consumer to be applied to all root predicates
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
        disconnect(falseNode);
        ArrayList<ImplicationNode> downNodes = falseNode.downNodes;
        if(downNodes != null) {
            for(ImplicationNode node :downNodes) {
                if(node.upNodes == null) {
                    if(node.downNodes != null) {roots.add(node);}}}}
        roots.remove(falseNode);
        falseNode.downNodes = null;
        falseNode.upNodes = null;
    }

    /** checks if there is an implication -p -&gt; q, which is a positive clause
     *
     * @return true if there is a positive clause.
     */
    public boolean hasPositiveClause() {
        for(ImplicationNode node : roots) {if(hasPositiveClause(node)) {return true;}}
        return false;}

    /** checks recursively if there is a positive clause
     *
     * @param node the node to be checked
     * @return true if there is a positive clause.
     */
    public boolean hasPositiveClause(ImplicationNode node) {
        if(node.downNodes == null || node.downNodes.isEmpty()) {return false;}
        int literal = node.literal;
        for(ImplicationNode down : node.downNodes) {
            if((literal < 0 && down.literal > 0) || hasPositiveClause(down)) {return true;}}
        return false;}

    /** checks if there is an implication p -&gt; -q, which is a negative clause
     *
     * @return true if there is a positive clause.
     */
    public boolean hasNegativeClause() {
        for(ImplicationNode node : roots) {if(hasNegativeClause(node)) {return true;}}
        return false;}

    /** checks recursively if there is a negative clause
     *
     * @param node the node to be checked
     * @return true if there is a positive clause.
     */
    public boolean hasNegativeClause(ImplicationNode node) {
        if(node.downNodes == null || node.downNodes.isEmpty()) {return false;}
        int literal = node.literal;
        for(ImplicationNode down : node.downNodes) {
            if((literal > 0 && down.literal < 0) || hasNegativeClause(down)) {return true;}}
        return false;}

    /** completes the model such that all ID_Implications in the DAG become true.
     * Not all predicates need to be assigned truth values.
     * It is, however, not clear whether the model is minimal.
     * The DAG becomes empty by this operation.
     * The trueLiteralObservers are overwritten.
     *
     * @param model a partial model.
     */
    public void completeModel(Model model) {
        trueLiteralObservers.clear();
        trueLiteralObservers.add(literal-> {model.addImmediately(literal);});
        while(!roots.isEmpty()) {
            for(Object node : roots.toArray()) { // this guarantees that the implication: 'root -> node' is true.
                ++timestamp;
                newTrueLiteral((ImplicationNode)node);}}}



    /** calls all trueLiteralObservers
     *
     * @param trueLiteral a true literal
     */
    private void reportTrueLiteral(Integer trueLiteral) {
        for(Consumer<Integer> observer : trueLiteralObservers) {observer.accept(trueLiteral);}}

    /** calls all implicationObservers
     *
     * @param from  a literal node
     * @param to  a literal node
     */
    private void reportImplication(ImplicationNode from, ImplicationNode to) {
        for(BiConsumer<ImplicationNode,ImplicationNode> observer : implicationObservers) {observer.accept(from,to);}}


    /** checks the consistency of the DAG.
     * System.exit if it is inconsistent.
     */
    private void checkConsistency() {
        for(ImplicationNode node : nodesMap.values()) {
            if(node.upNodes != null) {
                for(ImplicationNode upNode : node.upNodes) {
                    if(upNode.downNodes == null || upNode.downNodes.isEmpty()) {
                        System.out.println("DAG Inconsistency: Node " + node.toString() + " has upNode " + upNode +", but this has no downNodes");
                        System.out.println(toString());
                        System.exit(1);}
                    if(!upNode.downNodes.contains(node)) {
                        System.out.println("DAG Inconsistency: Node " + node.toString() + " has upNode " + upNode +", but does not contain it in its downNodes");
                        System.out.println(toString());
                        System.exit(1);}}}

            if(node.downNodes != null) {
                for(ImplicationNode downNode : node.downNodes) {
                    if(downNode.upNodes == null || downNode.upNodes.isEmpty()) {
                        System.out.println("DAG Inconsistency: Node " + node.toString() + " has downNode " + downNode +", but this has no upNodes");
                        System.out.println(toString());
                        System.exit(1);}
                    if(!downNode.upNodes.contains(node)) {
                        System.out.println("DAG Inconsistency: Node " + node.toString() + " has downNode " + downNode +", but does not contain it in its upNodes");
                        System.out.println(toString());
                        System.exit(1);}}}}

        for(ImplicationNode node :roots) {
            if(node.upNodes != null && !node.upNodes.isEmpty()) {
                System.out.println("DAG Inconsistency: Root " + node.toString() + " has upNodes " + node.upNodes);}
        }

    }


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
