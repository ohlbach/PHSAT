package Coordinator;

import Datastructures.Clauses.Clause;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.ImplicationGraph;
import Datastructures.Theory.Model;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Created by ohlbach on 20.09.2018.
 */
public class EquivalenceClasses {
    private int predicates;
    public ClauseList equivalenceClasses = null;
    private HashMap<Integer,Integer> replacements = null;
    private Model model;
    private ImplicationGraph implicationGraph;
    public ArrayList<Consumer<Integer>> trueLiteralObservers = new ArrayList();
    public ArrayList<Consumer<Unsatisfiable>> unsatisfiabilityObservers = new ArrayList();
    public ArrayList<BiConsumer<Integer,Integer>> equivalenceObservers = new ArrayList();

    public EquivalenceClasses(Model model, ImplicationGraph implicationGraph) {
        this.model = model;
        this.implicationGraph = implicationGraph;
        this.predicates = model.predicates;
        equivalenceClasses = new ClauseList(5,predicates);
        if(implicationGraph != null) {
            implicationGraph.implicationObservers.add((from,to) -> {
                if(implicationGraph.implies(to,from)) {addEquivalence("E"+from+"="+to, from,to);}});}
        };




    /** turns a basicClause into a clause. <br/>
     * A true literal causes all other literals to become true <br/>
     * A false literal causes all other literals to become false <br/>
     * p &lt;=&gt; -p is a contradiction.<br/>
     * A double literal p,p is ignored.<br/>
     * p,q,r and p -&gt; not r  causes all literals to become false.
     *
     * @param basicClause
     * @return either an Unsatisfiable object, or null.
     */
    public void addEquivalenceClass(int[] basicClause) {
        assert basicClause.length > 2;
        if(basicClause.length == 3) {return;}
        String id = ""+basicClause[0];
        for(int i = 2; i < basicClause.length; ++i) {
            int literal = mapToRepresentative(basicClause[i]);
            if(model.isTrue(literal)) {
                for(int j = 2; j < basicClause.length; ++j) {
                    if(i != j) {reportTrueLiteral(mapToRepresentative(basicClause[j]));}}
                return;}

            if(model.isFalse(literal)) {
                for(int j = 2; j < basicClause.length; ++j) {
                    if(i != j) {reportTrueLiteral(-mapToRepresentative(basicClause[j]));}}}
            return;}

        int representative = basicClause[2];
        for(int i = 3; i < basicClause.length; ++i) {addEquivalence(id,representative,basicClause[i]);}}


    private void addEquivalence(String id, int literal1, int literal2) {
        literal1 = mapToRepresentative(literal1);
        literal2 = mapToRepresentative(literal2);
        if(literal1 == literal2) {return;}
        if(literal1 == -literal2) {reportUnsatisfiable(literal1,literal2); return;}
        if(equivalenceClasses == null) {
            equivalenceClasses = new ClauseList(5,predicates);
            replacements = new HashMap<>();}

        Clause eqClass = null;
        int representative = literal1;
        int literal = literal2;
        LinkedList<CLiteral> classes = equivalenceClasses.getLiterals(literal1);
        if(!classes.isEmpty()) {eqClass = classes.getFirst().clause;}
        else {
            classes = equivalenceClasses.getLiterals(literal2);
            if(!classes.isEmpty()) {
                representative = literal2; literal = literal1;
                eqClass = classes.getFirst().clause;}
            else {eqClass = new Clause(id,2);
                eqClass.addCLiteralDirectly(new CLiteral(representative));
                equivalenceClasses.addClause(eqClass);}}

        eqClass.addCLiteralDirectly(new CLiteral(literal));
        if(implicationGraph != null) {
            for(CLiteral cLiteral : eqClass.cliterals) {
                if(implicationGraph.implies(cLiteral.literal,-literal)) {makeClassFalse(eqClass); return;}}}
        addReplacement(literal,representative);
        for(BiConsumer<Integer,Integer> observer : equivalenceObservers) {observer.accept(representative,literal);}
    }



    private void addReplacement(int literal, int representative) {
        replacements.put(literal,representative);
        replacements.put(-literal,-representative);
    }

    /** maps literals to their representative in the equivalence class.
     *
     * @param literal the literal
     * @return either the literal itself, or its representative.
     */
    public int mapToRepresentative(int literal) {
        Integer replaced = replacements.get(literal);
        return (replaced == null) ? literal : replaced;}

    private void makeClassFalse(Clause eqClass) {
        for(CLiteral cLiteral: eqClass.cliterals) {
            reportTrueLiteral(-cLiteral.literal);
            replacements.remove(cLiteral.literal);
            replacements.remove(-cLiteral.literal);}
        equivalenceClasses.removeClause(eqClass);}

    private void reportTrueLiteral(int literal) {
        for(Consumer<Integer> observer : trueLiteralObservers) {observer.accept(literal);}}

    private void reportUnsatisfiable(int literal1 , int literal2) {
        for(Consumer<Unsatisfiable> observer : unsatisfiabilityObservers) {
            observer.accept(new Unsatisfiable("Equivalence " + literal1 + " = " +literal2 + " is false."));}}

    public String toString() {return toString(null);}

    public String toString(Symboltable symboltable) {
        return "Equivalence Classes:\n" + equivalenceClasses.toString(symboltable);
    }

}
