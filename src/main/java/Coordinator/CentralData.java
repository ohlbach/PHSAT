package Coordinator;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.Theory.ImplicationGraph;
import Datastructures.Theory.Model;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.function.Consumer;

/**
 * Created by ohlbach on 12.09.2018.
 */
public class CentralData {
    private BasicClauseList basicClauseList;
    private int predicates;
    private ClauseList clauseList;
    private Model model;
    private ImplicationGraph implicationGraph;
    private int emptyClause = 0;
    private Incoming incoming = new Incoming();
    private Outgoing outgoing = new Outgoing();

    public CentralData(BasicClauseList basicClauseList) {
        this.basicClauseList = basicClauseList;
        predicates = basicClauseList.predicates;
        clauseList = new ClauseList(predicates,basicClauseList.symboltable);
        model = new Model(predicates);
        implicationGraph = new ImplicationGraph(predicates);
    }

    public int processClauses() {
        transferClauses();
        simplifyClauses();
        return 0;
    }

    public void processChanges() {
        while(true) {
            Object[] income = incoming.getIncoming();
            processChange((int[])income[0],(int[][])income[1]);}
    }

    private void processChange(int[] ones, int[][] twos) {

    }



    int transferClauses() {
        ArrayList<int[]> disjointnessClauses = new ArrayList<>();
        for(int c = 0; c < basicClauseList.clauses.size(); ++c) {
            int[] clause = basicClauseList.clauses.get(c);
            if(clause[0] == 0) {disjointnessClauses.add(clause); continue;}
            ArrayList<CLiteral> cl = new ArrayList<>();
            boolean tautology = false;
            for(int literal : clause) {
                if(cl.contains(literal)  || model.isFalse(literal)) {continue;}
                if(cl.contains(-literal) || model.isTrue(literal)) {tautology = true; break;}
                CLiteral cLiteral = new CLiteral(literal);
                cl.add(cLiteral);}
            if(tautology) {continue;}
            switch(cl.size()) {
                case 0: emptyClause = c+1; return -1;
                case 1:
                    int unit = cl.get(0).literal;
                    if(model.add(unit) == -1) {emptyClause = c+1; return -1;}
                    break;
                case 2:
                    HashSet<Integer> units = implicationGraph.addImplication(cl.get(0).literal,cl.get(1).literal);
                    if(units != null) {
                        for(int literal : units) {
                            if(model.add(literal) == -1) {return -1;}}}
                default:
                    clauseList.addClause(new Clause(c+1,cl));}}

         if(clauseList.isEmpty()) {return 1;}  // Model for disjointness clauses.
         for(int i = 0; i < disjointnessClauses.size(); ++i) {
            int[] clause = disjointnessClauses.get(i);
            if(i > 0) {clause[0] = disjointnessClauses.get(i-1)[0];}
            boolean contradiction = add2ImplicationGraph(clause);
            if(contradiction) {return -1;}}

        return 0;}

    boolean add2ImplicationGraph(int[] clause) {
        if(checkDisjointnessClause(clause)) {return true;}
        int size = clause.length;
        for(int l1 = 1; l1 < size; ++l1) {
            int literal1 = clause[l1];
            for(int l2 = 1; l2 < size; ++l2) {
                if(l1 == l2) {continue;}
                HashSet<Integer> units = implicationGraph.addImplication(literal1,-clause[l2]);
                if(units != null) {
                    for(int literal : units) {
                        if(model.add(literal) == -1) {return true;}}}}}
        return false;}

    boolean checkDisjointnessClause(int[] clause) {
        int size = clause.length;
        for(int l1 = 1; l1 < size; ++l1) {
            int literal1 = clause[l1];
            if(model.isTrue(literal1)) {
                for(int l2 = 1; l2 < size; ++l2) {
                    if(l1 == l2) {continue;}
                    if(model.add(-clause[l2]) == -1) {return true;}}}}
        return false;}

    int simplifyClauses() {
        ArrayList<Integer> units = model.getModel();
        for(int i = 0; i < units.size(); ++i) {

        }
        return 0;
    }

    public  void addOne(int literal) {
        if(incoming.addOne(literal)) {}; // contradiction
    }

    public  void addTwo(int[] clause) {incoming.addTwo(clause);}

    public  void addTwo(int literal1, int literal2) {
        int[] clause = new int[]{literal1,literal2};
        incoming.addTwo(clause);}

    public void addObserver(Consumer<ChangeBlock> observer) {outgoing.addObserver(observer);}

}
