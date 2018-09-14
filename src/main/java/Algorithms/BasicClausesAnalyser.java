package Algorithms;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Clauses.ClauseList;
import Datastructures.Literals.CLiteral;
import Datastructures.LocalModel;
import Datastructures.Theory.ImplicationGraph;
import Datastructures.TrueLiterals;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Created by ohlbach on 11.09.2018.
 */
public class BasicClausesAnalyser {
    private Function<Integer,CLiteral> literalCreator;
    private BiFunction<Integer,ArrayList<CLiteral>,Clause> clauseCreator;
    private ClauseList clauseList;
    TrueLiterals trueLiterals;
    LocalModel model;
    boolean withImplications;
    Thread thread;
    ImplicationGraph implicationGraph;

    public BasicClausesAnalyser(Function<Integer,CLiteral> literalCreator,
                                BiFunction<Integer,ArrayList<CLiteral>,Clause> clauseCreator,
                                ClauseList clauseList,TrueLiterals trueLiterals, LocalModel model) {
        this.literalCreator = literalCreator;
        this.clauseCreator = clauseCreator;
        this.clauseList = clauseList;
        this.trueLiterals = trueLiterals;
        this.model = model;
        thread = Thread.currentThread();

    }

    public boolean analyse(BasicClauseList basicClauses, boolean withImplications) {
        this.withImplications = withImplications;
        if(withImplications) {
            implicationGraph = new ImplicationGraph(basicClauses.predicates);
            clauseList.implicationGraph = implicationGraph;}
        if(clearClauses(basicClauses.clauses)) {return true;}
        while(!trueLiterals.isEmpty()) {
            if(thread.isInterrupted()) {return true;}
            if(unitPropagate(trueLiterals.poll())) {return true;}}
        if(withImplications && integrateTwoLiteralClauses()) {return true;}
        return false;
    }

    /** removes double literals and tautologies from a clause list.
     * Unit clauses are extracted.
     *
     * @param clauses an ArrayList if clauses (as int-Array).
     * @return the cleared Clauses
     */
    public  boolean  clearClauses(ArrayList<int[]> clauses) {
        ArrayList<int[]> disjointnessClauses = new ArrayList<>();
        for(int c = 0; c < clauses.size(); ++c) {
            if(thread.isInterrupted()) {return true;}
            int[] clause = clauses.get(c);
            if(clause[0] == 0) {disjointnessClauses.add(clause); continue;}
            ArrayList<CLiteral> cl = new ArrayList<>();
            boolean tautology = false;
            for(int literal : clause) {
                if(cl.contains(literal)  || model.isFalse(literal)) {continue;}
                if(cl.contains(-literal) || model.isTrue(literal)) {tautology = true; break;}
                CLiteral cLiteral = literalCreator.apply(literal);
                cl.add(cLiteral);}
            if(tautology) {continue;}
            switch(cl.size()) {
                case 0: model.signalEmptyClause(c); return true;
                case 1:
                    int unit = cl.get(0).literal;
                    if(trueLiterals.addLocalLiteral(unit)) {return true;}
                    break;
                default:
                    clauseList.addClause(clauseCreator.apply(c+1,cl));}}

       if(disjointnessClauses.isEmpty()) {return false;}
        disjointnessClauses.get(0)[0] = clauses.size();
        for(int i = 0; i < disjointnessClauses.size(); ++i) {
            int[] clause = disjointnessClauses.get(i);
            if(i > 0) {clause[0] = disjointnessClauses.get(i-1)[0];}
            boolean stopped = withImplications ? add2ImplicationGraph(clause) : expandDisjointnessClause(clause);
            if(stopped) {return true;}}
        return false;}

    boolean expandDisjointnessClause(int[] clause) {
        if(checkDisjointnessClause(clause)) {return true;}
        int size = clause.length;
        for(int l1 = 1; l1 < size; ++l1) {
            int literal1 = clause[l1];
            for(int l2 = l1+1; l2 < size; ++l2) {
                ArrayList<CLiteral> lits = new ArrayList<>();
                CLiteral cLiteral1 = literalCreator.apply(-literal1);
                CLiteral cLiteral2 = literalCreator.apply(-clause[l2]);
                lits.add(cLiteral1); lits.add(cLiteral2);
                clauseList.addClause(clauseCreator.apply(clause[0]++,lits));}}
        return false;}

    boolean add2ImplicationGraph(int[] clause) {
        if(checkDisjointnessClause(clause)) {return true;}
        int size = clause.length;
        for(int l1 = 1; l1 < size; ++l1) {
            int literal1 = clause[l1];
            for(int l2 = 1; l2 < size; ++l2) {
                if(l1 == l2) {continue;}
                HashSet<Integer>  units = implicationGraph.addImplication(literal1,-clause[l2]);
                if(units != null) {
                    for(int literal : units) {if(trueLiterals.addLocalLiteral(literal)) {return true;}}}}}
        return false;}

    boolean checkDisjointnessClause(int[] clause) {
        int size = clause.length;
        for(int l1 = 1; l1 < size; ++l1) {
            int literal1 = clause[l1];
            if(model.isTrue(literal1)) {
                for(int l2 = 1; l2 < size; ++l2) {
                    if(l1 == l2) {continue;}
                    if(trueLiterals.addLocalLiteral(-clause[l2])) {return true;}}}}
        return false;}

    boolean unitPropagate(int literal) {
        for(CLiteral cliteral : clauseList.literalIndex.getLiterals(literal)) {
            clauseList.removeClause(cliteral.getClause());}
        for(CLiteral cliteral : clauseList.literalIndex.getLiterals(-literal)) {
            Clause clause = cliteral.getClause();
            clauseList.removeLiteral(cliteral);
            if(clause.size() == 1) {
                if(trueLiterals.addLocalLiteral(clause.cliterals[0].literal)) {return true;}
                clauseList.removeClause(clause);}}
        if(clauseList.isEmpty()) {model.signalEmptyClauseList(); return true;}
        return false;}

    boolean integrateTwoLiteralClauses() {
        ArrayList<Clause> clauses = new ArrayList<>();
        for(Clause clause : clauseList.clauses) {if(clause.size() == 2) {clauses.add(clause);}}
        for(Clause clause : clauses) {if(integrateTwoLiteralClause(clause)) {return true;}}
        while(!trueLiterals.isEmpty()) {
            if(thread.isInterrupted()) {return true;}
            if(unitPropagate(trueLiterals.poll())) {return true;}}
        return false;}


    boolean integrateTwoLiteralClause(Clause clause) {
        HashSet<Integer>  units = implicationGraph.addClause(clause.cliterals[0].literal,clause.cliterals[1].literal);
        if(units != null) {for(int literal : units) {if(trueLiterals.addLocalLiteral(literal)) {return true;}}}
        clauseList.removeClause(clause);
        return false;}


}
