package Solvers.Walker;

import Datastructures.Clauses.Clause;
import Datastructures.Results.Result;
import Datastructures.Statistics.Statistic;
import Datastructures.Theory.Model;
import Management.ProblemSupervisor;
import Solvers.Solver;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
import java.util.HashMap;

public class Walker extends Solver {

    private final ArrayList<WClause> wClauses = new ArrayList<>();
    private final ArrayList<WClause>[] posOccurrences;
    private final ArrayList<WClause>[] negOccurrences;
    private final int[] posScores;
    private final int[] negScores;
    private IntArrayList quantifiers;
    private int maxQuantifier;



    /** constructs a new Walker solver.
     *
     * @param solverNumber         for distinguishing different solvers of the same type, but different parameters
     * @param solverParameters     contains the parameters for controlling the solver
     * @param problemSupervisor    coordinates several solvers.
     */
    public Walker(Integer solverNumber, HashMap<String,Object> solverParameters, ProblemSupervisor problemSupervisor) {
        super(solverNumber,solverParameters, problemSupervisor);
        posOccurrences = new ArrayList[predicates+1];
        negOccurrences = new ArrayList[predicates+1];
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            posOccurrences[predicate] = new ArrayList<>();
            negOccurrences[predicate] = new ArrayList<>();}
        posScores = new int[predicates+1];
        negScores = new int[predicates+1];
    }

    public void addClause(Clause clause) {
        WClause wClause = new WClause(clause);
        wClauses.add(wClause);
        addWClauseToIndex(wClause);
        if(!quantifiers.contains(wClause.quantifier)) quantifiers.add(wClause.quantifier);
    }

    @Override
    public Result solve() {

        return null;
    }

    private void setInitialScores() {
        int productQuantifier = productQuantifier();
        for(WClause wClause: wClauses) {
            switch(wClause.clauseType) {
                case OR:
                    for(int literal : wClause.literals) {
                        if(literal > 0) posScores[literal] += productQuantifier;
                        else            negScores[literal] += productQuantifier;}
                    break;
                case ATLEAST:
                    int score = productQuantifier / wClause.quantifier;
                    for(int literal : wClause.literals) {
                        if(literal > 0) posScores[literal] += score;
                        else            negScores[literal] -= score;}
                    break;
                case ATMOST:
                    

            }
        }
    }

    private int productQuantifier() {
        int product = 1;
        for(int q : quantifiers) product *= q;
        return product;}

    private void addWClauseToIndex(WClause wClause) {
        for(int literal : wClause.literals) {
            if(literal > 0) posOccurrences[literal].add(wClause);
            else             posOccurrences[-literal].add(wClause);}}

    private ArrayList<WClause> getClauses(int literal) {
        return (literal > 0) ? posOccurrences[literal] : negOccurrences[-literal];}



    @Override
    public Statistic getStatistics() {
        return null;
    }
}
