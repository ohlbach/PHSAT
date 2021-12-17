package Datastructures.Clauses;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import InferenceSteps.Input;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import Utilities.Utilities;

import java.util.ArrayList;

public class InfAtmostToAtleast extends InferenceStep {
    public static final String title ="Atmost to Atleast";
    public static final String rule = "atmost n l_1,...,l_k -> atleast k-n -l_1,...,-l_k";

    private int[] basicClause = null;
    private IntArrayList literals;
    private int id;
    private short limit;
    private final Clause clause;

    public InfAtmostToAtleast(int[] basicClause, Clause clause) {
        this.basicClause = basicClause;
        this.clause = clause;
        id = basicClause[0];}

    public InfAtmostToAtleast(int id, short limit, IntArrayList literals , Clause clause) {
        this.id = id;
        this.limit = limit;
        this.literals = literals;
        this.clause = clause;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return title + ":\n"+rule;}

    @Override
    public String toString(Symboltable symboltable) {
        if (basicClause != null)
            return BasicClauseList.clauseToString(0, basicClause, symboltable) + " -> " +
                    clause.toString(0, symboltable);
        return id + ": atmost " + limit + ": " + Utilities.intArrayListToString(literals,
                ((Integer literal) -> Symboltable.toString(literal,symboltable))) + " -> " +
                clause.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        return IntArrayList.wrap(new int[]{id});}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        steps.add(new Input(id));
        steps.add(this);}
}
