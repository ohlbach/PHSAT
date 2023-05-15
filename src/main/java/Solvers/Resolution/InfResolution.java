package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfResolution extends InferenceStep {

    private String title = "Resolution";
    @Override
    public String title() {return title;}

    private String rule = ">= n p^k,phi and >= m -p^l,psi -> >= n+m-max(k,l) phi,psi";
    @Override
    public String rule() {return rule;}

    private String parentClause1,parentClause2,resolvent;
    private InferenceStep inferenceStep1,inferenceStep2;

    public InfResolution(Clause parentClause1, Clause parentClause2, Clause resolvent, Symboltable symboltable) {
        this.parentClause1 = parentClause1.toString(symboltable,0);
        this.parentClause2 = parentClause2.toString(symboltable,0);
        this.resolvent     = resolvent.toString(symboltable,0);
        inferenceStep1 = parentClause1.inferenceStep;
        inferenceStep2 = parentClause2.inferenceStep;
    }

    public InfResolution(Clause parentClause1, String parentClause1String, Clause parentClause2, String parentClause2String, Clause resolvent, Symboltable symboltable) {
        this.parentClause1 = parentClause1String;
        this.parentClause2 = parentClause2String;
        this.resolvent     = resolvent.toString(symboltable,0);
        inferenceStep1 = parentClause1.inferenceStep;
        inferenceStep2 = parentClause2.inferenceStep;
    }



    @Override
    public String toString(Symboltable symboltable) {
        return parentClause1 + " and " + parentClause2 + " -> " + resolvent;}

    @Override
    public IntArrayList inputClauseIds() {
        IntArrayList list = (inferenceStep1 == null|| inferenceStep1.inputClauseIds() == null) ? new IntArrayList() : inferenceStep1.inputClauseIds().clone();
        if(inferenceStep2 != null) list.addAll(inferenceStep2.inputClauseIds());
        return list;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(inferenceStep1 != null) inferenceStep1.inferenceSteps(steps);
        if(inferenceStep2 != null)inferenceStep2.inferenceSteps(steps);}
}
