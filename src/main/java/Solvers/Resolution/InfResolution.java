package Solvers.Resolution;

import Datastructures.Symboltable;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfResolution extends InferenceStep {

    private String title = "Resolution";
    @Override
    public String title() {return title;}
    private String ruleAtleast = title + "\n  >= n p^k,phi and >= m -p^l,psi -> >= n+m-max(k,l) phi,psi";
    private String ruleOr = title + "\n  p,phi and -p,psi -> phi,psi";

    private String rule;
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
        if(parentClause1.isDisjunction && parentClause2.isDisjunction) rule = ruleOr;
        else rule = ruleAtleast;
    }

    public InfResolution(Clause parentClause1, String parentClause1String, Clause parentClause2, String parentClause2String, Clause resolvent, Symboltable symboltable) {
        this.parentClause1 = parentClause1String;
        this.parentClause2 = parentClause2String;
        this.resolvent     = resolvent.toString(symboltable,0);
        inferenceStep1 = parentClause1.inferenceStep;
        inferenceStep2 = parentClause2.inferenceStep;
        if(parentClause1.isDisjunction && parentClause2.isDisjunction) rule = ruleOr;
        else rule = ruleAtleast;
    }



    @Override
    public String toString(Symboltable symboltable) {
        return title+"\n  "+ parentClause1 + " and " + parentClause2 + " -> " + resolvent;}

    @Override
    public void inputClauseIds(IntArrayList ids) {
        inferenceStep1.inputClauseIds(ids);
        inferenceStep2.inputClauseIds(ids);}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(inferenceStep1 != null) inferenceStep1.inferenceSteps(steps);
        if(inferenceStep2 != null) inferenceStep2.inferenceSteps(steps);
        steps.add(this);}
}
