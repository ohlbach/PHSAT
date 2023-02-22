package NormalFormTransformers;

import Datastructures.Clauses.Connective;
import Datastructures.Clauses.InputClauses;
import Datastructures.Symboltable;
import InferenceSteps.InfInputClause;
import InferenceSteps.InferenceStep;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

public class InfClauseToAtleast extends InferenceStep {
    private final int[] clause;
    private final int[] atleastClause;

    private static final String titleAtmost = "ANFNormalizer: Atmost To Atleast";
    private static final String titleExactly = "ANFNormalizer: Exactly To Atleast";
    private static final String titleInterval = "ANFNormalizer: Interval To Atleast";

    private String title;

    private static final String ruleAtmost = titleAtmost + ":\natmost k l1,...,ln -> atleast n-k -l1...-ln";

    private static final String ruleExactly = titleExactly +":\n" +
            "exactly k l1...ln\n"+
            "-----------------\n"+
            "atleast k l1...ln\n"+
            "atleast n-k -l1...-ln";

    private static final String ruleInterval = titleInterval +":\n" +
            "[min,max] l1...ln\n"+
            "-----------------\n"+
            "atleast min l1...ln\n"+
            "atleast n-max -l1...-ln";
    private String rule;




    public InfClauseToAtleast(int[] clause, int[] atleastClause) {
        this.clause = clause;
        this.atleastClause = atleastClause;
        Connective connective = Connective.getConnective(clause[1]);
        assert(connective != null);
        switch(connective) {
            case ATMOST:   rule = ruleAtmost;   title = titleAtmost;   break;
            case EXACTLY:  rule = ruleExactly;  title = titleExactly;  break;
            case INTERVAL: rule = ruleInterval; title = titleInterval; break;
        }
    }

    @Override
    public String title() {return title;}
    @Override
    public String rule() {return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + "\n" + InputClauses.toString(2, clause,symboltable) + " -> "+
                InputClauses.toString(2,atleastClause,symboltable);
    }

    @Override
    public IntArrayList inputClauseIds() {
        return IntArrayList.wrap(new int[]{clause[0]});}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(steps != null) {
            steps.add(new InfInputClause(clause[0]));
            steps.add(this);}}
}

