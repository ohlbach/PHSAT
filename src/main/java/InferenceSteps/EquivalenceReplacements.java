package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import org.apache.commons.lang3.StringUtils;

public class EquivalenceReplacements extends InferenceStep {
    private Clause oldClause;
    private Clause newClause;
    private int oldLiteral;
    private int newLiteral;
    private Clause equivalenceClause;

    public static String rule = "Replacement of Literals by equivalent ones\n"+
            "p,q,a,s,t   with (a == b)\n"+
            "---------\n"+
            "p,q,b,s,t";

    public EquivalenceReplacements(Clause oldClause, int oldLiteral, Clause newClause, int newLiteral, Clause equivalenceClause) {
        this.oldClause  = oldClause;
        this.oldLiteral = oldLiteral;
        this.newClause  = newClause;
        this.newLiteral = newLiteral;
        this.equivalenceClause = equivalenceClause;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String st = oldClause.toString(0,symboltable);
        int width = st.length();
        st += "  (" + Symboltable.toString(oldLiteral,symboltable) + " == " +
                Symboltable.toString(newLiteral,symboltable) + ")\n" +
                StringUtils.repeat('-',width) + "\n" +
                newClause.toString(0,symboltable);
        return st;}

    @Override
    public Clause[] input() {
        return new Clause[]{oldClause,equivalenceClause}; }

    @Override
    public Clause output() {
        return newClause;}
}
