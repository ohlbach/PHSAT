package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.joinIntArrays;

/** documents a replacement resolution step
 */
public class ReplacementResolution extends InferenceStep{
    private final Clause parentClause1;
    private final Clause parentClause2;
    private final int literal;
    private final Clause resolvent;

    public static String title = "Replacement Resolution";

    public static String rule = title + ":\n" +
            " p,q,r,...,s\n"+
            "-p,q,r,...,s,t\n"+
            "---------------\n"+
            "   q,r,...,s,t";

    /** creates a replacement resolution step
     *
     * @param parentClause1  a clause
     * @param parentClause2  a corresponding clause
     * @param literal       the resolution literal
     * @param resolvent     the resolvent
     */
    public ReplacementResolution(Clause parentClause1, Clause parentClause2, int literal, Clause resolvent) {
        this.parentClause1 = parentClause1;
        this.parentClause2 = parentClause2;
        this.literal = literal;
        this.resolvent = resolvent;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String s1 = parentClause1.toString(0,symboltable);
        String s2 = parentClause2.toString(0,symboltable);
        return title + ":\n" + s1 + "\n" + s2 + " at literal " +
                Symboltable.toString(literal,symboltable) +"\n" +
                StringUtils.repeat('-',Math.max(s1.length(),s2.length())) + "\n"+
                resolvent.toString(0,symboltable);}

    @Override
    public Clause[] input() {
        return new Clause[]{parentClause1,parentClause2};}

    @Override
    public Clause output() {
        return resolvent;}

    @Override
    public IntArrayList origins() {
        return  joinIntArrays(parentClause1.inferenceStep.origins(), parentClause2.inferenceStep.origins());}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        parentClause1.inferenceStep.inferenceSteps(steps);
        parentClause2.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
