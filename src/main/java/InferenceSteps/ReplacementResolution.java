package InferenceSteps;

import Datastructures.Clauses.Clause;
import Datastructures.Symboltable;
import Datastructures.TwoLiteral.TwoLitClause;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

import static Utilities.Utilities.*;

/** documents a replacement resolution step
 */
public class ReplacementResolution extends InferenceStep{
    private Clause parentClause1 = null;
    private TwoLitClause parent2Clause1 = null;
    private final Clause parentClause2;
    private TwoLitClause twoClause = null;
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
    public ReplacementResolution(Clause parentClause1, Clause parentClause2, TwoLitClause twoClause, int literal, Clause resolvent) {
        this.parentClause1 = parentClause1;
        this.parentClause2 = parentClause2;
        this.twoClause = twoClause;
        this.literal = literal;
        this.resolvent = resolvent;}

    /** creates a replacement resolution step
     *
     * @param parent2Clause1  a clause
     * @param parentClause2  a corresponding clause
     * @param literal       the resolution literal
     * @param resolvent     the resolvent
     */
    public ReplacementResolution(TwoLitClause parent2Clause1, Clause parentClause2, int literal, Clause resolvent) {
        this.parent2Clause1 = parent2Clause1;
        this.parentClause2 = parentClause2;
        this.literal = literal;
        this.resolvent = resolvent;}


    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        String s1 = parentClause1 != null ? parentClause1.toString(0,symboltable) : parent2Clause1.toString("",symboltable);
        String s2 = parentClause2.toString(0,symboltable);
        String s3 = twoClause == null ? "" : twoClause.toString("",symboltable) + "\n";
        return title + ":\n" + s1 + "\n" + s2 + " at literal " +
                Symboltable.toString(literal,symboltable) + "\n" + s3 +
                StringUtils.repeat('-',Math.max(s1.length(),s2.length())) + "\n"+
                resolvent.toString(0,symboltable);}

    @Override
    public IntArrayList origins() {
        IntArrayList origins = parentClause2.inferenceStep == null ? null : parentClause2.inferenceStep.origins();
        if(twoClause != null && twoClause.inferenceStep != null)
            origins = joinIntArrays(origins,twoClause.inferenceStep.origins());
        if(parentClause1 != null)
             origins = joinIntArrays(origins,parentClause1.inferenceStep == null ? null : parentClause1.inferenceStep.origins());
        else origins = joinIntArrays(origins,parent2Clause1.inferenceStep == null ? null : parent2Clause1.inferenceStep.origins());
        return  origins;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(parentClause1 != null) {
            if(parentClause1.inferenceStep != null) parentClause1.inferenceStep.inferenceSteps(steps);}
        else {if(parent2Clause1.inferenceStep != null) parent2Clause1.inferenceStep.inferenceSteps(steps);}
        if(parentClause2.inferenceStep != null) parentClause2.inferenceStep.inferenceSteps(steps);
        if(twoClause != null && twoClause.inferenceStep != null) twoClause.inferenceStep.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
