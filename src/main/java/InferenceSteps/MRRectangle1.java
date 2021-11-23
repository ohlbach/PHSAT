package InferenceSteps;

import Datastructures.Clauses.MRMatrix;
import Datastructures.Literals.CLiteralOld;
import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Formatter;
import java.util.Locale;

import static Utilities.Utilities.joinIntArrays;

public class MRRectangle1 extends InferenceStep {

    public static final String title = "Multi-Resolution of Unit Clause";

    public static String rule = "Multi-Resolution of Unit Clause\nExample:\n" +
            "Ci horizontal: clauses (disjunctions)\n" +
            "Di vertical:   disjointness clauses\n\n" +
            "    D1  D2  D3\n" +
            "C1:  1   2   3\n" +
            "C2:  4   5   6  p\n" +
            "C3:  7   8   9\n" +
            "C4: 10  11  12\n" +
            "--------------\n" +
            "      p\n" +
            "In each clause Ci one literal must be true.\n" +
            "If p is false then there must be two true literals in at least one column.\n" +
            "This contradictions the disjointness of the literals in the column.\n"+
            "Therefore p must be true.";

    private final MRMatrix mrMatrix;
    private final int[] colIndices;
    private final int literal;
    private final ArrayList<CLiteralOld[]> block;

    public MRRectangle1(MRMatrix mrMatrix, int[] colIndices, int literal, ArrayList<CLiteralOld[]> block) {
        this.mrMatrix = mrMatrix;
        this.colIndices = colIndices;
        this.literal = literal;
        this.block = block;}

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + ":\n" + block2String(symboltable);}


    private String block2String(Symboltable symboltable) {
        int width = 0;
        for(int colIndex : colIndices) {
            width = Math.max(width,Integer.toString(mrMatrix.disjointnessClauses[colIndex].id).length());}
        for(CLiteralOld[] row : block) {
            for(int j = 0; j < row.length-1; ++j) {
                CLiteralOld cLiteral = row[j];
                width = Math.max(width, cLiteral == null ? 0 :
                        Symboltable.toString(cLiteral.literal, symboltable).length());}}

        StringBuilder st = new StringBuilder();
        int lineLength = 0;
        for (CLiteralOld[] row : block) {
            StringBuilder line = new StringBuilder();
            Formatter fLine = new Formatter(line, Locale.GERMANY);
            fLine.format("%" + width + "s:", mrMatrix.getClause(row).id);
            for (int j = 0; j < row.length; ++j) {
                CLiteralOld cLiteral = row[j];
                fLine.format("%" + width + "s|", cLiteral == null ? " " :
                        Symboltable.toString(cLiteral.literal, symboltable));}
            String ln = line.toString();
            lineLength = Math.max(lineLength, ln.length());
            st.append(ln).append("\n");}

        return title + ":\n" + st + "\n" +
                StringUtils.repeat('-',lineLength) + "\n" +
                StringUtils.center(Symboltable.toString(literal,symboltable),lineLength);}

    @Override
    public IntArrayList origins() {
        if(!mrMatrix.trackReasoning) return null;
        InferenceStep step;
        IntArrayList origins = new IntArrayList();
        for (int index : colIndices) {
            step = mrMatrix.disjointnessClauses[index].inferenceStep;
            if (step != null) origins = joinIntArrays(origins, step.origins());}
        for(CLiteralOld[] row : block) {
            for(CLiteralOld cLiteral : row) {
                if(cLiteral != null) {
                    step = cLiteral.clause.inferenceStep;
                    if(step != null) origins = joinIntArrays(origins,step.origins());}}}
        return origins;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(!mrMatrix.trackReasoning) return;
        InferenceStep step;
        for (int index : colIndices) {
            step = mrMatrix.disjointnessClauses[index].inferenceStep;
            if (step != null) step.inferenceSteps(steps);}
        for(CLiteralOld[] row : block) {
            for(CLiteralOld cLiteral : row) {
                if(cLiteral != null) {
                    step = cLiteral.clause.inferenceStep;
                    if(step != null) step.inferenceSteps(steps);}}}
        if(!steps.contains(this)) steps.add(this);}

}
