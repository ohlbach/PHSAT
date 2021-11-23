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

public class MRSquare1 extends InferenceStep{
    public static String title = "Multi-Resolution of Unit Clause with Square Matrix";

    public static String rule = "Multi-Resolution of Unit Clause with Square Matrix\nExample:\n" +
            "Ci horizontal: clauses (disjunctions)\n" +
            "Di vertical:   disjointness clauses\n\n" +
            "   D1  D2  D3\n" +
            "C1: 1   2   3\n" +
            "C2: 4   5   6\n" +
            "C3: 7   8   9\n" +
            "       10\n" +
            "-------------\n" +
            "      -10\n" +
            "In each clause Ci one literal must be true.\n" +
            "Therefore one of the first three literals of each disjointness clause Di is true.\n" +
            "All the remaining disjoint literals must therefore be false.";

    private final MRMatrix mrMatrix;
    private final int[] colIndices;
    private final ArrayList<CLiteralOld[]> block;
    private final CLiteralOld dLiteral;
    private final int colIndex;

    public MRSquare1(MRMatrix mrMatrix, CLiteralOld dLiteral, int colIndex, int[] colIndices, ArrayList<CLiteralOld[]> block) {
        this.mrMatrix = mrMatrix;
        this.dLiteral = dLiteral;
        this.colIndex = colIndex;
        this.colIndices = colIndices;
        this.block = block;
    }

    @Override
    public String title() {
        return title;}

    @Override
    public String rule() {
        return rule;}

    @Override
    public String toString(Symboltable symboltable) {
        return title + ":\n" + block2String(colIndices,block,symboltable);}

    private String block2String(int[] colIndices, ArrayList<CLiteralOld[]> block, Symboltable symboltable) {
        int width = 0;
        for(int colIndex : colIndices) {
            width = Math.max(width,Integer.toString(mrMatrix.disjointnessClauses[colIndex].id).length());}
        for(CLiteralOld[] row : block) {
            for(int j = 0; j < row.length-1; ++j) {
                CLiteralOld cLiteral = row[j];
                width = Math.max(width, cLiteral == null ? 0 :
                    Symboltable.toString(cLiteral.literal, symboltable).length());}}

        StringBuilder st = new StringBuilder();
        Formatter format = new Formatter(st, Locale.GERMANY);
        int lineLength = 0;
        for (CLiteralOld[] row : block) {
            StringBuilder line = new StringBuilder();
            Formatter fLine = new Formatter(line, Locale.GERMANY);
            fLine.format("%" + width + "s:", mrMatrix.getClause(row).id);
            for (int j = 0; j < row.length - 1; ++j) {
                CLiteralOld cLiteral = row[j];
                fLine.format("%" + width + "s|", cLiteral == null ? " " :
                        Symboltable.toString(cLiteral.literal, symboltable));
            }
            String ln = line.toString();
            lineLength = Math.max(lineLength, ln.length());
            st.append(ln).append("\n");
        }
        format.format("%"+width+"s:"," ");
        for(int i = 0; i < colIndices.length; ++i) {
            format.format("%"+width+"s|",i != colIndex ? " " :
                    Symboltable.toString(dLiteral.literal,symboltable));}
        st.append("\n").append(StringUtils.repeat('-',lineLength)).append("\n");

        format.format("%"+width+"s "," ");
        for(int i = 0; i < colIndices.length; ++i) {
            format.format("%"+width+"s ",i != colIndex ? " " :
                    Symboltable.toString(-dLiteral.literal,symboltable));}

        String body = st.toString();
        st = new StringBuilder();
        st.append("Multi-Resolution Square of size ").append(colIndices.length).append(":\n");
        format = new Formatter(st, Locale.GERMANY);
        format.format("%"+width+"s:"," ");
        for(int colIndex : colIndices) {
            format.format("%"+width+"s|", mrMatrix.disjointnessClauses[colIndex].id);}
        st.append("\n");
        st.append(StringUtils.repeat('-',lineLength)).append("\n");
        return st + body;}

    @Override
    public IntArrayList origins() {
        if(!mrMatrix.trackReasoning) return null;
        InferenceStep step;
        IntArrayList origins = new IntArrayList();
        for (int index : colIndices) {
            step = mrMatrix.disjointnessClauses[index].inferenceStep;
            if (step != null) origins = joinIntArrays(origins, step.origins());
        }
        for(CLiteralOld[] row : block) {
            for(CLiteralOld cLiteral : row) {
                if(cLiteral != null) {
                    step = cLiteral.clause.inferenceStep;
                    if(step != null) origins = joinIntArrays(origins,step.origins());}}}
        step = dLiteral.clause.inferenceStep;
        if(step != null) origins = joinIntArrays(origins,step.origins());
        return origins;}

    @Override
    public void inferenceSteps(ArrayList<InferenceStep> steps) {
        if(!mrMatrix.trackReasoning) return;
        InferenceStep step;
        for (int index : colIndices) {
            step = mrMatrix.disjointnessClauses[index].inferenceStep;
            if (step != null) step.inferenceSteps(steps);
        }
        for(CLiteralOld[] row : block) {
            for(CLiteralOld cLiteral : row) {
                if(cLiteral != null) {
                    step = cLiteral.clause.inferenceStep;
                    if(step != null) step.inferenceSteps(steps);}}}
        step = dLiteral.clause.inferenceStep;
        if(step != null) step.inferenceSteps(steps);
        if(!steps.contains(this)) steps.add(this);}
}
