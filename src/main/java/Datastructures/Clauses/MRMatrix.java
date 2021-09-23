package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

import static Utilities.Utilities.*;

public class MRMatrix {
    public Clause[] combination;
    public ArrayList<CLiteral>[] dLiterals;
    public ArrayList<CLiteral[]> matrix = new ArrayList<>();
    public int columns;
    public boolean trackReasoning;

    public MRMatrix(Clause[] combination, boolean trackReasoning) {
        this.combination = combination;
        this.trackReasoning = trackReasoning;
        this.columns = combination.length;
        int maxLength = 0;
        for(Clause clause : combination) maxLength = Math.max(maxLength,clause.size());
        dLiterals = new ArrayList[combination.length];
        for(int i = 0; i < columns; ++i) {
            dLiterals[i] = (ArrayList<CLiteral>)combination[i].cliterals.clone();}
        for(int i = columns; i < maxLength; ++i) dLiterals[i].add(null);}

    /** inserts the clause into the matrix
     *
     * @param cClause a clause
     * @return true if the clause could be inserted.
     */
    public boolean insertClause(Clause cClause) {
        for(CLiteral cLiteral : cClause) {
            int literal = cLiteral.literal;
            if(contains(literal, getColumn(literal))) return false;}

        int row = matrix.size();
        CLiteral[] matrixRow = new CLiteral[columns +1];
        matrix.add(matrixRow);

        for(CLiteral cLiteral : cClause) {
            int literal = cLiteral.literal;
            int column = getColumn(literal);
            matrixRow[column] = cLiteral;
            if(column < columns) setRow(literal,row,column);}
        return true;}

    /** determines the column number for the literal
     *
     * @param literal a literal
     * @return the column number for the literal
     */
    private int getColumn(int literal) {
        for(int column = 0; column < columns; ++column) {
            if(combination[column].contains(literal) > 0) {return column;}}
        return columns;}



    /** checks if the literal is already contained in the matrix at the given column
     *
     * @param literal a literal
     * @param column a column index in the matric
     * @return true if the literal is already in the matrix at the given column
     */
    private boolean contains(int literal, int column) {
        for(CLiteral[] cLiterals : matrix) {
            CLiteral cLiteral = cLiterals[column];
            if(cLiteral != null && literal == cLiteral.literal) return true;}
        return false;}

    /** exchanges the CLiterals in the column such that the literal can be put into the iven row number
     *
     * @param literal a literal
     * @param row     a target row number
     * @param column  a column number
     */
    private void setRow(int literal, int row, int column) {
        ArrayList<CLiteral> dLiterals = this.dLiterals[column];
        int oldRow = getRow(literal,dLiterals);
        if(oldRow == row) return;
        CLiteral dummy = dLiterals.get(oldRow);
        dLiterals.set(oldRow,dLiterals.get(row));
        dLiterals.set(row,dummy);}

    /** determines the row where the literal is supposed to be put into
     *
     * @param literal  a literal
     * @param dColumn  a column of CLiterals
     * @return the original row number where the literal should be put into
     */
    private int getRow(int literal, ArrayList<CLiteral> dColumn) {
        for(int row = 0; row < dColumn.size(); ++row) {
            CLiteral dLiteral = dColumn.get(row);
            if(dLiteral != null && literal == dColumn.get(row).literal) return row;}
        return dColumn.size()-1;}

    public void mrResolve(ArrayList<Clause> oneLitClauses, ArrayList<Clause> twoLitClauses) throws Unsatisfiable {
        int[] colIndices;
        for(int size = columns; size > 1; --size) {
            while((colIndices = findFirstColIndices(size)) != null) {
                ArrayList<CLiteral[]> block = findBlock(colIndices);
                int rows = block.size();
                if(rows < size) continue;
                if(rows == size) {mrResolveSquare(colIndices,block,oneLitClauses,twoLitClauses); continue;}
                mrResolveRectangle(colIndices,block,oneLitClauses,twoLitClauses);}}}

    private void mrResolveSquare(int[] colIndices, ArrayList<CLiteral[]> block,
                                 ArrayList<Clause> oneLitClauses, ArrayList<Clause> twoLitClauses) {
        IntArrayList origins = null;
        CLiteral external = null;
        for(CLiteral[] clausePart : block) {
            CLiteral nextExternal = clausePart[clausePart.length-1];
            if(external != null && nextExternal != null) return; // only at most one external allowed
            external = nextExternal;
            if(trackReasoning) {
                for(CLiteral cLiteral : clausePart) {
                    if(cLiteral != null) {origins = joinIntArrays(origins,cLiteral.clause.origins); break;}}}}

        for(int colIndex : colIndices) {
            Clause dClause = combination[colIndex];
            for(CLiteral dLiteral : dClause) {
                int literal = dLiteral.literal;
                boolean found = false;
                for(CLiteral[] clausePart : block) {
                    if(clausePart[colIndex].literal == literal) {found = true; break;}}
                if(!found) {
                    if(external == null) {
                        Clause unitClause = new Clause(0,ClauseType.AND,1);
                        unitClause.add(-literal);
                        if(trackReasoning) unitClause.origins = joinIntArraysSorted(origins,dClause.origins);
                        oneLitClauses.add(unitClause);}
                    else {
                        Clause twoClause = new Clause(0,ClauseType.OR,2);
                        twoClause.add(external.literal);
                        twoClause.add(-literal);
                        if(trackReasoning) twoClause.origins = joinIntArraysSorted(origins,dClause.origins);
                        twoLitClauses.add(twoClause);}}}}}

    private void mrResolveRectangle(int[] colIndices, ArrayList<CLiteral[]> block,
                                    ArrayList<Clause> oneLitClauses, ArrayList<Clause> twoLitClauses) throws Unsatisfiable{
        IntArrayList origins = null;
        ArrayList<CLiteral> externals = new ArrayList<>();
        for (CLiteral[] clausePart : block) {
            CLiteral external = clausePart[clausePart.length - 1];
            if (external != null && externals.size() > 1) return; // only at most two externals allowed
            externals.add(external);
            if (trackReasoning) {
                for (CLiteral cLiteral : clausePart) {
                    if (cLiteral != null) {
                        origins = joinIntArrays(origins, cLiteral.clause.origins);
                        break;}}}}
        if(externals.size() == 0) {
            throw new Unsatisfiable("",origins);}

        if(externals.size() == 2) {
            Clause twoClause = new Clause(0,ClauseType.OR,2);
            twoClause.add(externals.get(0));
            twoClause.add(externals.get(1));
            if(trackReasoning) twoClause.origins = sortIntArray(origins);
            twoLitClauses.add(twoClause);
            return;}

        for(int colIndex : colIndices) {
            Clause dClause = combination[colIndex];
            for(CLiteral dLiteral : dClause) {
                int literal = dLiteral.literal;
                boolean found = false;
                for(CLiteral[] clausePart : block) {
                    if(clausePart[colIndex].literal == literal) {found = true; break;}}
                if(!found) {
                    if(externals.isEmpty()) {
                        Clause unitClause = new Clause(0,ClauseType.AND,1);
                        unitClause.add(-literal);
                        if(trackReasoning) unitClause.origins = joinIntArraysSorted(origins,dClause.origins);
                        oneLitClauses.add(unitClause);}
                    else {
                        Clause twoClause = new Clause(0,ClauseType.OR,2);
                        twoClause.add(externals.get(0).literal);
                        twoClause.add(-literal);
                        if(trackReasoning) twoClause.origins = joinIntArraysSorted(origins,dClause.origins);
                        twoLitClauses.add(twoClause);}}}}}


        private ArrayList<CLiteral[]> block = new ArrayList<>();

    /** finds a block in the matrix which matches colIndices.
     * Example: colIndices = [3,5,7] <br>
     * Each row in the matrix where at positions 3,5,7 is a CLiteral != null is added to the block;
     * but only of there is atmost one external CLiteral.
     * An external CLiteral is either the last CLiteral in the row (which will become part of the resolvent)
     * or a non-null CLiteral at a position in the row which is not in the colIndices
     *
     * @param colIndices [colIndex1,...]
     * @return a list with elements [row, external CLiteral, CLiteral1, ...]
     */
    private ArrayList<CLiteral[]> findBlock(int[] colIndices) {
        block.clear();
        CLiteral[] cliterals = new CLiteral[colIndices.length];
        boolean isEmpty = true;
        for(int row = 0; row < matrix.size(); ++row) {
           if(!isEmpty) cliterals = new CLiteral[colIndices.length];
           isEmpty = false;
            CLiteral[] clause = matrix.get(row);
            CLiteral extern = clause[columns];
            cliterals[colIndices.length-1] = extern;
            for(int col = 0; col < clause.length-1; ++col) {
                CLiteral cliteral = clause[col];
                if(cliteral != null) {
                    boolean found = false;
                    for(int j = 0; j < colIndices.length; ++j) {
                        if(colIndices[j] == col) {found = true; break;}}
                    if(found) cliterals[col] = cliteral;
                    else{
                        if(extern == null) cliterals[colIndices.length-1] = cliteral;
                        else {isEmpty = true; break;}}}}
            if(!isEmpty) {block.add(cliterals);}}
        return block;}

    private ArrayList<int[]> previousIndices = new ArrayList<>();

    /** searches the first new row with exactly 'size' non-null core literals
     *
     * @param size an integer
     * @return [colIndex1,...,colIndexSize]
     */
    private int[] findFirstColIndices(int size) {
        for(int row = 0; row < matrix.size(); ++row) {
            CLiteral[] clause = matrix.get(row);
            if(clause[columns] != null) continue;
            int counter = 0;
            for(int i = 0; i < columns; ++i){
                if(clause[i] != null) ++counter;}
            if(counter != size) continue;
            int[] indices = new int[size];
            counter = -1;
            for(int i = 0; i < columns; ++i){
                if(clause[i] != null) indices[++counter] = i;}
            boolean found = false;
            for(int[] previous : previousIndices) {
                if(previous.length != size) continue;
                found = true;
                for(int i = 0; i <= size; ++i) {
                    if(previous[i] != indices[i]) {found = false; break;}}
                if(found) break;}
            if(found) continue;
            previousIndices.add(indices);
            return indices;}
        return null;}
}
