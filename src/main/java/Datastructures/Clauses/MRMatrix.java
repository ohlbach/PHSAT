package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Datastructures.TwoLiteral.TwoLitClause;
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

    public void mrResolve(ArrayList<Clause> oneLitClauses, ArrayList<TwoLitClause> twoLitClauses) throws Unsatisfiable {
        int[] colIndices;
        for(int size = columns; size > 1; --size) {
            while((colIndices = findFirstColIndices(size)) != null) {
                ArrayList<CLiteral[]> block = findBlock(colIndices);
                int rows = block.size();
                if(rows < size) continue;
                if(rows == size) {mrResolveSquare(colIndices,block,oneLitClauses,twoLitClauses); continue;}
                mrResolveRectangle(colIndices,block,oneLitClauses,twoLitClauses);}}}

    private void mrResolveSquare(int[] colIndices, ArrayList<CLiteral[]> block,
                                 ArrayList<Clause> oneLitClauses, ArrayList<TwoLitClause> twoLitClauses)
        throws Unsatisfiable {
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
                        addOneLitClause(-literal,
                                trackReasoning ? joinIntArraysSorted(origins,dClause.origins) : null,oneLitClauses);}
                    else {addTwoLitClause(external.literal,-literal,
                                trackReasoning ? joinIntArraysSorted(origins,dClause.origins) : null, twoLitClauses);}}}}}

    private void mrResolveRectangle(int[] colIndices, ArrayList<CLiteral[]> block,
                                    ArrayList<Clause> oneLitClauses, ArrayList<TwoLitClause> twoLitClauses) throws Unsatisfiable{
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
        switch(externals.size()) {
            case 0: throw new Unsatisfiable("",origins);
            case 1:
            for(int colIndex : colIndices) {
                Clause dClause = combination[colIndex];
                for(CLiteral dLiteral : dClause) {
                    int literal = dLiteral.literal;
                    boolean found = false;
                    for(CLiteral[] clausePart : block) {
                        if(clausePart[colIndex].literal == literal) {found = true; break;}}
                    if(!found) {
                        if(externals.isEmpty())
                            addOneLitClause(-literal, trackReasoning ? joinIntArraysSorted(origins,dClause.origins) : null,
                            oneLitClauses);
                    else {addTwoLitClause(-literal,externals.get(0).literal,
                                trackReasoning ? joinIntArraysSorted(origins,dClause.origins) : null,
                                twoLitClauses);}}}}
            case 2: addTwoLitClause(externals.get(0).literal,externals.get(1).literal,origins,twoLitClauses);}}

    /** adds a derived literal as one-literal clause to the list.
     * The literal is only added if it is new to the list
     * The literal is wrapped into a clause in order to store the origins with it.
     *
     * @param literal        a derived literal
     * @param origins        the basic clause ids causing the derivation of the literal
     * @param oneLitClauses  the list of derived unit clauses
     * @throws Unsatisfiable if the literal contradicts a previously derived literal.
     */
    private void addOneLitClause(int literal, IntArrayList origins, ArrayList<Clause> oneLitClauses) throws Unsatisfiable {
        for(Clause clause : oneLitClauses) {
            int oldLiteral = clause.getLiteral(0);
            if(literal == oldLiteral) return; // is already there
            if(literal == -oldLiteral) throw new Unsatisfiable("", joinIntArraysSorted(clause.origins, origins));}
            Clause unitClause = new Clause(0,ClauseType.AND,1); // clause id is arbitrary
            unitClause.add(literal);
            unitClause.origins = sortIntArray(origins);
            oneLitClauses.add(unitClause);}

    /** adds a derived two-literal clause to the list, if it is not already there
     * The two literals are wrapped into a clause in order to store the origins as well.
     *
     * @param literal1       a literal of the two-literal clause
     * @param literal2       a literal of the two-literal clause
     * @param origins        the basic clause ids causing the derivation of the literal
     * @param twoLitClauses  the list of derived two-literal clauses.
     */
    private void addTwoLitClause(int literal1, int literal2, IntArrayList origins,  ArrayList<TwoLitClause> twoLitClauses) {
        for(TwoLitClause clause : twoLitClauses) {
            int lit1 = clause.literal1;
            int lit2 = clause.literal2;
            if((lit1 == literal1 && lit2 == literal2) || (lit2 == literal1 && lit1 == literal2)) return;}
            TwoLitClause twoClause = new TwoLitClause(0,literal1,literal2, trackReasoning ? sortIntArray(origins) : null);
            twoLitClauses.add(twoClause);}



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
