package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.TwoLiteral.TwoLitClause;
import Management.Monitor;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.awt.image.AreaAveragingScaleFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Formatter;
import java.util.Locale;

import static Utilities.Utilities.*;

/** This class performs multi-resolution.
 * A simple example: Clauses:
 * 1,2,3<br>
 * 4,5,6<br>
 * 7,8,9<br>
 * 10,11,12<br>
 * where the disjointness clauses are:<br>
 * 1,4,7,10<br>
 * 2,5,8,11<br>
 * 3,6,9,12<br>
 * This is an immediate contradiction. <br>
 * There are other versions of multi-resolution where resolvents are created.
 */
public class MRMatrix {
    private Clause[] combination;            // a list of Disjointness clauses
    private ArrayList<CLiteral>[] dLiterals; // the rearranged list of CLiterals of the disjointness clauses
    private ArrayList<CLiteral[]> matrix = new ArrayList<>(); // the matrix of clauses
    private int columns;                     // combination.length
    public boolean trackReasoning;           // controls computation of origins
    private Symboltable symboltable;         // null or a symboltable
    private Monitor monitor;                 // null or a monitor
    private boolean monitoring;              // monitor != null
    private String monitorId;                // a monitor id


    /** Creates a multi-resolution matrix
     *
     * @param combination a list of Disjointness clauses
     * @param symboltable a symboltable
     * @param monitor     null or a monitor
     * @param monitorId   a monitorId
     * @param trackReasoning controls computation of origins.
     */

    public MRMatrix(Clause[] combination, Symboltable symboltable, Monitor  monitor, String monitorId,
                    boolean trackReasoning) {
        this.combination = combination;
        this.symboltable = symboltable;
        this.monitor = monitor;
        monitoring = monitor != null;
        this.monitorId = monitorId;
        this.trackReasoning = trackReasoning;
        this.columns = combination.length;
        int maxLength = 0;
        for(Clause clause : combination) maxLength = Math.max(maxLength,clause.size());
        dLiterals = new ArrayList[combination.length];
        for(int i = 0; i < columns; ++i) {
            dLiterals[i] = (ArrayList<CLiteral>)combination[i].cliterals.clone();
            int size = dLiterals[i].size();
            for(int j = size; j < maxLength; ++j) dLiterals[i].add(null);}}

    /** inserts the clause into the matrix
     *
     * @param cClause a clause
     * @return true if the clause could be inserted.
     */
    public boolean insertClause(Clause cClause) {
        System.out.println("INS " + cClause.toString());
        for(CLiteral cLiteral : cClause) {
            int literal = cLiteral.literal;
            if(contains(literal, getColumn(literal))) return false;}

        int row = matrix.size();
        CLiteral[] matrixRow = new CLiteral[columns +1];
        matrix.add(matrixRow);

        for(CLiteral cLiteral : cClause) {
            int literal = cLiteral.literal;
            int column = getColumn(literal);
            if(column == columns) assert matrixRow[column] == null;
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

    /** exchanges the CLiterals in the column such that the literal can be put into the given row number
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

    /** performs all multi-resolutions and adds the resolvents to oneLitClauses and twoLitClauses
     *
     * @param oneLitClauses  for adding derived one-literal clauses
     * @param twoLitClauses  for adding derived two-literal clauses.
     * @throws Unsatisfiable if a contradiction is found.
     */
    public void mrResolve(ArrayList<Clause> oneLitClauses, ArrayList<TwoLitClause> twoLitClauses) throws Unsatisfiable {
        int[] colIndices;
        for(int size = columns; size > 1; --size) {
            while((colIndices = findFirstColIndices(size)) != null) {
                ArrayList<CLiteral[]> block = findBlock(colIndices);
                int rows = block.size();
                if(rows < size) continue;
                if(rows == size) {mrResolveSquare(colIndices,block,oneLitClauses,twoLitClauses); continue;}
                mrResolveRectangle(colIndices,block,oneLitClauses,twoLitClauses);}}}

    /** performs multi-resolution with a block of clauses whose core literals form a square.
     * First Example: block <br>
     *  1,2,3<br>
     *  4,5,6<br>
     *  7,8,9<br>
     *  Suppose the disjointness clauses are:
     *  1,4,7,20,21 <br>
     *  2,5,8,21<br>
     *  3,6,9,22<br>
     *  Since a model must make at least one literal in the block's clauses true,
     *  it can't make the remaining literals in the disjointness clauses true.<br>
     *  Therefore one can derive: -20,-21,-22 as true literals.<br>
     *  <br>
     *  Second Example: block:
     *  1,2,3<br>
     *  4,5,6,10<br>
     *  7,8,9<br>
     *  with the same disjointness clauses. <br>
     *  A model may make the extra literal 10 true. <br>
     *  Now one can only derive two-literal clauses: <br>
     *  10,-20 and 10,-21 and 10,-22. <br>
     *  Only one such external literal is allowed, otherwise one would get 3- or more literal clauses.
     *
     * @param colIndices     the result of findFirstColIndices
     * @param block          the result of findBlock
     * @param oneLitClauses  for adding derived one-literal clauses
     * @param twoLitClauses  for adding derived two-literal clauses.
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected void mrResolveSquare(int[] colIndices, ArrayList<CLiteral[]> block,
                                 ArrayList<Clause> oneLitClauses, ArrayList<TwoLitClause> twoLitClauses)
            throws Unsatisfiable {
        System.out.println("COL " + Arrays.toString(colIndices) );
        IntArrayList origins = null;
        CLiteral external = null;
        for(CLiteral[] row : block) {
            if(trackReasoning) {
                for(CLiteral cLiteral : row) {
                    if(cLiteral != null) {origins = joinIntArrays(origins,cLiteral.clause.origins); break;}}}
            CLiteral nextExternal = row[row.length-1];
            if(nextExternal == null) continue;
            if(external != null) return; // at most one external allowed
            external = nextExternal;}

        for(int colIndex : colIndices) {
            Clause dClause = combination[colIndex];
            for(CLiteral dLiteral : dClause) {  // now look for literals in the disjointness clauses which are
                int literal = dLiteral.literal; // not in the block. They generate resolvents.
                boolean found = false;
                for(CLiteral[] row : block) {
                    System.out.println("ROW " + arraysToString(row,(cl-> cl == null ? " ": ((CLiteral)cl).literal)));
                    if(row[colIndex] != null && row[colIndex].literal == literal) {found = true; break;}}
                if(!found) { // literal is not in the block
                    if(external == null) {  // generate unit literals
                        addOneLitClause(-literal,colIndices, block,
                                trackReasoning ? joinIntArraysSorted(origins,dClause.origins) : null,oneLitClauses);}
                    else {addTwoLitClause(external.literal,-literal,colIndices, block,
                                trackReasoning ? joinIntArraysSorted(origins,dClause.origins) : null, twoLitClauses);}}}}}


    /** performs multi-resolution where the block has more rows than columns.
     * Example: block:
     * 1,2,3<br>
     * 4,5,6<br>
     * 7,8,9<br>
     * 10,11,12<br>
     * where the disjointness clauses are:<br>
     * 1,4,7,10<br>
     * 2,5,8,11<br>
     * 3,6,9,12<br>
     * This is an immediate contradiction. <br>
     * <br>
     * Example: (with the same disjointness clauses) block: <br>
     * 1,2,3<br>
     * 4,5,6,20<br>
     * 7,8,9<br>
     * 10,11,12<br>
     * Now a unit clause 20 can be derived.
     * Example: (with the same disjointness clauses) block: <br>
     * 1,2,3<br>
     * 4,5,6,20<br>
     * 7,8,9,30<br>
     * 10,11,12<br>
     * Now a two-literal clause 20,30 can be derived.
     *
     * @param colIndices     the result of findFirstColIndices
     * @param block          the result of findBlock
     * @param oneLitClauses  for adding derived one-literal clauses
     * @param twoLitClauses  for adding derived two-literal clauses.
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected void mrResolveRectangle(int[] colIndices, ArrayList<CLiteral[]> block,
                                    ArrayList<Clause> oneLitClauses, ArrayList<TwoLitClause> twoLitClauses) throws Unsatisfiable{
        IntArrayList origins = null;
        ArrayList<CLiteral> externals = new ArrayList<>();
        for (CLiteral[] row : block) { // compute externals and origins
            if (trackReasoning) {
                for (CLiteral cLiteral : row) {
                    if (cLiteral != null) {origins = joinIntArrays(origins, cLiteral.clause.origins); break;}}}
            CLiteral external = row[row.length - 1];
            if(external == null) continue;
            if(externals.size() > 1) return; // at most two externals allowed
            externals.add(external);}

        if (trackReasoning) {
            for(int colIndex : colIndices) {
                origins = joinIntArraysSorted(origins,combination[colIndex].origins);}}

        switch(externals.size()) {
            case 0: throw new Unsatisfiable(block2String(colIndices,block,symboltable) + "is unsatisfiable",
                                            origins);
            case 1: addOneLitClause(externals.get(0).literal,colIndices,block,origins,oneLitClauses); break;
            case 2: addTwoLitClause(externals.get(0).literal,externals.get(1).literal,
                    colIndices, block,origins,twoLitClauses);}}

    /** adds a derived literal as one-literal clause to the list.
     * The literal is only added if it is new to the list
     * The literal is wrapped into a clause in order to store the origins with it.
     *
     * @param literal        a derived literal
     * @param origins        the basic clause ids causing the derivation of the literal
     * @param oneLitClauses  the list of derived unit clauses
     * @throws Unsatisfiable if the literal contradicts a previously derived literal.
     */
    private void addOneLitClause(int literal,
                                 int[] colIndices, ArrayList<CLiteral[]> block,
                                 IntArrayList origins, ArrayList<Clause> oneLitClauses) throws Unsatisfiable {
        for(Clause clause : oneLitClauses) {
            int oldLiteral = clause.getLiteral(0);
            if(literal == oldLiteral) return; // is already there
            if(literal == -oldLiteral)
                throw new Unsatisfiable(
                        "MR-Resolution yields a contradiction when adding derived unit literal " +
                        Symboltable.toString(literal,symboltable) + " :\n"+ block2String(colIndices,block,symboltable),
                        joinIntArraysSorted(clause.origins, origins));}
        if(monitoring) { String orig = "";
            if(origins != null) orig = "\nOrigins: " +origins.toString();
            monitor.print(monitorId,"Multi-resolution with block\n" + block2String(colIndices,block,symboltable) +
                    "yields unit literal " + Symboltable.toString(literal,symboltable) + orig);}
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
    private void addTwoLitClause(int literal1, int literal2,
                                 int[] colIndices, ArrayList<CLiteral[]> block,
                                 IntArrayList origins,  ArrayList<TwoLitClause> twoLitClauses) {
        for(TwoLitClause clause : twoLitClauses) {
            int lit1 = clause.literal1;
            int lit2 = clause.literal2;
            if((lit1 == literal1 && lit2 == literal2) || (lit2 == literal1 && lit1 == literal2)) return;}
        if(monitoring) {
            String orig = "";
            if(origins != null) orig = "\nOrigins: " +origins.toString();
            monitor.print(monitorId,"Multi-resolution with block\n" + block2String(colIndices,block,symboltable) +
                    "yields two-literal clause " +
                    Symboltable.toString(literal1,symboltable)+","+Symboltable.toString(literal2,symboltable) + orig);}
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
     * @return a list with elements [CLiteral1, ..., CLiteral_n, external CLiteral]
     */
    protected ArrayList<CLiteral[]> findBlock(int[] colIndices) {
        block.clear();
        CLiteral[] cliterals = new CLiteral[colIndices.length+1];
        boolean isEmpty = true;
        for(int row = 0; row < matrix.size(); ++row) {
            if(isEmpty) {for(int i = 0; i < cliterals.length; ++i) {cliterals[i] = null;}} // reuse it
            else cliterals = new CLiteral[colIndices.length+1];
            isEmpty = false;
            CLiteral[] clause = matrix.get(row);  // candidate for the block
            CLiteral extern = clause[columns];
            cliterals[colIndices.length] = extern;         // maybe null
            int index = -1;
            for(int col = 0; col < clause.length-1; ++col) { // find a non-null entry matching the colIndices
                CLiteral cliteral = clause[col];
                if(cliteral != null) {
                    boolean found = false;
                    for(int j = 0; j < colIndices.length; ++j) {
                        if(colIndices[j] == col) {found = true; break;}} // found one
                    if(found) cliterals[++index] = cliteral;
                    else{ // only one non-null CLiteral which is not in colIndices is allowed
                        if(extern == null) {cliterals[colIndices.length] = cliteral; extern = cliteral;}
                        else {isEmpty = true; break;}}}}
            if(!isEmpty) {block.add(cliterals);}}
        return block;}

    /** turns a block, found by findBlock into a formatted string
     *
     * @param colIndices  the result of findFirstColIndices
     * @param block       the result of findBlock
     * @param symboltable null or a symboltable
     * @return            the block as string
     */
    public String block2String(int[] colIndices, ArrayList<CLiteral[]> block, Symboltable symboltable) {
        int width = 0;
        for(int colIndex : colIndices) {
            width = Math.max(width,Integer.toString(combination[colIndex].id).length());}
        for(CLiteral[] row : block) {
            for(CLiteral cliteral : row) {width = Math.max(width, cliteral == null ? 0 :
                        Symboltable.toString(cliteral.literal, symboltable).length());}}

        StringBuilder st = new StringBuilder();
        st.append("Multi-Resolution Block of size ").append(Integer.toString(colIndices.length)).append(":\n");
        Formatter format = new Formatter(st, Locale.GERMANY);
        format.format("%"+width+"s|"," ");
        for(int colIndex : colIndices) {
            format.format("%"+width+"s|",combination[colIndex].id);}
        st.append("\n");
        st.append(concatenateString("-",combination.length * (width+3))).append("\n");
        for(CLiteral[] row : block) {
            format.format("%"+width+"s|",getClause(row).id);
            for(CLiteral cLiteral : row) {
                format.format("%"+width+"s|",cLiteral == null ? " " :
                        Symboltable.toString(cLiteral.literal,symboltable));}
            st.append("\n");}
        return st.toString();}

    /** Stores previously found colIndices (see findFirstColIndices).
     * It helps to prevent finding the same indices again.*/
    private ArrayList<int[]> previousIndices = new ArrayList<>();

    /** searches the first new row with exactly 'size' non-null core literals.
     * This determines a block of rows which are candidates for multi-resolutions.
     *
     * @param size an integer
     * @return null or [colIndex1,...,colIndexSize]
     */
    protected int[] findFirstColIndices(int size) {
        for(int row = 0; row < matrix.size(); ++row) { // check all rows. Find the first matching row.
            CLiteral[] clause = matrix.get(row);
            if(clause[columns] != null) continue; // has an extra non core-literal.
            int counter = 0;
            for(int i = 0; i < columns; ++i){     // count the number of non-null entries
                if(clause[i] != null) ++counter;}
            if(counter != size) continue;         // has not the right size
            int[] indices = new int[size];
            counter = -1;
            for(int i = 0; i < columns; ++i){
                if(clause[i] != null) indices[++counter] = i;}  // collect the indices
            boolean found = false;
            for(int[] previous : previousIndices) {             // check if the indices are new.
                if(previous.length != size) continue;
                found = true;
                for(int i = 0; i < size; ++i) {
                    if(previous[i] != indices[i]) {found = false; break;}} // no exact match
                if(found) break;}
            if(found) continue;
            previousIndices.add(indices);
            return indices;}
        return null;}   // nothing new found


    /** returns the clause of the given matrix's row.
     * The first non-Null CLiteral in the row determines the clause
     *
     * @param row a row in the matrix (the literals of the clause, possibly with nulls in between
     * @return the clause belonging to the row.
     */
    private Clause getClause(CLiteral[] row) {
        for(CLiteral cLiteral : row) {
            if(cLiteral != null) return cLiteral.clause;}
        return null;}


    public String infoString(Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("Multi-Resolution Matrix\nDisjointness Clauses:\n");
        int width = 0;
        for(Clause dClause : combination) {width = Math.max(width,Integer.toString(dClause.id).length());}

        for(Clause dClause :combination) {st.append(dClause.toString(width,symboltable)).append("\n");}

        for(Clause dClause : combination) {
            for(CLiteral dLiteral : dClause)
                width = Math.max(width,Symboltable.toString(dLiteral.literal,symboltable).length());}

        st.append("\n\nRearranged literals of the disjointness clauses:\n");
        Formatter format = new Formatter(st, Locale.GERMANY);
        combinationHeader(st,0,width,format);
        for(int row = 0; row <dLiterals[0].size(); ++row) {
            for(int i = 0; i < columns; ++i) {
                CLiteral dLiteral = dLiterals[i].get(row);
                format.format("%"+width+"s|",
                        (dLiteral == null ? " " : Symboltable.toString(dLiteral.literal,symboltable)));}
            st.append("\n");}
        return st.toString();}

    private void combinationHeader(StringBuilder st, int prefix, int width, Formatter format) {
        if(prefix > 0) format.format("%"+prefix+"s "," ");
        for(Clause clause : combination) format.format("%"+width+"s|",clause.id);
        st.append("\n");
        st.append(concatenateString("-",combination.length * (width+3)));
        st.append("\n");}

    //public Clause[] combination;
    //public ArrayList<CLiteral>[] dLiterals;
    //public ArrayList<CLiteral[]> matrix = new ArrayList<>();

    public String toString(Symboltable symboltable) {
        int dSize = Clause.clauseNameWidth(combination) + 2;
        int cSize = 1;
        for(CLiteral[] row : matrix) {
            for(CLiteral cLiteral : row) {
                Clause clause = getClause(row);
                if(clause != null) cSize = Math.max(cSize,Integer.toString(clause.id).length()+2);}}

        StringBuilder st = new StringBuilder();
        st.append("Multi-Resolution Matrix:\n");
        Formatter format = new Formatter(st, Locale.GERMANY);
        combinationHeader(st,cSize,dSize,format);
        for(CLiteral[] row : matrix) {
            Clause clause = getClause(row);
            if(clause == null) continue;
            format.format("%"+cSize+"s",clause.id);
            for(CLiteral cLiteral : row) {
                format.format("|%"+dSize+"s",cLiteral == null ? " " :
                        Symboltable.toString(cLiteral.literal,symboltable));}
            st.append("\n");}
        return st.toString();}


}
