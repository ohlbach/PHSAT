package Datastructures.Clauses;

import Datastructures.Literals.CLiteral;
import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import Datastructures.TwoLiteral.TwoLitClause;
import Management.Monitor;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;
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
    private AllClauses allClauses;
    private Clause[] disjointnessClauses;    // a list of Disjointness clauses
    private ArrayList<CLiteral>[] dLiterals; // the rearranged list of CLiterals of the disjointness clauses
    private ArrayList<CLiteral[]> matrix = new ArrayList<>(); // the matrix of clauses
    private int columnSize;                  // disjointnessClauses.length
    public boolean trackReasoning;           // controls computation of origins
    private Symboltable symboltable;         // null or a symboltable
    private Monitor monitor;                 // null or a monitor
    private boolean monitoring;              // monitor != null
    private String monitorId;                // a monitor id
    private Model model;                     // the global model
    int matrixDepth = 0;                     // the maximum allowed depth of the matrix (the longest disjointness clause)

    /** creates a multi-resolution matrix
     *
     * @param allClauses          the "parent class"
     * @param disjointnessClauses an array of disjointness clauses.
     */
    public MRMatrix(AllClauses allClauses, Clause[] disjointnessClauses) {
        this.allClauses = allClauses;
        this.disjointnessClauses = disjointnessClauses;
        columnSize = disjointnessClauses.length;
        monitor = allClauses.monitor;
        monitorId = allClauses.monitorId;
        monitoring = monitor != null;
        model = allClauses.model;
        symboltable = model.symboltable;
        trackReasoning = allClauses.trackReasoning;
        for(Clause clause : disjointnessClauses) matrixDepth = Math.max(matrixDepth,clause.size());
        dLiterals = new ArrayList[disjointnessClauses.length];
        for(int i = 0; i < columnSize; ++i) {
            dLiterals[i] = (ArrayList<CLiteral>) disjointnessClauses[i].cliterals.clone();
            int size = dLiterals[i].size();
            for(int j = size; j < matrixDepth; ++j) dLiterals[i].add(null);}
    }

    /** inserts the clause into the matrix
     * A clause forms a new row in the matrix only if certain conditions hold.
     * The literals are distributed such that they fit into the disjointnessClauses's columns.
     * Only one extra literal which is not in the columns is allowed.
     * It is put into the last cell of the row.
     * Two literals in the clause which are already disjoint are not allowed.
     *
     * @param cClause a clause
     * @return true if the clause could be inserted.
     */
    public boolean insertClause(Clause cClause) {
        if(matrix.size() == matrixDepth) return false; // the clause cannot be part of a multi-resolution
        int row = matrix.size();
        CLiteral[] matrixRow = new CLiteral[columnSize +1];

        for(CLiteral cLiteral : cClause) {
            int literal = cLiteral.literal;
            int column = getColumn(literal);
            if(column == columnSize && matrixRow[columnSize] != null) {return false;} // more than one external literal
            matrixRow[column] = cLiteral;
            if(column < columnSize) setRow(literal,row,column);}
        matrix.add(matrixRow);
        return true;}

    /** determines the column number for the literal.
     * The literal must be in the disjointness clause of the column,
     * but not already in the column of the matrix.
     *
     * @param literal a literal
     * @return a free column number for the literal
     */
    private int getColumn(int literal) {
        for(int column = 0; column < columnSize; ++column) {
            if(disjointnessClauses[column].contains(literal) > 0 && !matrixContains(literal,column)) {return column;}}
        return columnSize;}



    /** checks if the literal is already contained in the matrix at the given column
     *
     * @param literal a literal
     * @param column a column index in the matrix
     * @return true if the literal is already in the matrix at the given column
     */
    private boolean matrixContains(int literal, int column) {
        for(CLiteral[] row : matrix) {
            CLiteral cLiteral = row[column];
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
            if(dLiteral != null && literal == dLiteral.literal) return row;}
        return dColumn.size()-1;}

    /** performs all multi-resolutions and adds the resolvents to the model and twoLitClauses
     *
     * @param twoLitClauses  for adding derived two-literal clauses.
     * @throws Unsatisfiable if a contradiction is found.
     */
    public void mrResolve(ArrayList<TwoLitClause> twoLitClauses) throws Unsatisfiable {
        int[] colIndices;
        for(int size = columnSize; size > 1; --size) {
            while((colIndices = findFirstColIndices(size)) != null) {
                ArrayList<CLiteral[]> block = findBlock(colIndices);
                int rows = block.size();
                if(rows < size) continue;
                if(rows == size) {mrResolveSquare(colIndices,block,twoLitClauses); continue;}
                mrResolveRectangle(colIndices,block,twoLitClauses);}}}

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
     * @param twoLitClauses  for adding derived two-literal clauses.
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected void mrResolveSquare(int[] colIndices, ArrayList<CLiteral[]> block, ArrayList<TwoLitClause> twoLitClauses)
            throws Unsatisfiable {
        IntArrayList origins = null;

        int size = block.size();
        ArrayList<CLiteral> externals = new ArrayList<>();
        for(int i = 0; i < size; ++i) {
            CLiteral external = block.get(i)[size];
            if(external != null) externals.add(external);}
        if(externals.size() >= 2) return;

        if(trackReasoning) {
            for(int colIndex : colIndices) {
                origins = joinIntArrays(origins, disjointnessClauses[colIndex].origins);}}

        int maxSurplus = 0;
        for(int i = 0; i < colIndices.length; ++i) {
            maxSurplus = Math.max(maxSurplus,disjointnessClauses[colIndices[i]].size()-size);}

        if(maxSurplus >= 2) {
            switch(externals.size()) {
                case 1: sendTrueLiteral(externals.get(0).literal,colIndices,block,
                        null,null,-1,sortIntArray(origins));
                return;
                case 2: addTwoLitClause(externals.get(0).literal,externals.get(1).literal,colIndices, block,
                        null, null,-1,-1,sortIntArray(origins), twoLitClauses);
                return;}}

        for(int i = 0; i < colIndices.length; ++i) {
            for(CLiteral dLiteral : disjointnessClauses[colIndices[i]]) {  // now look for literals in the disjointness clauses which are
                int literal = dLiteral.literal;                    // not in the block. They generate resolvents.
                boolean found = false;
                for(CLiteral[] row : block) {
                    if(row[i] != null && row[i].literal == literal) {found = true; break;}}
                if(!found) { // literal is not in the block
                    if(externals.size() == 0) {  // generate unit literals
                        sendTrueLiteral(-literal,colIndices, block,null,null,-1,sortIntArray(origins));}
                    else {addTwoLitClause(externals.get(0).literal,-literal,colIndices, block,null, null,-1,-1,
                                sortIntArray(origins), twoLitClauses);}}}}}


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
     * 20 is now a so called "external" literal.
     * Now a unit clause 20 can be derived.
     * Example: (with the same disjointness clauses) block: <br>
     * 1,2,3<br>
     * 4,5,6,20<br>
     * 7,8,9,30<br>
     * 10,11,12<br>
     * Now a two-literal clause 20,30 can be derived.<br>
     * If the block has more rows than necessary, the algorithm iterates over all
     * possibilities to reduce the block to the necessary rows, and
     * therefore to reduce the externals. Now several unit- and two-literal clauses
     * can be derived.
     *
     * @param colIndices     the result of findFirstColIndices
     * @param block          the result of findBlock
     * @param twoLitClauses  for adding derived two-literal clauses.
     * @throws Unsatisfiable if a contradiction is found.
     */
    protected void mrResolveRectangle(int[] colIndices, ArrayList<CLiteral[]> block, ArrayList<TwoLitClause> twoLitClauses) throws Unsatisfiable{

        IntArrayList origins = null;
        if(trackReasoning) {
            for(int i : colIndices) {
                origins = joinIntArrays(origins, disjointnessClauses[i].origins);}}

        int width = disjointnessClauses.length;
        int depth = block.size();
        int ignoreRows = depth - width;
        if(ignoreRows < 0) return;

        ArrayList<CLiteral> externals = new ArrayList<>();
        IntArrayList externalRowIndices = new IntArrayList();
        for(int i = 0; i < depth; ++i) {
            CLiteral[] row = block.get(i);
            CLiteral external = row[width];
            if(external != null)
                externalRowIndices.add(i);
                externals.add(external);}

        int externalSize = externals.size();
        int keepExternals = externalSize-ignoreRows;

        switch(keepExternals) {
            case 0: throw new Unsatisfiable(block2String(colIndices,block,
                        externalRowIndices,-1,-1,symboltable) + "is unsatisfiable",origins);
            case 1:
                for(int i = 0; i < externalSize; ++i) {
                    sendTrueLiteral(externals.get(i).literal,
                            colIndices,block,externals, externalRowIndices,externalRowIndices.getInt(i), origins);}
                break;
            case 2:
                for(int i = 0; i < externalSize; ++i) {
                    CLiteral external1 = externals.get(i);
                    int keepIndex1 = externalRowIndices.getInt(i);
                    for(int j = i + 1; j < externalSize; ++j) {
                        addTwoLitClause(external1.literal,externals.get(j).literal,
                            colIndices, block, externals, externalRowIndices,keepIndex1,externalRowIndices.getInt(j),
                                origins,twoLitClauses);}}}}


    /** adds a derived true literal to the model
     *
     * @param literal        the true literal
     * @param colIndices     the list of indices for the disjointness clauses (combination)
     * @param block          the block of clauses which caused the multi-resolution
     * @param externals      the list of external literals to become part of the resolvents
     * @param ignoreIndices  the list of row indices which can be ignored
     * @param keepIndex      the row index which cannot be ignored
     * @param origins        null or the origins of the disjointness clauses
     * @throws Unsatisfiable if the new literal contradicts the model.
     */
    private void sendTrueLiteral(int literal,
            int[] colIndices, ArrayList<CLiteral[]> block, ArrayList<CLiteral> externals,
                                 IntArrayList ignoreIndices, int keepIndex,
                                 IntArrayList origins) throws Unsatisfiable {
        if(model.isTrue(literal)) return;
        if(trackReasoning) {
            for(int i = 0; i < block.size(); ++i) {
                if(externals == null || i == keepIndex || !ignoreIndices.contains(i)) {
                    joinIntArrays(origins,getClause(block.get(i)).origins);}}}
        if(monitoring) {
            String orig = "";
            if(origins != null) orig = "\nOrigins: " +origins.toString();
            monitor.print(monitorId,"Multi-resolution with block\n" +
                    block2String(colIndices,block,ignoreIndices,keepIndex,keepIndex,symboltable) +
                    "yields unit literal " + Symboltable.toString(literal,symboltable) + orig);}
        model.add(literal,null,null);}


    /** adds a derived two-literal clause to the list, if it is not already there
     * The two literals are wrapped into a clause in order to store the origins as well.
     *
     * @param literal1       a literal of the two-literal clause
     * @param literal2       a literal of the two-literal clause
     * @param origins        the basic clause ids causing the derivation of the literal
     * @param twoLitClauses  the list of derived two-literal clauses.
     */
    private void addTwoLitClause(int literal1, int literal2,
                                 int[] colIndices, ArrayList<CLiteral[]> block,ArrayList<CLiteral> externals,
                                 IntArrayList ignoreIndices, int keepIndex1, int keepIndex2,
                                 IntArrayList origins,  ArrayList<TwoLitClause> twoLitClauses) {

        if(trackReasoning) {
            for(int i = 0; i < block.size(); ++i) {
                if(externals == null || i == keepIndex1 || i == keepIndex2 || !ignoreIndices.contains(i)) {
                    joinIntArrays(origins,getClause(block.get(i)).origins);}}}

        for(TwoLitClause clause : twoLitClauses) {
            int lit1 = clause.literal1;
            int lit2 = clause.literal2;
            if((lit1 == literal1 && lit2 == literal2) || (lit2 == literal1 && lit1 == literal2)) return;}

        if(monitoring) {
            String orig = "";
            if(origins != null) orig = "\nOrigins: " +origins.toString();
            monitor.print(monitorId,"Multi-resolution with block\n" +
                    block2String(colIndices,block,ignoreIndices,keepIndex1,keepIndex2,symboltable) +
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
            CLiteral extern = clause[columnSize];
            cliterals[colIndices.length] = extern;         // maybe null
            for(int col = 0; col < clause.length-1; ++col) { // find a non-null entry matching the colIndices
                CLiteral cliteral = clause[col];
                if(cliteral != null) {
                    boolean found = false;
                    int j = 0;
                    for(; j < colIndices.length; ++j) {
                        if(colIndices[j] == col) {found = true; break;}} // found one
                    if(found) cliterals[j] = cliteral;
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
    public String block2String(int[] colIndices, ArrayList<CLiteral[]> block,
                               IntArrayList ignoreIndices, int keepIndex1, int keepIndex2,
                               Symboltable symboltable) {
        int width = 0;
        for(int colIndex : colIndices) {
            width = Math.max(width,Integer.toString(disjointnessClauses[colIndex].id).length());}
        for(CLiteral[] row : block) {
            for(CLiteral cliteral : row) {width = Math.max(width, cliteral == null ? 0 :
                        Symboltable.toString(cliteral.literal, symboltable).length());}}

        StringBuilder st = new StringBuilder();
        st.append("Multi-Resolution Block of size ").append(Integer.toString(colIndices.length)).append(":\n");
        Formatter format = new Formatter(st, Locale.GERMANY);
        format.format("%"+width+"s|"," ");
        for(int colIndex : colIndices) {
            format.format("%"+width+"s|", disjointnessClauses[colIndex].id);}
        st.append("\n");
        st.append(concatenateString("-", disjointnessClauses.length * (width+3))).append("\n");
        for(int i = 0; i < block.size(); ++i) {
            if(ignoreIndices == null || i == keepIndex1 || i == keepIndex2 || !ignoreIndices.contains(i)) {
                CLiteral[] row = block.get(i);
                format.format("%"+width+"s|",getClause(row).id);
                for(CLiteral cLiteral : row) {
                    format.format("%"+width+"s|",cLiteral == null ? " " :
                            Symboltable.toString(cLiteral.literal,symboltable));}}
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
            if(clause[columnSize] != null) continue; // has an extra non core-literal.
            int counter = 0;
            for(int i = 0; i < columnSize; ++i){     // count the number of non-null entries
                if(clause[i] != null) ++counter;}
            if(counter != size) continue;         // has not the right size
            int[] indices = new int[size];
            counter = -1;
            for(int i = 0; i < columnSize; ++i){
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
        for(Clause dClause : disjointnessClauses) {width = Math.max(width,Integer.toString(dClause.id).length());}

        for(Clause dClause : disjointnessClauses) {st.append(dClause.toString(width,symboltable)).append("\n");}

        for(Clause dClause : disjointnessClauses) {
            for(CLiteral dLiteral : dClause)
                width = Math.max(width,Symboltable.toString(dLiteral.literal,symboltable).length());}

        st.append("\n\nRearranged literals of the disjointness clauses:\n");
        Formatter format = new Formatter(st, Locale.GERMANY);
        combinationHeader(st,0,width,format);
        for(int row = 0; row <dLiterals[0].size(); ++row) {
            for(int i = 0; i < columnSize; ++i) {
                CLiteral dLiteral = dLiterals[i].get(row);
                format.format("%"+width+"s|",
                        (dLiteral == null ? " " : Symboltable.toString(dLiteral.literal,symboltable)));}
            st.append("\n");}
        return st.toString();}

    private void combinationHeader(StringBuilder st, int prefix, int width, Formatter format) {
        if(prefix > 0) format.format("%"+prefix+"s "," ");
        for(Clause clause : disjointnessClauses) format.format("%"+width+"s|",clause.id);
        st.append("\n");
        st.append(concatenateString("-", disjointnessClauses.length * (width+3)));
        st.append("\n");}

    //public Clause[] combination;
    //public ArrayList<CLiteral>[] dLiterals;
    //public ArrayList<CLiteral[]> matrix = new ArrayList<>();

    public String toString(Symboltable symboltable) {
        int dSize = Clause.clauseNameWidth(disjointnessClauses) + 2;
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
