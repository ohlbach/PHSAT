package Datastructures.Theory;

import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Created by ohlbach on 29.08.2018.
 *
 * The Implication Graph is a representation of two-literal disjunctions.
 * A clause p,q is equivalent to the two implications -p -> q and -q -> p.
 * The Implication Graph represents for each literal all the consequences with can be derived
 * from the two-literal disjunctions.
 */
public class ImplicationGraph {
    private final int predicates;   // number of predicates (used only in toString())
    private final HashMap<Integer,TreeSet<Integer>> implicants = new HashMap<>();  // contains for each literal the implied literals
    private static final TreeSet<Integer> empty = new TreeSet<Integer>();
    public final ArrayList<Consumer<Integer>> trueLiteralObservers = new ArrayList<>();
    public final ArrayList<BiConsumer<Integer,Integer>> implicationObservers = new ArrayList<>();
    private final ReentrantReadWriteLock rwl = new ReentrantReadWriteLock();
    private final Lock readLock = rwl.readLock();
    private final Lock writeLock = rwl.writeLock();


    /** constructs an empty Implication Graph
     *
     * @param predicates
     */
    public ImplicationGraph(int predicates) {
        this.predicates = predicates;}

    public void readLock() {readLock.lock();}
    public void readUnLock() {readLock.unlock();}

    /** returns for the literal all the implied literals.
     * Access to it must be synchronized.
     *
     * @param literal a literal (positive or negative predicate)
     * @return null or a list of implied literals
     */
    public TreeSet<Integer> getImplicants(int literal) {
        readLock.lock();
        try{
            TreeSet<Integer> implied = implicants.get(literal);
            return (implied == null) ? empty : implied;}
        finally {readLock.unlock();}}

    /** checks if from implies to
     *
     * @param from a literal
     * @param to   a literal
     * @return true if from implies to
     */
    public boolean implies(int from, int to) {
        readLock.lock();
        try{
            TreeSet<Integer> implied = implicants.get(from);
            return (implied == null) ? false : implied.contains(to);}
        finally {readLock.unlock();}}



    /** adds a clause to the Implication Graph
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return the derived unit disjunctions (if any), or null
     */
    public void addClause(int literal1, int literal2) {
        addImplication(-literal1,literal2);
        addImplication(-literal2,literal1);}



    /** adds an implication ante -> succ to the graph.
     * Puts derived unit disjunctions into units.
     *
     * @param ante a literal
     * @param succ a literal
     */
    public void addImplication(int ante, int succ) {
        writeLock.lock();
        try{
            TreeSet<Integer> antelist = implicants.get(ante);
            if(antelist == null) {antelist = new TreeSet<>(); implicants.put(ante,antelist);}
            if(antelist.contains(succ)) {return;}   // double occurrence
            if(antelist.contains(-succ)) {reportTrueLiteral(-ante);} // ante -> -succ,succ   -> -ante can be derived as unit literal
            else                         {reportImplication(ante,succ);}

            antelist.add(succ);  // one must continue, otherwise removeLiteral won't find all occurrences.
            TreeSet<Integer> succlist = implicants.get(succ);  // ante -> p1,...,pn,succ  succ->q1,...,qm
            if(succlist != null) {join(ante,antelist,succlist);}

            TreeSet<Integer> neglist = implicants.get(-ante);   // x -> ante
            if(neglist != null) {
                for(Integer negliteral : neglist) {join(-negliteral,implicants.get(-negliteral),antelist);}}}
        finally{writeLock.unlock();}}

    /** joins to lists of implied literals, e.g. p -> a1,..,q,..,a2  q -> b1...bm.
     * puts derived unit disjunctions into units.
     *
     * @param ante     a literal
     * @param antelist the consequences of ante
     * @param succlist consequences of one of ante's consequences
     */
    private void join(int ante, TreeSet<Integer> antelist, TreeSet<Integer> succlist) {
        if(succlist != null) {
            for(Integer succ : succlist) {
                if(succ == -ante) {reportTrueLiteral(-ante);}
                if(antelist.contains(succ)) {continue;}
                if(antelist.contains(-succ)){reportTrueLiteral(-ante);}
                else                        {reportImplication(ante,succ);}
                antelist.add(succ);}}}

    public void makeTrue(int literal) {
        writeLock.lock();
        try{
            TreeSet<Integer> consequences = implicants.get(literal);
            remove(literal);
            remove(-literal);
            if(consequences != null) {
                for(Integer lit : consequences) {
                    reportTrueLiteral(lit);
                    remove(lit);
                    remove(-lit);}}}
        finally{writeLock.unlock();}}


    /** removes a literal and its consequences from the Implication Graph
     *
     * @param literal the literal to be removed.
     */
    public void remove(int literal) {
        writeLock.lock();
        try{
            TreeSet<Integer> list = implicants.get(-literal);
            if(list != null) {
                list.remove((Integer) literal);  // when unit disjunctions have been derived
                for(Integer succ : list) {  // remove the literal from all implications
                    TreeSet<Integer> succlist = implicants.get(-succ);
                    if(succlist != null) {
                        succlist.remove((Integer)(-succ));
                        succlist.remove((Integer)literal);
                        if(succlist.isEmpty()) {implicants.remove(-succ);}}}}
            implicants.remove(literal);}
        finally{writeLock.unlock();}}


    public void replaceByRepresentative(int representative, int literal) {
        writeLock.lock();
        try{
            TreeSet rep = getImplicants(representative);
            TreeSet lit = getImplicants(literal);
            if(rep == null) {return;}
            if(lit == null) {implicants.put(representative,lit);}
            else {join(representative,rep,lit);}
            for(int r : getImplicants(-literal)) {
                TreeSet mr = getImplicants(-r);
                mr.remove(literal);
                mr.add(representative);
                reportImplication(-r,representative);}}
        finally{writeLock.unlock();}}

    /** assigns missing truth values to the predicates
     *
     * @param model a possibly incomplete model
     * @return 0 in case of success, otherwise the literal whose assigment failed.
     */
    public int completeModel(Model model) {
        writeLock.lock();
        try{
            for(int predicate = 1; predicate <= predicates; ++predicate) {
                if(model.status(predicate) != 0) {continue;}
                if(implicants.get(predicate) != null && !implicants.get(predicate).isEmpty()) {
                    model.add(predicate);
                    for(Integer literal : implicants.get(predicate)) {
                        if(model.add(literal) < 0) {return literal;}}
                    for(Integer literal : implicants.get(-predicate)) {
                        if(model.add(-literal) < 0) {return -literal;}}
                    continue;}
                if(implicants.get(-predicate) != null && !implicants.get(-predicate).isEmpty()) {
                    model.add(-predicate);
                    for(Integer literal : implicants.get(-predicate)) {
                        if(model.add(-literal) < 0) {return -literal;}}
                    for(Integer literal : implicants.get(predicate)) {
                        if(model.add(literal) < 0) {return literal;}}
                    continue;}
                if(model.add(predicate) < 0) {return predicate;}}
            return 0;}
        finally{writeLock.unlock();}}


    private void reportTrueLiteral(int literal) {
        for(Consumer<Integer> observer : trueLiteralObservers) {observer.accept(literal);}}

    private void reportImplication(int from, int to) {
        for(BiConsumer<Integer,Integer> observer : implicationObservers) {observer.accept(from,to);}}


    /** generates a String representation of the graph:</br>
     *  1 -&gt; ...</br>
     * -1 -&gt; ...
     *
     * @return a String representation of the graph:
     */
    public synchronized String toString(){
        StringBuffer st = new StringBuffer();
        int numbersize = ("-"+predicates).length();
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            TreeSet<Integer> list = implicants.get(predicate);
            if(list != null) {
                st.append(String.format("%"+numbersize+"d->",predicate));
                for(Integer literal : list) {st.append(literal.toString()).append(",");}
                st.append("\n");}
            list = implicants.get(-predicate);
            if(list != null) {
                st.append(String.format("%"+numbersize+"d",-predicate)).append("->");
                for(Integer literal : list) {st.append(literal.toString()).append(",");}
                st.append("\n");}}
        return st.toString();}

}
