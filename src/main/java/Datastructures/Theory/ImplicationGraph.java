package Datastructures.Theory;

import com.sun.xml.internal.ws.message.ByteArrayAttachment;
import org.omg.PortableInterceptor.INACTIVE;

import java.util.*;

/**
 * Created by ohlbach on 29.08.2018.
 *
 * The Implication Graph is a representation of two-literal clauses.
 * A clause p,q is equivalent to the two implications -p -> q and -q -> p.
 * The Implication Graph represents for each literal all the consequences with can be derived
 * from the two-liteal clauses.
 */
public class ImplicationGraph {
    private final int predicates;   // number of predicates (used only in toString())
    private final HashMap<Integer,TreeSet<Integer>> implicants = new HashMap<>();  // contains for each literal the implied literals
    private final HashSet<Integer> units = new HashSet<>(); // an intermediate list for storing derived unit clause
    private final TreeSet<Integer> empty = new TreeSet<Integer>();
    private ArrayList<Integer> changes = new ArrayList<>();
    /** constructs an empty Implication Graph
     *
     * @param predicates
     */
    public ImplicationGraph(int predicates) {
        this.predicates = predicates;}

    /** returns for the literal all the implied literals
     *
     * @param literal a literal (positive or negative predicate)
     * @return null or a list of implied literals
     */
    public TreeSet<Integer> getImplicants(int literal) {
        TreeSet<Integer> implied = implicants.get(literal);
        return (implied == null) ? empty : implied;}


    /** adds a clause to the Implication Graph
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return the derived unit clauses (if any), or null
     */
    public HashSet<Integer> addClause(int literal1, int literal2) {
        units.clear();
        addFromTo(-literal1,literal2);
        addFromTo(-literal2,literal1);
        clear();
        return units;}

    public HashSet<Integer> addImplication(int from, int to) {
        units.clear();
        addFromTo(from,to);
        clear();
        return units;}

    /** adds an implication ante -> succ to the graph.
     * Puts derived unit clauses into units.
     *
     * @param ante a literal
     * @param succ a literal
     */
    private void addFromTo(int ante, int succ) {
        changes.add(ante);
        TreeSet<Integer> antelist = implicants.get(ante);
        if(antelist == null) {antelist = new TreeSet<>(); implicants.put(ante,antelist);}
        for(Integer literal :antelist) {    // ante -> p1,...pn,succ
            if((int)literal == succ) {return ;}  // double occurrence
            if((int)literal == -succ) {          // ante -> -succ,succ   -> -ante can be derived as unit literal
                addUnit(-ante);
                break;}}        // one must continue, otherwise removeLiteral won't find all occurrences.
        antelist.add(succ);

        TreeSet<Integer> succlist = implicants.get(succ);  // ante -> p1,...,pn,succ  succ->q1,...,qm
        if(succlist != null) {join(ante,antelist,succlist);}

        TreeSet<Integer> neglist = implicants.get(-ante);   // x -> ante
        if(neglist != null) {
            for(Integer negliteral : neglist) {join(-negliteral,implicants.get(-negliteral),antelist);}}}

    /** joins to lists of implied literals, e.g. p -> a1,..,q,..,a2  q -> b1...bm.
     * puts derived unit clauses into units.
     *
     * @param ante     a literal
     * @param antelist the consequences of ante
     * @param succlist consequences of one of ante's consequences
     */
    private void join(int ante, TreeSet<Integer> antelist, TreeSet<Integer> succlist) {
        if(succlist != null) {
            changes.add(ante);
            for(Integer succliteral : succlist) {
                if(succliteral == -ante) {addUnit(-ante);}
                boolean ignore = false;
                for(Integer anteliteral : antelist) {
                    if(anteliteral.equals(succliteral)) {ignore = true; break;}
                    if(anteliteral.equals(-succliteral)) {addUnit(-ante);}}
                if(!ignore) {antelist.add(succliteral);}}}}

    /** adds a newly derived unit clause together with its consequences into units.
     *
     * @param unit a derived unit clause
     */
    private void addUnit(Integer unit) {
        units.add(unit);
        TreeSet<Integer> consequences = implicants.get(unit);
        if(consequences != null) {units.addAll(consequences);}}

    /** removes the derived unit clauses from the Implication Graph
     */
    private void clear() {
        for(Integer unit : units) {
            remove(unit);
            remove(-unit);}}

    /** removes a literal and its negation from the Implication Graph
     *
     * @param literal  a literal to be removed
     */
    public void removeLiteral(int literal) {
        remove(literal);
        remove(-literal);
    }

    /** removes a literal and its consequences from the Implication Graph
     *
     * @param literal the literal to be removed.
     */
    private void remove(int literal) {
        TreeSet<Integer> list = implicants.get(-literal);
        if(list != null) {
            list.remove((Integer) literal);  // when unit clauses have been derived
            for(Integer succ : list) {  // remove the literal from all implications
                TreeSet<Integer> succlist = implicants.get(-succ);
                if(succlist != null) {
                    succlist.remove((Integer)(-succ));
                    succlist.remove((Integer)literal);
                    if(succlist.isEmpty()) {implicants.remove(-succ);}}}}
        implicants.remove(literal);}


    private ArrayList<ArrayList<Integer>> equivalences = new ArrayList<>();

    public ArrayList<ArrayList<Integer>> getEquivalences() {
        if(changes.isEmpty()) {return null;}
        equivalences.clear();
        for(int literal1 :changes) {
            for(int literal2: getImplicants(literal1)) {
                if(getImplicants(literal2).contains(literal1)) {
                    ArrayList<Integer> eqv = new ArrayList<>();
                    eqv.add(literal1); eqv.add(literal2);
                    equivalences.add(eqv);}}}
        for(int i = 0; i < equivalences.size(); ++i) {
            for(Integer literal : equivalences.get(i)) {
                for(int j = i+1; j < equivalences.size(); ++j) {
                    if(equivalences.get(j).contains(literal)) {
                        equivalences.get(i).addAll(equivalences.get(j));
                        equivalences.remove(j);}}}}
        changes.clear();
        return equivalences;}

    public HashSet<Integer> replaceEquivalences(int representative, int literal) {
        units.clear();
        TreeSet rep = getImplicants(representative);
        TreeSet lit = getImplicants(literal);
        if(rep == null) {return units;}
        if(lit == null) {implicants.put(representative,lit);}
        else {join(representative,rep,lit);}
        for(int r : getImplicants(-literal)) {
            TreeSet mr = getImplicants(-r);
            mr.remove(literal);
            mr.add(representative);}
        return units;}

    /** generates a String representation of the graph:</br>
     *  1 -&gt; ...</br>
     * -1 -&gt; ...
     *
     * @return a String representation of the graph:
     */
    public String toString(){
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
