package Datastructures.Theory;

import java.util.ArrayList;
import java.util.HashMap;

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
    private final HashMap<Integer,ArrayList<Integer>> implicants = new HashMap<>();  // contains for each literal the implied literals
    private final ArrayList<Integer> units = new ArrayList<>(); // an intermediate list for storing derived unit clause

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
    public ArrayList<Integer> getImplicants(int literal) {
        return implicants.get(literal);}

    /** adds a clause to the Implication Graph
     *
     * @param literal1 a literal
     * @param literal2 a literal
     * @return the derived unit clauses (if any)
     */
    public ArrayList<Integer> addClause(int literal1, int literal2) {
        ArrayList<Integer> units = addImplication(-literal1,literal2);
        if(units != null && units.size()==1 && units.get(0)==literal1) {return clear(units);}
        return clear(addImplication(-literal2,literal1));}

    /** adds an implication ante -> succ to the graph
     *
     * @param ante a literal
     * @param succ a literal
     * @return null or the derived unit clauses.
     */
    private ArrayList<Integer> addImplication(int ante, int succ) {
        units.clear();
        ArrayList<Integer> list = implicants.get(ante);
        if(list == null) {list = new ArrayList<>(); implicants.put(ante,list);}
        for(Integer literal :list) {            // ante -> p1,...pn,succ
            if(literal == succ) {return null;}  // double occurrence
            if(literal == -succ) {              // ante -> -succ,succ   -> -ante can be derived as unit literal
                units.add(-ante);
                ArrayList<Integer> antelist = implicants.get(-ante); // implicants of -ante become units.
                if(antelist != null) {units.addAll(antelist);}
                list.add(succ);  // removed in clear()
                return units;}}
        list.add(succ);

        ArrayList<Integer> succlist = implicants.get(succ);  // ante -> p1,...,pn,succ  succ->q1,...,qm
        if(succlist != null) {
            int unit = join(ante,list,succlist);             // ante -> succ,   succ -> -ante,   -ante can be derived
            if(unit != 0) {units.add(unit); return units;}}

        ArrayList<Integer> neglist = implicants.get(-ante);   // x -> ante
        if(neglist == null) {return null;}
        for(Integer negliteral : neglist) {
            int oldante = -negliteral;
            int unit = join(oldante,implicants.get(oldante),list);
            if(unit != 0) {
                units.add(unit);
                ArrayList<Integer> antelist = implicants.get(unit);
                if(antelist != null) {units.addAll(antelist);}
            }}
        return units.isEmpty() ? null : units;}

    /** joins to lists of implied literals, e.g. p -> a1,..,q,..,a2  q -> b1...bm
     *
     * @param ante     a literal
     * @param antelist the consequences of ante
     * @param succlist consequences of one of ante's conseqences
     * @return 0 or -ante if a contradiction has been discovered.
     */
    private int join(int ante, ArrayList<Integer> antelist, ArrayList<Integer> succlist) {
        if(succlist != null) {
            for(Integer succliteral : succlist) {
                if(succliteral == -ante) {return -ante;}
                boolean ignore = false;
                for(Integer anteliteral : antelist) {
                    if(anteliteral.equals(succliteral)) {ignore = true; break;}
                    if(anteliteral.equals(-succliteral)) {return -ante;}}
                if(!ignore) {antelist.add(succliteral);}}}
        return 0;}

    /** removes the derived unit clauses from the Implication Graph
     *
     * @param units the derived unit clauses
     * @return units (unchanged)
     */
    private ArrayList<Integer> clear(ArrayList<Integer> units) {
        if(units == null) {return null;}
        for(Integer unit : units) {
            ArrayList<Integer> list = implicants.get(unit);
            if(list != null) {
                for(Integer succ : list) {
                    ArrayList<Integer> succlist = implicants.get(-succ);
                    if(succlist != null) {
                        succlist.remove(-unit);
                        if(succlist.isEmpty()) {implicants.remove(-succ);}}}}
            list = implicants.get(-unit);
            if(list != null) {
                for(Integer succ : list) {
                    ArrayList<Integer> succlist = implicants.get(-succ);
                    if(succlist != null) {
                        succlist.remove(unit);
                        if(succlist.isEmpty()) {implicants.remove(-succ);}}}}
            System.out.println(unit);
            System.out.println(this);
            implicants.remove(unit);
            implicants.remove(-unit);}
        return units;
    }

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
            ArrayList<Integer> list = implicants.get(predicate);
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
