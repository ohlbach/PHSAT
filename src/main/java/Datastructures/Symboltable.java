package Datastructures;

import it.unimi.dsi.fastutil.ints.IntArrayList;

/** Created by ohlbach on 25.08.2018.<br>
 *
 * The symboltable maps predicate names to integers.<br>
 *
 * In source files with clauses one may use symbolic names for predicates, but
 * internally a predicate is a positive integer and
 * a literal is a positive or negative integer.<br>
 * The class guarantees injectivity of the mapping.
 * This means that different symbolic names are always mapped to different integers.
 */

public class Symboltable {

    /** Indicates that symbolic predicate names are used. */
    public boolean symbolic = true;

    /** Number of predicates */
    public int predicates;

    /** This array maps predicates (integers) to their names */
    private final String[] names;

    /** Creates a new symboltable for a given number of predicates.
     *
     * @param predicates the number of predicates.
     */
    public Symboltable(int predicates) {
        assert predicates > 0;
        this.predicates = predicates;
        names = new String[predicates + 1];}

    /** Sets the name of the predicate.
     *
     * @param predicate any predicate
     * @param name  the name of the predicate
     */
    public void setName(int predicate, String name) {
        assert predicate > 0 && predicate <= predicates;
        names[predicate] = name;}

    /** The index of the first position in the names array which is empty. */
    private int emptyPosition = 0;

    /** The method returns for a given predicate name the corresponding integer.
     * If the name is new, it gets the index of the first empty position in the names-array.
     *
     * @param name of a predicate
     * @return its number, or 0 if there is no free number anymore.
     */
    public int getPredicate(String name) {
        for(int predicate = 1; predicate <= emptyPosition; ++predicate) {
            if(names[predicate].equals(name)) return predicate;}
        emptyPosition += 1;
        if(emptyPosition > predicates) return 0;
        names[emptyPosition] = name;
        return emptyPosition;}

    /** The method gets the name of the literal, or null if it is unknown.
     *
     * @param literal a literal
     * @return        null or the name of the literal
     */
    public String getLiteral(int literal) {
        int predicate = Math.abs(literal);
        assert 0 < predicate && predicate <= predicates;
        String name = names[predicate];
        if(name == null) return null;
        return literal > 0 ? name : "-"+name;}

    /** Turns the integer into a string, either the name of the literal, or the integer as string.
     *
     * @param literal any positive or negative predicate
     * @return  the name of the literal (e.g. -A), or the literal as string
     */
    public String toString(int literal) {
        assert  literal != 0 && Math.abs(literal) <= predicates;
        String name;
        if(literal > 0) {
            name = names[literal];
            return name == null ? Integer.toString(literal) : name;}
        name = names[-literal];
        return name == null ? Integer.toString(literal) : "-"+name;}

    /** Yields the name of the literal as string.
     *
     * @param literal     a literal
     * @param symboltable a symboltable, or null
     * @return the name of the literal as string.
     */
    public static String toString(int literal, Symboltable symboltable) {
        return (symboltable == null || !symboltable.symbolic) ?
                Integer.toString(literal) : symboltable.toString(literal); }

    /** Turns a list of literals into a comma separated string of literal names.
     *
     * @param literals    a list of literals
     * @param symboltable null or a symboltable
     * @return the list as string
     */
    public static String toString(IntArrayList literals, Symboltable symboltable) {
        return toString(literals,",",symboltable);}



    /** The method turns a list of literals into a separator separated string of literal names.
     *
     * @param literals    a list of literals
     * @param separator   a string
     * @param symboltable null or a symboltable
     * @return the list as string
     */
    public static String toString(IntArrayList literals, String separator, Symboltable symboltable) {
        if(literals == null) {return "";}
        StringBuilder string = new StringBuilder();
        int size = literals.size();
        for(int i = 0; i < size; ++i) {
            string.append(toString(literals.getInt(i),symboltable));
            if(i < size-1) string.append(separator);}
        return string.toString(); }



    /** The method turns the entire symboltable into a string.
     *
     * @return the contents of the symboltable
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        for(int i = 1; i <= predicates; ++i) {
            String name = names[i];
            if(name != null) st.append(i + ":" + names[i]+",");}
        return st.toString();}


}
