package Datastructures;

import com.sun.istack.internal.Nullable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.ArrayList;

/**
 * Created by ohlbach on 25.08.2018.
 * The symboltable maps predicates to names.
 * A predicate is a positive integer.
 * A literal is a positive or negative integer.
 */

public class Symboltable {
    /** number of predicates */
    public int predicates;

    /** This array maps predicates to names */
    private String[] names;

    /** creates a new symboltable for a given number of prediates.
     *
     * @param predicates the number of predicates.
     */
    public Symboltable(int predicates) {
        this.predicates = predicates;
        names = new String[predicates + 1];}

    /** sets the name of the predicate
     *
     * @param predicate any predicate
     * @param name  the name of the predicate
     */
    public void setName(int predicate, String name) {
        assert predicate > 0 && predicate <= predicates;
        names[predicate] = name;}

    /** returns for a given predicate name the corresponding number.
     * If the name is new, it gets the first empty number
     *
     * @param name of a predicate
     * @return its number, or 0 if there is no free number any more.
     */
    public int getPredicate(String name) {
        int emptyPosition = 0;
        for(int predicate = 1; predicate <= predicates; ++predicate) {
            String pred = names[predicate];
            if(pred == null) {
                if(emptyPosition == 0) {emptyPosition = predicate;}
                continue;}
            if(pred.equals(name)) return predicate;}
        if(emptyPosition == 0) return 0;
        names[emptyPosition] = name;
        return emptyPosition;}

    /**
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

    /** yields the name of the literal as string
     *
     * @param literal     a literal
     * @param symboltable a symboltable, or null
     * @return the name of the literal as string.
     */
    public static String toString(int literal, Symboltable symboltable) {
        return (symboltable == null) ? Integer.toString(literal) : symboltable.toString(literal); }

    /** turns a list of literals into a comma separated string of literal names
     *
     * @param literals    a list of literals
     * @param symboltable null or a symboltable
     * @return the list as string
     */
    public static String toString(IntArrayList literals, Symboltable symboltable) {
        return toString(literals,",",symboltable);}



    /** turns a list of literals into a separator separated string of literal names
     *
     * @param literals    a list of literals
     * @param separator a string
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



    /**
     * @return the contents of the symboltable
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        for(int i = 1; i <= predicates; ++i) {
            String name = names[i];
            if(name != null) st.append(i + ":" + names[i]+",");}
        return st.toString();}


}
