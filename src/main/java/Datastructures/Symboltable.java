package Datastructures;

import com.sun.istack.internal.Nullable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

/**
 * Created by ohlbach on 25.08.2018.
 * The symboltable maps predicates to names.
 * A predicate is a positive integer.
 * A literal is a positive or negative integer.
 */

public class Symboltable {
    /**
     * size is the number of predicates
     */
    public int size;

    /**
     * This array maps predicates to names
     */
    private String[] names;

    public Symboltable(int size) {
        this.size = size;
        names = new String[size+1];
    }

    /**
     * @param predicate  any predicate
     * @return  the names of the predicate
     */
    public String getPredicateName(int predicate) {
        assert predicate > 0;
        return names[predicate];
    }

    /** yields the name of the literal as string
     *
     * @param literal     a literal
     * @param symboltable a symboltable, or null
     * @return the name of the literal as string.
     */
    public static String getLiteralName(int literal, Symboltable symboltable) {
        return (symboltable == null) ? Integer.toString(literal) : symboltable.getLiteralName(literal); }

    /** turns a list of literals into a comma separated string of literal names
     *
     * @param literals    a list of literals
     * @param symboltable null or a symboltable
     * @return the list as string
     */
    public static String getLiteralNames(IntArrayList literals, @Nullable Symboltable symboltable) {
        if(literals == null) {return "";}
        StringBuilder string = new StringBuilder();
        int size = literals.size();
        for(int i = 0; i < size; ++i) {
            string.append(getLiteralName(literals.getInt(i),symboltable));
            if(i < size-1) string.append(",");}
        return string.toString(); }

    /** turns a list of literals into a separator separated string of literal names
     *
     * @param literals    a list of literals
     * @param separartor a string
     * @param symboltable null or a symboltable
     * @return the list as string
     */
    public static String getLiteralNames(IntArrayList literals, String separartor, @Nullable Symboltable symboltable) {
        if(literals == null) {return "";}
        StringBuilder string = new StringBuilder();
        int size = literals.size();
        for(int i = 0; i < size; ++i) {
            string.append(getLiteralName(literals.getInt(i),symboltable));
            if(i < size-1) string.append(separartor);}
        return string.toString(); }


    /**
     * @param literal any positive or negative predicate
     * @return  the name of the literal (e.g. -A)
     */
    public String getLiteralName(int literal) {
        assert  literal != 0 && Math.abs(literal) <= size;
        String name;
        if(literal > 0) {
            name = names[literal];
            return name == null ? Integer.toString(literal) : name;}
        name = names[-literal];
        return name == null ? Integer.toString(literal) : "-"+name;}

    /**
     * @param predicate any predicate
     * @param name  sets the name of the predicate
     */
    public void setName(int predicate, String name) {
        assert predicate > 0 && predicate <= size;
        names[predicate] = name;}

    /**
     * @return the contents of the symboltable
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        for(int i = 1; i <= size; ++i) {
            st.append(i + ":" + names[i]+",");}
        return st.toString();}


}
