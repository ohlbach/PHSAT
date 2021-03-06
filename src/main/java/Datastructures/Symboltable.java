package Datastructures;

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

    /**
     * @param literal any positive or negative predicate
     * @return  the name of the literal (e.g. -A)
     */
    public String getLiteralName(int literal) {
        assert  literal != 0 && Math.abs(literal) <= size;
        return literal > 0 ? names[literal] : "-"+names[-literal];}

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
