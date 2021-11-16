package Datastructures.Clauses;


/** The enumeration type specifies the allowed logical connectives.
 * Created by ohlbach on 14.09.2018.
 */
public enum Connective {
    OR("o", ",",""),
    AND("a","&","A-"),
    EQUIV("e","=","E-"),
    ATLEAST("<=", ",", "L-"),
    ATMOST(">=", ",","M-"),
    EXACTLY("=", ",","X-");

    /** to be used in the input clauses */
    public String abbreviation;
    /** to be used for printing clauses */
    public String separator;
    /** to be used for clause names */
    public String prefix;

    Connective(String abbreviation, String separator, String prefix) {
        this.abbreviation = abbreviation;
        this.separator = separator;
        this.prefix = prefix;}

    /** returns for the given number the corresponding type
     *
     * @param n any number
     * @return null or the corresponding type
     */
    public static Connective getType(int n) {
        switch(n) {
            case 0: return OR;
            case 1: return AND;
            case 2: return EQUIV;
            case 3: return ATLEAST;
            case 4: return ATMOST;
            case 5: return EXACTLY;}
        return null;}

    /** returns for the given string the corresponding type
     *
     * @param n any number
     * @return null or the corresponding type
     */
    public static Connective getType(String n) {
        switch(n) {
            case "o":  return OR;
            case "a":  return AND;
            case "e":  return EQUIV;
            case "<=": return ATLEAST;
            case ">=": return ATMOST;
            case "=":  return EXACTLY;}
        return null;}

    /** checks if the connective is a quantifier (atleast, atmost, exactly)
     *
     * @param n any number
     * @return true if it is one of the quantifier numbers
     */
    public static boolean isQuantifier(int n) {
        return n >= 3 && n <= 5;}

    /** checks if the connective is a quantifier (atleast, atmost, exactly)
     *
     * @return true if the connective is a quantifier (atleast, atmost, exactly)
     */
    public boolean isQuantifier() {
        return ordinal() >= 3;}

    /** the number of connectives */
    public static int size() {
        return 6;}


}
