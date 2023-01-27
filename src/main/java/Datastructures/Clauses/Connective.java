package Datastructures.Clauses;


/** The enumeration type specifies the allowed logical connectives.
 * Created by ohlbach on 14.09.2018.
 */
public enum Connective {
    /** logical or-connective */
    OR("o", ",",""),
    /** logical and-connective */
    AND("a","&","A-"),
    /** equivalence of predicates */
    EQUIV("e","=","E-"),
    /** interval connective, example: [2,4] p,q,r,s  (between 2 and 4 literals must be true) */
    INTERVAL("i",",","I-"),
    /** atleast connective, example: >= 2 p,q,r,s  (atleast 2 of the literals must be true) */
    ATLEAST(">=", ",", "L-"),
    /** atmost connective, example: &lt;= 2 p,q,r,s  (atmost 2 of the literals are true) */
    ATMOST("<=", ",","M-"),
    /** exactly connective, example: = 2 p,q,r,s  (exactly 2 of the literals must be true) */
    EXACTLY("=", ",","X-");

    /** to be used in the input clauses. */
    public String abbreviation;
    /** to be used for printing clauses */
    public String separator;
    /** to be used for clause names */
    public String prefix;

    /** This is the internally used constructor for connectives.
     *
     * @param abbreviation  to be used in the input clauses.
     * @param separator     to be used for printing the clauses.
     * @param prefix        to be used for clause names.
     */
    Connective(String abbreviation, String separator, String prefix) {
        this.abbreviation = abbreviation;
        this.separator = separator;
        this.prefix = prefix;}

    /** returns for the given number the corresponding connective.
     *
     * @param ordninal any number
     * @return null or the corresponding type
     */
    public static Connective getConnective(int ordninal) {
        switch(ordninal) {
            case 0: return OR;
            case 1: return AND;
            case 2: return EQUIV;
            case 3: return INTERVAL;
            case 4: return ATLEAST;
            case 5: return ATMOST;
            case 6: return EXACTLY;}
        return null;}

    /** returns for the given string the corresponding connective.
     *
     * @param abbreviation the abbreviation for the connective
     * @return null or the corresponding connective
     */
    public static Connective getConnective(String abbreviation) {
        switch(abbreviation) {
            case "o":  return OR;
            case "a":  return AND;
            case "e":  return EQUIV;
            case "i":  return INTERVAL;
            case "<=": return ATMOST;
            case ">=": return ATLEAST;
            case "=":  return EXACTLY;}
        return null;}

    /** checks if the ordinal is the Interval connective.
     *
     * @param ordinal any integer
     * @return true if it is the Interval connective
     */
    public static boolean isInterval(int ordinal) {
        return ordinal == 3;}

    /** checks if the connective is the Interval connective.
     *
     * @return true if it is the Interval connective
     */
    public boolean isInterval() {
        return this == Connective.INTERVAL;}


    /** checks if the connective is a quantifier (atleast, atmost, exactly).
     *
     * @param n any number
     * @return true if it is one of the quantifier numbers
     */
    public static boolean isQuantifier(int n) {
        return n >= 3 && n <= 6;}

    /** checks if the connective is a quantifier (atleast, atmost, exactly).
     *
     * @return true if the connective is a quantifier (atleast, atmost, exactly)
     */
    public boolean isQuantifier() {
        return ordinal() >= 3;}

    /** the number of connectives.
     * @return the number of connectives*/
    public static int size() {
        return 7;}

    /** returns the abbreviation.
     *
     * @return the abbreviation.
     */
    public String toString() {
        return abbreviation;}


}
