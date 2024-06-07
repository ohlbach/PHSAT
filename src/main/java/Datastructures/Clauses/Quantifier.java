package Datastructures.Clauses;


/** The enumeration type specifies the allowed logical connectives.
 * Created by ohlbach on 14.09.2018.
 */
public enum Quantifier {
    /** logical or-connective */
    OR("", "v",2),
    /** logical and-connective */
    AND("&","&",2),
    /** equivalence of predicates */
    EQUIV("e","=",2),
    /** atleast connective, example: >= 2 p,q,r,s  (atleast 2 of the predicates must be true) */
    ATLEAST(">=", ",", 3),
    /** atmost connective, example: &lt;= 2 p,q,r,s  (atmost 2 of the predicates are true) */
    ATMOST("<=", ",",3),
    /** exactly connective, example: = 2 p,q,r,s  (exactly 2 of the predicates must be true) */
    EXACTLY("=", ",",3),
    /** interval connective, example: [2,4] p,q,r,s  (between 2 and 4 predicates must be true) */
    INTERVAL("i",",",4);

    /** to be used in the input clauses. */
    public String abbreviation;
    /** to be used for printing clauses */
    public String separator;
    /** to be used for clause names */
    public int firstLiteralIndex;

    /** This is the internally used constructor for connectives.
     *
     * @param abbreviation      to be used in the input clauses.
     * @param separator         to be used for printing the clauses.
     * @param firstLiteralIndex the first index of the predicates in the InputClauses int[]-arrays.
     */
    Quantifier(String abbreviation, String separator, int firstLiteralIndex) {
        this.abbreviation = abbreviation;
        this.separator = separator;
        this.firstLiteralIndex = firstLiteralIndex;}

    /** returns for the given number the corresponding connective.
     *
     * @param ordninal any number
     * @return null or the corresponding type
     */
    public static Quantifier getQuantifier(int ordninal) {
        switch(ordninal) {
            case 0: return OR;
            case 1: return AND;
            case 2: return EQUIV;
            case 3: return ATLEAST;
            case 4: return ATMOST;
            case 5: return EXACTLY;
            case 6: return INTERVAL;}
        return null;}

    /** returns for the given string the corresponding connective.
     *
     * @param abbreviation the abbreviation for the connective
     * @return null or the corresponding connective
     */
    public static Quantifier getQuantifier(String abbreviation) {
        switch(abbreviation) {
            case "o":  return OR;
            case "a":  return AND;
            case "e":  return EQUIV;
            case "<=": return ATMOST;
            case ">=": return ATLEAST;
            case "=":  return EXACTLY;
            case "i":  return INTERVAL;}
        return null;}

    /**
     * Returns the ASCII representation of the given Quantifier.
     *
     * @param quantifier the quantifier to get the ASCII representation of
     * @return the ASCII representation of the given quantifier
     */
    public static String asciiName(Quantifier quantifier) {
        switch(quantifier) {
            case OR:       return "o";
            case AND:      return "a";
            case EQUIV:    return "eq";
            case ATMOST:   return "le";
            case ATLEAST:  return "ge";
            case EXACTLY:  return "ex";
            case INTERVAL: return "i";}
        return null;}



    /** checks if the ordinal is the Interval connective.
     *
     * @param ordinal any integer
     * @return true if it is the Interval connective
     */
    public static boolean isInterval(int ordinal) {
        return ordinal == 6;}

    /** checks if the connective is the Interval connective.
     *
     * @return true if it is the Interval connective
     */
    public boolean isInterval() {
        return this == Quantifier.INTERVAL;}


    /** checks if the connective is a quantifier (atleast, atmost, exactly).
     *
     * @param n any number
     * @return true if it is one of the quantifier numbers
     */
    public static boolean isQuantifier(int n) {
        return n >= 3;}

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
