package Datastructures.Clauses;


/** The enumeration type specifies the possible clause types.
 * Created by ohlbach on 14.09.2018.
 */
public enum ClauseType {
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

    ClauseType(String abbreviation, String separator, String prefix) {
        this.abbreviation = abbreviation;
        this.separator = separator;
        this.prefix = prefix;}

    /** returns for the given number the corresponding type
     *
     * @param n any number
     * @return null or the corresponding type
     */
    public static ClauseType getType(int n) {
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
    public static ClauseType getType(String n) {
        switch(n) {
            case "o":  return OR;
            case "a":  return AND;
            case "e":  return EQUIV;
            case "<=": return ATLEAST;
            case ">=": return ATMOST;
            case "=":  return EXACTLY;}
        return null;}

    /** checks if the type is numeric (atleast, atmost, exactly)
     *
     * @param n any number
     * @return true if it is one of the numeric type numbers
     */
    public static boolean isNumeric(int n) {
        return n >= 3 && n <= 5;}

    /** checks if the type is numeric (atleast, atmost, exactly)
     *
     * @return true if the type is numeric (atleast, atmost, exactly)
     */
    public boolean isNumeric() {
        return ordinal() >= 3;}


}
