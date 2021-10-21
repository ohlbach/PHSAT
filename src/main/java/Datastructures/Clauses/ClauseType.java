package Datastructures.Clauses;


/**
 * Created by ohlbach on 14.09.2018.
 */
public enum ClauseType {
    OR("o", ",",""),
    AND("a","&","A-"),
    EQUIV("e","=","E-"),
    ATLEAST("<=", ",", "X-"),
    ATMOST(">=", ",","D-"),
    EXACTLY("=", ",","D-");

    public String abbreviation;
    public String separator;
    public String prefix;

    ClauseType(String abbreviation, String separator, String prefix) {
        this.abbreviation = abbreviation;
        this.separator = separator;
        this.prefix = prefix;}

    public static ClauseType getType(int n) {
        assert n >= 0 && n < 5;
        switch(n) {
            case 0: return OR;
            case 1: return AND;
            case 2: return EQUIV;
            case 3: return ATLEAST;
            case 4: return ATMOST;
            case 5: return EXACTLY;
        }
        return null;
    }

    public static ClauseType getType(String n) {
        switch(n) {
            case "o":  return OR;
            case "a":  return AND;
            case "e":  return EQUIV;
            case "<=": return ATLEAST;
            case ">=": return ATMOST;
            case "=":  return EXACTLY;}
        return null;}

    public static boolean isNumericType(int n) {
        return n >= 3;}


}
