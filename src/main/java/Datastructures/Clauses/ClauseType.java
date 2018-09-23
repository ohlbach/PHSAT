package Datastructures.Clauses;


/**
 * Created by ohlbach on 14.09.2018.
 */
public enum ClauseType {
    OR('o'), AND('a'), XOR('x'), DISJOINT('d'), EQUIV('e');

    public char abbreviation;

    ClauseType(char abbreviation) {
        this.abbreviation = abbreviation;}

    public static ClauseType getType(int n) {
        assert n >= 0 && n < 5;
        switch(n) {
            case 0: return OR;
            case 1: return AND;
            case 2: return XOR;
            case 3: return DISJOINT;
            case 4: return EQUIV;
        }
        return null;
    }

    public static ClauseType getType(char n) {
        switch(n) {
            case 'o': return OR;
            case 'a': return AND;
            case 'x': return XOR;
            case 'd': return DISJOINT;
            case 'e': return EQUIV;
        }
        return null;
    }


}
