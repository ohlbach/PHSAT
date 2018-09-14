package Datastructures.Clauses;

/**
 * Created by ohlbach on 14.09.2018.
 */
public enum ClauseType {
    OR, AND, XOR, DISJOINT, EQUIV;

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
}
