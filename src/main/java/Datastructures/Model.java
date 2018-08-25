package Datastructures;

/**
 * Created by Ohlbach on 25.08.2018.
 */
public class Model {
    private int maxSize;
    private int actualSize;
    private int[] model;
    private short[] truth;

    public Model(int size) {
        maxSize = size;
        model = new int[size];
        truth = new short[size+1];}

    public void pushLiteral(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= maxSize;
        model[actualSize++] = literal;
        truth[predicate] = literal > 0 ? (short)1: (short)-1;}

    public void pop() {
        int literal = model[actualSize--];
        truth[Math.abs(literal)] = 0;}

    public boolean isTrue(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= maxSize;
        return truth[predicate] == (short)1;}

    public boolean isFalse(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= maxSize;
        return truth[predicate] == (short)-1;}

    public short truth(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= maxSize;
        return truth[predicate];}

    public boolean isAsserted(int literal) {
        int predicate = Math.abs(literal);
        assert predicate <= maxSize;
        return truth[predicate] != 0;}

    public String toString() {
        StringBuffer st = new StringBuffer();
        for(int i = 0; i < actualSize; ++i) {
            st.append(Integer.toString(model[i])).append(",");}
        return st.toString();}

}
