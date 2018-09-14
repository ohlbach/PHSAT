package Datastructures;

import java.util.LinkedList;

/**
 * Created by ohlbach on 11.09.2018.
 */
public class TrueLiterals {

    private LocalModel model;
    private LinkedList<Integer> literals = new LinkedList();

    public TrueLiterals(LocalModel mode) {
        this.model = model;}

    public synchronized void addExternalLiteral(int literal) {
        literals.add(literal);}

    public synchronized boolean addLocalLiteral(int literal) {
        literals.add(literal);
        short status = model.push(literal);
        return status == (short)-1;}

    public synchronized int poll() {
        return literals.pollFirst();}

    public synchronized boolean isEmpty() {return literals.isEmpty();}

    public synchronized String toString() {
        return literals.toString();}

}
