package Solvers.Backtracker;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class Propagator extends Thread {

    final BlockingQueue<Object> queue = new LinkedBlockingQueue<>(3);
    Backtracker backtracker;

    @Override
    public void run() {
        try {
            while (!isInterrupted()) {
                backtracker = (Backtracker)queue.take();
                int literal = (Integer)queue.take();
                backtracker.propagate(literal);
            }
        } catch (InterruptedException e) {
        }
    }
    public void newPropagateJob(Backtracker backtracker, int literal) {
        queue.add(backtracker); queue.add(literal);}
}
