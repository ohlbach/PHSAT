package Solvers.Backtracker;

import java.util.ArrayList;

public class ThreadPool {
    ArrayList<Propagator> passivePropagators = new ArrayList<>();
    ArrayList<Propagator>[] activePropagators;

    public ThreadPool(int backtrackers) {
        activePropagators = new ArrayList[backtrackers];
    }

    public synchronized void addPropagatorJob(Backtracker backtracker, int literal) {
        int solverNumber = backtracker.solverNumber;
        if(passivePropagators.isEmpty()) {
            Propagator propagator = new Propagator();
            activePropagators[solverNumber].add(propagator);
            propagator.newPropagateJob(backtracker,literal);
            propagator.start();}
        else {
            Propagator propagator = passivePropagators.remove(passivePropagators.size() - 1);
            activePropagators[solverNumber].add(propagator);
            propagator.newPropagateJob(backtracker,literal);}}

    public synchronized void jobFinished(Backtracker backtracker) {
        int solverNumber = backtracker.solverNumber;
        passivePropagators.addAll(activePropagators[solverNumber]);
        activePropagators[solverNumber].clear();
    }
}
