package Utilities;

import Datastructures.Results.Unsatisfiable;

@FunctionalInterface
public interface BiIntConsumerWithUnsatisfiable<S> {
    void accept(int a,S b) throws Unsatisfiable;
}
