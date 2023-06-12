package Utilities;

import Datastructures.Results.Unsatisfiable;

@FunctionalInterface
public interface IntConsumerWithUnsatisfiable {
    void accept(int i) throws Unsatisfiable;
}
