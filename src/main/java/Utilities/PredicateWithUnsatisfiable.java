package Utilities;

import Datastructures.Results.Unsatisfiable;

@FunctionalInterface
public interface PredicateWithUnsatisfiable<T> {
    boolean test(T item) throws Unsatisfiable;
}
