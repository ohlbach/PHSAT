package Utilities;

import Datastructures.Results.Result;

@FunctionalInterface
public interface PredicateWithResult<T> {
    boolean test(T item) throws Result;
}
