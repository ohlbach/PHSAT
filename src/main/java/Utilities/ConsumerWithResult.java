package Utilities;

import Datastructures.Results.Result;

@FunctionalInterface
public interface ConsumerWithResult<T> {
    void accept(T item) throws Result;}
