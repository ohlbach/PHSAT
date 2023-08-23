package Utilities;

import Datastructures.Results.Unsatisfiable;
@FunctionalInterface
public interface BiConsumerWithUnsatisfiable<T,S> {
    void accept(T item1,S item2) throws Unsatisfiable;}


