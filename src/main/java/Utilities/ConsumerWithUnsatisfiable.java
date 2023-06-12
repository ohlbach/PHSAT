package Utilities;

import Datastructures.Results.Unsatisfiable;

@FunctionalInterface

public interface ConsumerWithUnsatisfiable<T> {
  void accept(T item) throws Unsatisfiable;}


