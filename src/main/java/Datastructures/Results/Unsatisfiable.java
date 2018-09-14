package Datastructures.Results;

import Datastructures.Theory.Model;

/**
 * Created by ohlbach on 14.09.2018.
 */
public class Unsatisfiable extends Result {
    public String reason;

    public Unsatisfiable(String reason) {
        this.reason = reason;
    }

    public Unsatisfiable(Model model, int[] clause) {

    }

    public Unsatisfiable(Model model, int literal) {

    }
}
