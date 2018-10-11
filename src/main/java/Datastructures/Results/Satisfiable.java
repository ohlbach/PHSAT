package Datastructures.Results;

import Datastructures.Theory.Model;

/** The class just represents the model for the clause set.
 *
 * Created by ohlbach on 14.09.2018.
 */
public class Satisfiable extends Result {
    public Model model;

    public Satisfiable(Model model) {
        this.model = model;}

    public boolean isOkay() {return false;}

    public String toString() {
        return model.toString();}

}
