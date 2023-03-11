package Datastructures.Results;

import Datastructures.Theory.Model;

/** The class just represents the model for the clause set.
 *
 * Created by ohlbach on 14.09.2018.
 */
public class Satisfiable extends Result {
    public Model model;

    public Satisfiable(Model model) {
        super();
        this.model = model;}


    public String toString() {
        return "Satisfiable with model: "+ model.toString();}

}
