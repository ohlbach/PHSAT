package Datastructures.Clauses;

import Datastructures.Model;
import Datastructures.Status;

import java.util.ArrayList;

/**
 * Created by ohlbach on 26.08.2018.
 *
 * This is a superclass for Clause Set Generators, such as CNFReader.
 */
public class ClauseSetGenerator {
    protected ClauseList clauseList = null;
    protected ClauseGenerator clauseGenerator;

    /** This method has to be overwritten in subclasses.
     * It has to generate a ClauseList which becomes part of the Status object.
     *
     * @return a Status object with all the information about the clause list.
     * @throws Exception
     */
    public Status generate() throws Exception {return null;}

    /** The method simplifies a list of literals with a given model.
     *  Literals which are false in the model are removed.
     *  Tautologies and double literals are not checked.
     *
     * @param literals the list to be simplified
     * @param model a model
     * @return +1 if a literal is true, -1 if the clauses became empty, and 0 otherwise.
     */
    public static int simplify(ArrayList<Integer> literals, Model model) {
        if(literals.size() == 0){return -1;}
        if(literals.size() == 1) {
            int literal = literals.get(0);
            if(model.isFalse(literal)) {return -1;}
            int stat = model.push(literal);
            if(stat < 0) {return -1;}
            if(stat > 0) {return 1;}
            return 0;}
        for(int i = 0; i < literals.size(); ++i){
            Integer literal = literals.get(i);
            if(model.isTrue(literal)) {return 1;}
            if(model.isFalse(literal)) {literals.remove(literal);--i;}}
        if(literals.isEmpty()) {return -1;}
        return 0;}
}
