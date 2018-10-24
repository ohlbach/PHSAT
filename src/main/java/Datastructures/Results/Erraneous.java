package Datastructures.Results;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;

import java.util.ArrayList;

/** This class represents results of erraneous SAT-algorithms.
 * They deliver a model which still makes some clauses false.
 *
 * Created by ohlbach on 11.10.2018.
 */
public class Erraneous extends Result {
    public Model model;
    public ArrayList<int[]> falseClauses;
    public Symboltable symboltable;

    /** creates an Erraneous object with a model and a list of false clauses
     *
     * @param model        a model
     * @param falseClauses a list of false clauses
     * @param symboltable  a symbol table (optional)
     */
    public Erraneous(Model model, ArrayList<int[]> falseClauses, Symboltable symboltable) {
        this.model = model;
        this.falseClauses = falseClauses;
        this.symboltable = symboltable;}

    /** describes the false clauses and the model
     *
     * @return a description of the false clauses and the model.
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        int size = 0;
        for(int[] clause :falseClauses) {size = Math.max(size, (""+clause[0]).length());}
        st.append("SAT Error: the following clauses are false in the model:\n");
        for(int[] clause :falseClauses) {st.append(BasicClauseList.clauseToString(size,clause,symboltable)).append("\n");}
        st.append("Model:\n").append(model.toString());
        return st.toString();}

}
