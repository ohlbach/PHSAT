package Datastructures.Results;

import Datastructures.Clauses.InputClauses;
import Datastructures.Clause;
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
    public ArrayList<int[]> falseClauses = null;
    public Clause falseClause = null;
    public Symboltable symboltable;

    /** creates an Erraneous object with a model and a list of false clauses
     *
     * @param model        a model
     * @param falseClauses a list of false clauses
     */
    public Erraneous(String problemId, String solverId, Model model, ArrayList<int[]> falseClauses) {
        super(problemId,solverId, "error");
        this.model = model;
        this.falseClauses = falseClauses;}

    /** creates an Erraneous object with a model and a list of false clauses
     *
     * @param model        a model
     * @param falseClause a list of false clauses
     */
    public Erraneous(String problemId, String solverId, Model model, Clause falseClause) {
        super(problemId,solverId,"error");
        this.model = model;
        this.falseClause = falseClause;}


    /** describes the false clauses and the model
     *
     * @return a description of the false clauses and the model.
     */
    @Override
    public String toString(Symboltable symboltable, long startTime) {
        StringBuilder st = new StringBuilder();
        st.append("SAT Error: the following clauses are not true in the model:\n");
        st.append("Model:\n").append(model.toString()).append("\nClauses:\n");
        if(falseClause != null) {st.append(falseClause.toString(symboltable,0)).append("\n");}
        else{int size = 0;
            for(int[] clause :falseClauses) {size = Math.max(size, (""+clause[0]).length());}
            for(int[] clause :falseClauses) {st.append(InputClauses.toString(size,clause,symboltable)).append("\n");}}
        return st.toString();}

}
