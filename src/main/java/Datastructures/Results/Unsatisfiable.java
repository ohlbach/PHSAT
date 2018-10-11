package Datastructures.Results;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;

/** This class represents the final reason for an unsatisfiability in the clauses.
 * Created by ohlbach on 14.09.2018.
 */
public class Unsatisfiable extends Result {
    private String reason;

    /** creates an Unsatisfiable object with a reason
     *
     * @param reason for the unsatisfiability
     */
    public Unsatisfiable(String reason) {
        this.reason = reason;}

    /** creates an Unsatisfiable object for a false clause
     *
     * @param model   a model
     * @param clause a false clause in the model
     */
    public Unsatisfiable(Model model, int[] clause, Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append("The input clause ");
        st.append(BasicClauseList.clauseToString((""+clause[0]).length(),clause,symboltable));
        st.append( " is false in the model ");
        st.append(model.toString());
        reason = st.toString();}

    /** creates an Unsatisfiable object for a literal which became false in the model
     *
     * @param model   a model
     * @param literal a derived false unit literal
     */
    public Unsatisfiable(Model model, int literal) {
        reason = "The literal " + literal + " is false in the model " + model.toString();}

    /** just returns the reason for the unsatisfiability
     *
     * @return the reason for the unsatisfiability
     */
    public String toString() {return reason;}
}
