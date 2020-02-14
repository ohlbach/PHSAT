package Datastructures.Results;

import Datastructures.Clauses.BasicClauseList;
import Datastructures.Symboltable;
import Datastructures.Theory.Model;
import it.unimi.dsi.fastutil.ints.IntArrayList;

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
     * @param symboltable  a symbol table (optional)
     */
    public Unsatisfiable(Model model, int[] clause, Symboltable symboltable, IntArrayList origins) {
        StringBuilder st = new StringBuilder();
        st.append("The input clause ");
        st.append(BasicClauseList.clauseToString((""+clause[0]).length(),clause,symboltable));
        st.append( " is false in the model ");
        st.append(model.toString(symboltable));
        if(origins != null) {st.append("\n because of the clauses ").append(origins.toString());}
        reason = st.toString();}

    /** creates an Unsatisfiable object for a literal which became false in the model
     *
     * @param model   a model
     * @param literal a derived false unit literal
     * @param symboltable null or a symboltable
     * @param origins    the basic clause ids causing the unsatisfiability.
     */
    public Unsatisfiable(Model model, int literal, Symboltable symboltable, IntArrayList origins) {
        String name = (symboltable != null) ? symboltable.getLiteralName(literal) : Integer.toString(literal);
        IntArrayList origin = model.getOrigin(literal);
        if(origin == null) {origin = origins;}
        else {if(origins != null) {origin = origin.clone(); origin.addAll(origins);}}
        reason = "The literal " + name + " is false in the model " + model.toString();
        if(origin != null) {reason += "\nOrigns: " + origin.toString();}}


    /** just returns the reason for the unsatisfiability
     *
     * @return the reason for the unsatisfiability
     */
    public String toString() {return "Unsatisfiable: " + reason;}
}
