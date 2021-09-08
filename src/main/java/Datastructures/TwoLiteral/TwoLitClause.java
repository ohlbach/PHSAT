package Datastructures.TwoLiteral;

import Datastructures.Symboltable;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import static Utilities.Utilities.joinIntArrays;
import static Utilities.Utilities.sortIntArray;

/** A two-literal clause is usd for various special purposes.
    Therefore this special class.
**/

public class TwoLitClause {
    public int id;                // the identifier
    public int literal1;          // the first literal
    public int literal2;          // the second literal
    public IntArrayList origins;  // the list of basic clause ids which caused this clause.

    /** constructs a new two-literal clause
     *
     * @param literal1  the first literal
     * @param literal2  the second literal
     * @param origins    the list of basic clause ids which caused this clause.
     */
    public TwoLitClause(int id, int literal1, int literal2, IntArrayList origins) {
        this.id = id;
        this.literal1 = literal1;
        this.literal2 = literal2;
        this.origins = origins;}

    /** joins the origins */
    public void joinOrigins(IntArrayList origins) {
        this.origins = joinIntArrays(this.origins,origins); }


    /** turns this clause into a string */
    public String toString() {
        return "2-" + id + ": " + literal1 + "," + literal2;
    }

    /** turns this clause into a string using the symbol table
     *
     * @param symboltable null or a symboltable
     * @param prefix  for the string
     * @return the clause as a string.
     */
    public String toString(String prefix, Symboltable symboltable) {
        if(symboltable == null) return prefix + this;
        return prefix + "2-" + id + ": " + symboltable.toString(literal1) + "," + symboltable.toString(literal2); }

    /** turns this clause together with the origins into a string using the symbol table
     *
     * @param symboltable null or a symboltable
     * @param prefix  for the string
     * @return the clause as a string.
     */
    public String infoString(String prefix,Symboltable symboltable) {
        String st = toString(prefix,symboltable);
        if(origins != null) st += "@"+sortIntArray(origins).toString();
        return st;}
    }





