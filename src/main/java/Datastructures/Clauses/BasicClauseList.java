package Datastructures.Clauses;

import Datastructures.Model;

import java.util.ArrayList;
import java.util.Arrays;

/**
 * Created by Ohlbach on 03.09.2018.<br/>
 *
 * This class contains just the absolutely essential information about clauses.
 * The clauses should be the original clauses from the clause source and must not be changed.
 * They are used to check a candidate model against the original clause set.
 */
public class BasicClauseList {
    /** the original clauses */
    public ArrayList<int[]> clauses = new ArrayList<>();


    /** computes a list of clauses which are false in the model
     *
     * @param model a model for the literals of the clause
     * @return null or a list of false clauses.
     */
    public ArrayList<int[]> falseClauses(Model model) {
        ArrayList<int[]> falseClauses = new ArrayList<>();

        for(int[] clause : clauses) {
            int trueLiteral = 0;
            for(int literal : clause) {if(model.isTrue(literal)) {trueLiteral = literal; break;}}
            if(trueLiteral == 0) {falseClauses.add(clause);}}
        return falseClauses.isEmpty() ? null : falseClauses;}

    /** generates a string representation of the clauses
     *
     * @return  a string representation of the clauses.
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        for(int[] clause : clauses) {
            st.append(Arrays.toString(clause)).append("\n");}
        return st.toString();}
}
