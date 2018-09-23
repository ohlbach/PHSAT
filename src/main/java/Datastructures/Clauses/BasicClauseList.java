package Datastructures.Clauses;

import Datastructures.LocalModel;
import Datastructures.Symboltable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.InputMismatchException;

/**
 * Created by Ohlbach on 03.09.2018.<br/>
 *
 * This class contains just the absolutely essential information about clauses.
 * The clauses should be the original clauses from the clause source and must not be changed.
 * They are used to check a candidate model against the original clause set.
 */
public class BasicClauseList {
    public int predicates;

    /** the original clauses */
    public ArrayList<int[]> clauses = new ArrayList<>();

    public String info = null;

    public Symboltable symboltable = null;


    /** computes a list of clauses which are false in the model
     *
     * @param model a model for the literals of the clause
     * @return null or a list of false clauses.
     */
    public ArrayList<int[]> falseClauses(LocalModel model) {
        ArrayList<int[]> falseClauses = new ArrayList<>();

        if(false ) {//withDisjointness) {
            for(int[] clause : clauses) {
                int trueLiteral = 0;
                if(clause[0] == 0) {
                    for(int i = 1; i < clause.length; ++i) {
                        if(model.isTrue(clause[i])) {trueLiteral = clause[i]; break;}}
                    if(trueLiteral == 0) {falseClauses.add(clause);}}
                else {
                    for(int i = 1; i < clause.length; ++i) {
                        if(model.isTrue(clause[i])) {
                            if(trueLiteral != 0) {falseClauses.add(clause); break;}
                            trueLiteral = clause[i];}}}}}
        else {
            for(int[] clause : clauses) {
                int trueLiteral = 0;
                for(int literal : clause) {if(model.isTrue(literal)) {trueLiteral = literal; break;}}
                if(trueLiteral == 0) {falseClauses.add(clause);}}}

        return falseClauses.isEmpty() ? null : falseClauses;}

    /** generates a string representation of the clauses
     *
     * @return  a string representation of the clauses.
     */
    public String toString() {
        return toString(null);}

    /** generates a string representation of the clauses
     *
     * @return  a string representation of the clauses.
     */
    public String toString(Symboltable symboltable) {
        if(symboltable == null) {return toString();}
        StringBuilder st = new StringBuilder();
        int length = (""+clauses.size()).length();
        for(int[] clause : clauses) {
            st.append(String.format("%"+length+"d ",clause[0]));
            st.append(ClauseType.getType(clause[1]).abbreviation).append(": ");
            for(int i = 2; i < clause.length; ++i) {
                int literal = clause[i];
                if(symboltable != null) {st.append(symboltable.getLiteralName(literal));}
                else {st.append(Integer.toString(literal));}
                if(i < clause.length -1) {st.append(",");}}
            st.append("\n");}
        return st.toString();}



}
