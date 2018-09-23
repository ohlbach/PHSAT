
package Datastructures.Clauses;

import Datastructures.Symboltable;
import Datastructures.Theory.Model;

import java.util.ArrayList;

/**
 * Created by Ohlbach on 03.09.2018.<br/>
 *
 * This class contains just the absolutely essential information about disjunctions.
 * The disjunctions should be the original disjunctions from the clause source and must not be changed.
 * They are used to check a candidate model against the original clause set.
 */
public class BasicClauseList {
    public int predicates;

    /** the original disjunctions */
    public ArrayList<int[]> disjunctions  = new ArrayList<>();
    public ArrayList<int[]> conjunctions  = new ArrayList<>();
    public ArrayList<int[]> xor           = new ArrayList<>();
    public ArrayList<int[]> disjoints     = new ArrayList<>();
    public ArrayList<int[]> equivalences  = new ArrayList<>();

    public String info = null;

    public Symboltable symboltable = null;

    public void addClause(int[] clause) {
        switch(ClauseType.getType(clause[1])) {
            case OR:       disjunctions.add(clause); break;
            case AND:      conjunctions.add(clause); break;
            case XOR:      xor.add(clause);          break;
            case DISJOINT: disjoints.add(clause);    break;
            case EQUIV:    equivalences.add(clause); break;
        }
    }

    public void sortDisjunctions() {
        disjunctions.sort((clause1,clause2) -> Integer.compare(clause1.length,clause2.length));}

    public boolean disjunctionIsTrue(int[] clause, Model model) {
        for(int i = 2; i < clause.length; ++i) {if(model.isTrue(clause[i])) {return true;}}
        return false;}

    public boolean conjunctionIsTrue(int[] clause, Model model) {
        for(int i = 2; i < clause.length; ++i) {if(!model.isTrue(clause[i])) {return false;}}
        return true;}

    public boolean xorIsTrue(int[] clause, Model model) {
        int trueLiteral = 0;
        for(int i = 2; i < clause.length; ++i) {
            if(model.isTrue(clause[i])) {
                if(trueLiteral != 0) {return false;}
                trueLiteral = clause[i];}}
        return trueLiteral != 0;}


    public boolean disjointIsTrue(int[] clause, Model model) {
        int trueLiteral = 0;
        for(int i = 2; i < clause.length; ++i) {
            if(model.isTrue(clause[i])) {
                if(trueLiteral != 0) {return false;}
                trueLiteral = clause[i];}}
        return true;}

    public boolean equivalenceIsTrue(int[] clause, Model model) {
        int status = model.status(clause[2]);
        if(status == 0) {return false;}
        for(int i = 3; i < clause.length; ++i) {
            if(model.status(clause[i]) != status) {return false;}}
        return true;}




    /** computes a list of clauses which are false in the model
     *
     * @param model a model for the literals of the clause
     * @return null or a list of false clauses.
     */
    public ArrayList<int[]> falseClauses(Model model) {
        ArrayList<int[]> falseClauses = new ArrayList<>();
        for(int[] clause : disjunctions) {if(!disjunctionIsTrue(clause,model)) {falseClauses.add(clause);}}
        for(int[] clause : conjunctions) {if(!conjunctionIsTrue(clause,model)) {falseClauses.add(clause);}}
        for(int[] clause : xor)          {if(!xorIsTrue(clause,model))         {falseClauses.add(clause);}}
        for(int[] clause : disjoints)    {if(!disjointIsTrue(clause,model))    {falseClauses.add(clause);}}
        for(int[] clause : equivalences) {if(!equivalenceIsTrue(clause,model)) {falseClauses.add(clause);}}
        return falseClauses.isEmpty() ? null : falseClauses;}


    public String clauseToString(int size, int[] clause, Symboltable symboltable) {
        StringBuilder st = new StringBuilder();
        st.append(String.format("%"+size+"d ",clause[0]));
        st.append(ClauseType.getType(clause[0]).abbreviation).append(": ");
        int length = clause.length;
        for(int i = 2; i < length-1; ++i) {
            if(symboltable != null) {st.append(symboltable.getLiteralName(clause[i]));}
            else                    {st.append(Integer.toString(clause[i]));}
            st.append(",");}
        st.append(clause[length-1]);
        return st.toString();}

    /** generates a string representation of the disjunctions
     *
     * @return  a string representation of the disjunctions.
     */
    public String toString() {
        return toString(true);}

    /** generates a string representation of the disjunctions
     *
     * @return  a string representation of the disjunctions.
     */
    public String toString(boolean withSymbols) {
        Symboltable symboltable = withSymbols ? this.symboltable : null;
        if(symboltable == null) {return toString();}
        StringBuilder st = new StringBuilder();
        if(info != null) {st.append(info).append("\n");}
        int size = (""+disjunctions.size() + conjunctions.size() + xor.size() + disjoints.size()+equivalences.size()).length();
        if(!disjunctions.isEmpty()) {
            st.append("Disjunctions:\n");
            for(int[] clause : disjunctions) {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(!conjunctions.isEmpty()) {
            st.append("Conjunctions:\n");
            for(int[] clause : conjunctions) {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(!xor.isEmpty()) {
            st.append("Xor:\n");
            for(int[] clause : xor)          {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(!disjoints.isEmpty()) {
            st.append("Disjoints:\n");
            for(int[] clause : disjoints)    {st.append(clauseToString(size,clause,symboltable)).append("\n");}}
        if(!equivalences.isEmpty()) {
            st.append("Equivalences:\n");
            for(int[] clause : equivalences) {st.append(clauseToString(size,clause,symboltable)).append("\n");}}

        return st.toString();}



}
