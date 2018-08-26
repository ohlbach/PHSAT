package Datastructures.Clauses;

import Datastructures.Model;
import Datastructures.Symboltable;

import java.util.ArrayList;

/**
 * Created by ohlbach on 26.08.2018.
 */
public class ClauseList {
    public String info;
    private final ArrayList<Clause> clauses;
    private final Model model;
    private final Symboltable symboltable;

    public ClauseList(Model model,Symboltable symboltable) {
        clauses = new ArrayList<Clause>();
        this.model = model;
        this.symboltable = symboltable;}

    public ClauseList(int size, Model model,Symboltable symboltable) {
        clauses = new ArrayList<Clause>(size);
        this.model = model;
        this.symboltable = symboltable;}

    public void addClause(Clause clause) {
        clauses.add(clause);}

    public void removeClause(Clause clause) {
        clauses.remove(clause);}

    public int size() {return clauses.size();}

    public String toString(){
        StringBuffer st = new StringBuffer();
        if(info != null) {st.append(info).append("\n");}
        if(!model.isEmpty()) {st.append("Partial Model: ").append(model.toString()).append("\n");}
        int numbersize = (""+clauses.size()).length();
        for(Clause clause : clauses) {
            if(!clause.isTrue(model)) {
                st.append(clause.toString(model,numbersize)).append("\n");}}
        return st.toString();
    }





}
