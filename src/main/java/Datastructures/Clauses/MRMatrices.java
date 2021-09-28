package Datastructures.Clauses;

import Datastructures.Results.Unsatisfiable;
import Datastructures.Symboltable;
import Datastructures.TwoLiteral.TwoLitClause;
import Management.Monitor;

import java.util.ArrayList;

public class MRMatrices {
    public Clause[] combination;

    public ArrayList<MRMatrix> matrices = new ArrayList<>();
    private ArrayList<Clause> oneLitClauses = new ArrayList<>();
    private ArrayList<TwoLitClause> twoLitClauses = new ArrayList<>();


    public MRMatrices(Clause[] combination, Symboltable symboltable, Monitor monitor,  String monitorId,boolean trackReasoning,
                      ArrayList<Clause> fullyTagged, ArrayList<Clause> almostTagged) {
        this.combination = combination;
        matrices.add(new MRMatrix(combination, symboltable, monitor,monitorId, trackReasoning));
        for(int i = 0; i < fullyTagged.size(); ++i) {
            Clause clause = fullyTagged.get(i);
            boolean done = false;
            for(MRMatrix matrix : matrices) {
                if(matrix.insertClause(clause)) {done = true; break;}}
            if(done) continue;
            MRMatrix matrix = new MRMatrix(combination,symboltable, monitor,monitorId, trackReasoning);
            matrices.add(matrix);
            matrix.insertClause(clause);
            for(int j = 0; j < i; ++j) {matrix.insertClause(fullyTagged.get(j));}}

        for(int i = 0; i < almostTagged.size(); ++i) {
            Clause clause = almostTagged.get(i);
            boolean done = false;
            for(MRMatrix matrix : matrices) {
                if(matrix.insertClause(clause)) {done = true; break;}}
            if(done) continue;
            MRMatrix matrix = new MRMatrix(combination,symboltable, monitor,monitorId, trackReasoning);
            matrices.add(matrix);
            matrix.insertClause(clause);
            for(int j = 0; j < fullyTagged.size(); ++j) {matrix.insertClause(fullyTagged.get(j));}
            for(int j = 0; j < i; ++j) {matrix.insertClause(almostTagged.get(j));}}
        }

        private void mrResolve() throws Unsatisfiable {
            for(MRMatrix matrix : matrices) {
                matrix.mrResolve(oneLitClauses,twoLitClauses);}}
}
