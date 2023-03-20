package Datastructures.Clauses;

import Datastructures.Results.Unsatisfiable;
import Datastructures.TwoLiteral.TwoLitClause;

import java.util.ArrayList;

public class MRMatrices {


    public MRMatrices() {}

    public void mrResolve(Clause[] disjointnessClauses, ArrayList<Clause> clauses, ArrayList<TwoLitClause> twoLitClauses) throws Unsatisfiable {
        ArrayList<MRMatrix> matrices = new ArrayList<>();
        //matrices.add(new MRMatrix(allClauses,disjointnessClauses));
        for(int i = 0; i < clauses.size(); ++i) {
            Clause clause = clauses.get(i);
            boolean done = false;
            for(MRMatrix matrix : matrices) {
                if(matrix.insertClause(clause)) {done = true; break;}}
            if(done) continue;
            MRMatrix matrix = null; //new MRMatrix(allClauses,disjointnessClauses);
            matrices.add(matrix);
            matrix.insertClause(clause);
            for(int j = 0; j < i; ++j) {matrix.insertClause(clauses.get(j));}}

        for(MRMatrix matrix : matrices) {matrix.mrResolve(twoLitClauses);}
    }

}
