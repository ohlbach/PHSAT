package Solvers.Normalizer;

import Datastructures.Statistics.Statistic;
import Solvers.Solver;
import Utilities.Description;

public class NormalizerStatistics extends Statistic {
    public NormalizerStatistics(Solver solver) {
        super(solver);}

    @Description("True literals in the input files.")
    int initialTrueLiterals = 0;

    @Description("Equivalences in the input files.")
    int initialEquivalences = 0;

    @Description("True literals derived from the clauses.")
    int derivedTrueLiterals = 0;

    @Description("Clauses simplified in different ways.")
    int simplifiedClauses = 0;

    @Description("Simplifications by dividing multiplicities by their greatest common divisor.")
    int gcdReductions = 0;

    @Description("Survived Clauses")
    int survivedClauses = 0;

    @Description("Removed Clauses")
    int removedClauses = 0;

    @Description("Pure Literals")
    int pureLiterals = 0;

    @Description("Singleton Literals")
    int singletonLiterals = 0;


    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Normalizer Statistics:");
        if(initialTrueLiterals > 0) st.append("\n  Initially True Literals: ").append(initialTrueLiterals);
        if(derivedTrueLiterals > 0) st.append("\n  Derived True Literals:   ").append(derivedTrueLiterals);
        if(initialEquivalences > 0) st.append("\n  Initial Equivalences:    ").append(initialEquivalences);
        if(simplifiedClauses   > 0) st.append("\n  Simplified Clauses:      ").append(simplifiedClauses);
        if(gcdReductions       > 0) st.append("\n  GCD Reductions:          ").append(gcdReductions);
        if(survivedClauses     > 0) st.append("\n  Survived Clauses:        ").append(survivedClauses);
        if(removedClauses      > 0) st.append("\n  Removed Clauses:         ").append(removedClauses);
        if(pureLiterals        > 0) st.append("\n  Pure Literals:           ").append(pureLiterals);
        if(singletonLiterals   > 0) st.append("\n  Singleton Literals:      ").append(singletonLiterals);
        return st.toString();}

}
