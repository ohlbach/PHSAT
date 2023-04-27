package Solvers.Normalizer;

import Datastructures.Statistics.Statistic;
import Solvers.Solver;
import Utilities.Description;

public class NormalizerStatistics extends Statistic {
    public NormalizerStatistics(Solver solver) {
        super(solver);}

    @Description("True literals in the input files.")
    int initialTrueLiterals = 0;

    @Description("True literals derived from the clauses.")
    int derivedTrueLiterals = 0;

    @Description("Clauses simplified in differernt ways.")
    int simplifiedClauses = 0;

    @Description("Simplifications by dividing multiplicities by their greatest common divisor.")
    int gcdReductions = 0;

    @Description("Equivalence classes.")
    int equivalenceClasses = 0;

    public String toString() {
        StringBuilder st = new StringBuilder();
        if(initialTrueLiterals > 0) st.append("Initially True Literals: ").append(initialTrueLiterals);
        if(derivedTrueLiterals > 0) st.append("Derived True Literals:   ").append(derivedTrueLiterals);
        if(simplifiedClauses   > 0) st.append("Simplified Clauses:      ").append(simplifiedClauses);
        if(gcdReductions       > 0) st.append("GCD Reductions:          ").append(gcdReductions);
        if(equivalenceClasses  > 0) st.append("Equivalence Classes:     ").append(equivalenceClasses);
        return st.toString();}

}
