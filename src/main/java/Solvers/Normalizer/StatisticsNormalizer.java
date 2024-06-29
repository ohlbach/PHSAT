package Solvers.Normalizer;

import Datastructures.Statistics.Statistic;
import Solvers.Solver;
import Utilities.Description;

public class StatisticsNormalizer extends Statistic {
    public StatisticsNormalizer(Solver solver) {
        super(solver);}

    @Description("True predicates in the input files.")
    int initialTrueLiterals = 0;

    @Description("Literals replaced by equivalent predicates")
    int equivalenceReplacements = 0;

    @Description("Equivalences in the input files.")
    int initialEquivalences = 0;

    @Description("True predicates derived from the clauses.")
    int derivedTrueLiterals = 0;

    @Description("Clauses simplified in different ways.")
    int simplifiedClauses = 0;

    @Description("Survived Clauses")
    int survivedClauses = 0;

    @Description("Removed Clauses")
    int removedClauses = 0;

    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Normalizer Statistics:");
        if(initialTrueLiterals > 0)      st.append("\n  Initially True Literals:  ").append(initialTrueLiterals);
        if(derivedTrueLiterals > 0)      st.append("\n  Derived True Literals:    ").append(derivedTrueLiterals);
        if(initialEquivalences > 0)      st.append("\n  Initial Equivalences:     ").append(initialEquivalences);
        if(equivalenceReplacements > 0)  st.append("\n  Equivalence Replacements: ").append(equivalenceReplacements);
        if(simplifiedClauses   > 0)      st.append("\n  Simplified Clauses:       ").append(simplifiedClauses);
        if(survivedClauses     > 0)      st.append("\n  Survived Clauses:         ").append(survivedClauses);
        if(removedClauses      > 0)      st.append("\n  Removed Clauses:          ").append(removedClauses);
        return st.toString();}

}
