package Solvers.Normalizer;

import Datastructures.Statistics.Statistic;
import Solvers.Solver;
import Utilities.Description;

public class StatisticsNormalizer extends Statistic {
    public StatisticsNormalizer(Solver solver) {
        super(solver);}

    @Description("Tautologies in disjunctions")
    int initialTautologies = 0;

    @Description("Complementary Literals in non-disjunctions")
    int complementaryLiterals = 0;

    @Description("Reduced Multiplicities")
    int reducedMultiplicities = 0;

    @Description("True predicates in the input files.")
    int initialTrueLiterals = 0;

    @Description("Clauses reduced to their essential predicates (e.g. >= 2 p^2,q^2,r -> p,q.")
    int essentialClauses = 0;

    @Description("Literals replaced by equivalent predicates")
    int equivalenceReplacements = 0;

    @Description("Equivalences in the input files.")
    int initialEquivalences = 0;

    @Description("True predicates derived from the clauses.")
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

    @Description("Hidden Conjunctions (atleast 3 p,q,r or atmost 0 p,q,r")
    int hiddenConjunctions = 0;

    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Normalizer Statistics:");
        if(initialTautologies > 0)       st.append("\n  Initial Tautologies:      ").append(initialTautologies);
        if(complementaryLiterals > 0)    st.append("\n  Complementary Literals:   ").append(complementaryLiterals);
        if(reducedMultiplicities > 0)    st.append("\n  Reduced Multiplicities:   ").append(reducedMultiplicities);
        if(initialTrueLiterals > 0)      st.append("\n  Initially True Literals:  ").append(initialTrueLiterals);
        if(derivedTrueLiterals > 0)      st.append("\n  Derived True Literals:    ").append(derivedTrueLiterals);
        if(essentialClauses > 0)         st.append("\n  Essential Clauses:        ").append(essentialClauses);
        if(initialEquivalences > 0)      st.append("\n  Initial Equivalences:     ").append(initialEquivalences);
        if(equivalenceReplacements > 0)  st.append("\n  Equivalence Replacements: ").append(equivalenceReplacements);
        if(simplifiedClauses   > 0)      st.append("\n  Simplified Clauses:       ").append(simplifiedClauses);
        if(gcdReductions       > 0)      st.append("\n  GCD Reductions:           ").append(gcdReductions);
        if(survivedClauses     > 0)      st.append("\n  Survived Clauses:         ").append(survivedClauses);
        if(removedClauses      > 0)      st.append("\n  Removed Clauses:          ").append(removedClauses);
        if(pureLiterals        > 0)      st.append("\n  Pure Literals:            ").append(pureLiterals);
        if(singletonLiterals   > 0)      st.append("\n  Singleton Literals:       ").append(singletonLiterals);
        if(hiddenConjunctions  > 0)      st.append("\n  Hidden Conjunctions:      ").append(hiddenConjunctions);
        return st.toString();}

}
