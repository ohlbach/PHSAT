package Solvers.Resolution;

import Datastructures.Statistics.Statistic;
import Utilities.Description;

public class ResolutionStatistics extends Statistic {
    public ResolutionStatistics(String id) {
        super(id);
    }

    @Description("number of initial or- and atleast-clauses")
    public int initialClauses = 0;

    @Description("number of derived true literal")
    public int derivedTrueLiterals = 0;

    @Description("number of pure literals")
    public int pureLiterals = 0;

    @Description("number of partially pure literals")
    public int partiallyPureLiterals = 0;

    @Description("number of true literals in saturated 2-literal clauses.")
    public int saturatedLiterals = 0;

    @Description("number of subsumed clauses")
    public int subsumedClauses = 0;

    @Description("number of merged clauses")
    public int mergedClauses = 0;

    @Description("number of binary resolvents")
    public int binaryResolvents = 0;

    @Description("number of longer resolvents")
    public int longerResolvents = 0;
    @Description("merge resolution between binray clauses")
    public int mergeResolutionTwoTwo = 0;
    @Description("merge resolution between a binary clause and a longer clause")
    public int mergeResolutionTwoMore = 0;
    @Description("merge resolution between a longer clauses")
    public int mergeResolutionMoreMore = 0;
    @Description("binary equivalences")
    public int binaryEquivalences = 0;
    @Description("triggered equivalences")
    public int triggeredEquivalences = 0;
    @Description("equivalence replacements")
    public int equivalenceReplacements = 0;
    @Description("triggered equivalence replacements")
    public int equivalenceReplacementsTriggered = 0;

    @Description("number of complementary literal pairs.")
    public int complementaryLiterals = 0;

    @Description("number of predicates eliminated by resolution.")
    public int resolvedPredicates = 0;


    public void clear() {
        initialClauses                   = 0;
        derivedTrueLiterals              = 0;
        pureLiterals                     = 0;
        partiallyPureLiterals            = 0;
        resolvedPredicates               = 0;
        complementaryLiterals            = 0;
        saturatedLiterals                = 0;
        subsumedClauses                  = 0;
        mergedClauses                    = 0;
        binaryResolvents                 = 0;
        longerResolvents                 = 0;
        mergeResolutionTwoTwo            = 0;
        mergeResolutionTwoMore           = 0;
        mergeResolutionMoreMore          = 0;
        binaryEquivalences               = 0;
        equivalenceReplacements          = 0;
        equivalenceReplacementsTriggered = 0;
    }
    public String toString(){
        StringBuilder st = new StringBuilder();
        st.append("Simplifier Statistics\n");
        st.append("Initial Clauses:                   ").append(initialClauses).append("\n");
        st.append("Derived Unit Clauses:              ").append(derivedTrueLiterals).append("\n");
        st.append("Pure Literals:                     ").append(pureLiterals).append("\n");
        st.append("Parially pure Literals:            ").append(partiallyPureLiterals).append("\n");
        st.append("Resolved Predicates:               ").append(resolvedPredicates).append("\n");
        st.append("complementary Literals:            ").append(complementaryLiterals).append("\n");
        st.append("Saturated Literals:                ").append(saturatedLiterals).append("\n");
        st.append("Subsumed Clauses:                  ").append(subsumedClauses).append("\n");
        st.append("Merged Clauses:                    ").append(mergedClauses).append("\n");
        st.append("Binary Resolvents:                 ").append(binaryResolvents).append("\n");
        st.append("Longer Resolvents:                 ").append(longerResolvents).append("\n");
        st.append("Merge Resolution binary-binary:    ").append(mergeResolutionTwoTwo).append("\n");
        st.append("Merge Resolution binary-longer:    ").append(mergeResolutionTwoMore).append("\n");
        st.append("Merge Resolution longer-longer:    ").append(mergeResolutionMoreMore).append("\n");
        st.append("Binary Equivalences:               ").append(binaryEquivalences).append("\n");
        st.append("Triggered Equivalences:            ").append(triggeredEquivalences).append("\n");
        st.append("Equivalence Replacements:          ").append(equivalenceReplacements).append("\n");
        st.append("Triggered Equivalence Replacements ").append(equivalenceReplacementsTriggered);
        return st.toString();
    }



}
