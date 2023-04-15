package Solvers.Simplifier;

import Datastructures.Statistics.Statistic;
import org.glassfish.gmbal.Description;

public class SimplifierStatistics extends Statistic {
    public SimplifierStatistics(String id) {
        super(id);
    }

    @Description("number of initial or- and atleast-clauses")
    public int orAndAtleastCLauses = 0;
    @Description("number of not internalized input clauses ")
    public int notInternalizedInputClauses = 0;

    @Description("number of derived unit clauses")
    public int derivedUnitClauses = 0;

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

    @Description("equivalence replacements")
    public int equivalenceReplacements = 0;

    @Description("number of complementary literal pairs.")
    public int complementaryLiterals = 0;


    public void clear() {
        orAndAtleastCLauses         = 0;
        notInternalizedInputClauses = 0;
        derivedUnitClauses          = 0;
        pureLiterals                = 0;
        partiallyPureLiterals       = 0;
        complementaryLiterals       = 0;
        saturatedLiterals           = 0;
        subsumedClauses             = 0;
        mergedClauses               = 0;
        binaryResolvents            = 0;
        longerResolvents            = 0;
        equivalenceReplacements     = 0;
    }
    public String toString(){
        StringBuilder st = new StringBuilder();
        st.append("Simplifier Statistics\n");
        st.append("Initial Clauses:           ").append(orAndAtleastCLauses).append("\n");
        st.append("Initially Removed Clauses: ").append(notInternalizedInputClauses).append("\n");
        st.append("Derived Unit Clauses:      ").append(derivedUnitClauses).append("\n");
        st.append("Pure Literals:             ").append(pureLiterals).append("\n");
        st.append("Parially pure Literals:    ").append(partiallyPureLiterals).append("\n");
        st.append("complementary Literals:    ").append(complementaryLiterals).append("\n");
        st.append("Saturated Literals:        ").append(saturatedLiterals).append("\n");
        st.append("Subsumed Clauses:          ").append(subsumedClauses).append("\n");
        st.append("Merged Clauses:            ").append(mergedClauses).append("\n");
        st.append("Binary Resolvents:         ").append(binaryResolvents).append("\n");
        st.append("Longer Resolvents:         ").append(longerResolvents).append("\n");
        st.append("Equivalence Replacements:  ").append(equivalenceReplacements);
        return st.toString();
    }



}
