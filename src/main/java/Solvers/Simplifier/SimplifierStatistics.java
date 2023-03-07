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

    @Description("number of subsumed clauses")
    public int subsumedClauses = 0;

    public void clear() {
        orAndAtleastCLauses         = 0;
        notInternalizedInputClauses = 0;
        derivedUnitClauses          = 0;
        pureLiterals                = 0;
        subsumedClauses             = 0;
    }
    public String toString(){
        StringBuilder st = new StringBuilder();
        st.append("Simplifier Statistics\n");
        st.append("Initial Clauses:           ").append(orAndAtleastCLauses).append("\n");
        st.append("Initially Removed Clauses: ").append(notInternalizedInputClauses).append("\n");
        st.append("Derived Unit Clauses:      ").append(derivedUnitClauses).append("\n");
        st.append("Pure Literals:             ").append(pureLiterals).append("\n");
        st.append("Subsumed Clauses:          ").append(subsumedClauses);
        return st.toString();
    }



}
