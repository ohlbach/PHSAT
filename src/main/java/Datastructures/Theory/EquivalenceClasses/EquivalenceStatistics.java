package Datastructures.Theory.EquivalenceClasses;

import Datastructures.Statistics.Statistic;
import org.glassfish.gmbal.Description;

public class EquivalenceStatistics extends Statistic {


    public EquivalenceStatistics(String id) {
        super(id);}

    @Description("number of input classes")
    public int inputClasses = 0;

    @Description("number of derived equivalence classes")
    public int derivedClasses = 0;

    @Description("number of joined equivalence classes")
    public int joinedClasses = 0;

    @Description("number of imported true literals")
    public int importedTrueLiterals = 0;

    @Description("number of derived true literals")
    public int derivedTrueLiterals = 0;

    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Equivalence Classes Statistics:\n");
        if(inputClasses != 0)
            st.append("Number of Input Classes:         ").append(inputClasses);
        if(derivedClasses != 0)
            st.append("Number of Derived Classes:       ").append(inputClasses);
        if(joinedClasses != 0)
            st.append("Number of Joined Classes:        ").append(joinedClasses);
        if(importedTrueLiterals != 0)
            st.append("Number of Imported True Literals:").append(importedTrueLiterals);
        if(derivedTrueLiterals != 0)
            st.append("Number of Derived True Literals: ").append(derivedTrueLiterals);
        return st.toString();
    }


}
