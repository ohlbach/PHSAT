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


}
