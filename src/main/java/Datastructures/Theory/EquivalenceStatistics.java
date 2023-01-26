package Datastructures.Theory;

import Datastructures.Statistics.Statistic;
import org.glassfish.gmbal.Description;

public class EquivalenceStatistics extends Statistic {


    public EquivalenceStatistics(String id) {
        super(id);}

    @Description("number of basic clauses")
    public int basicClauses = 0;

    @Description("number of clauses")
    public int clauses = 0;

    @Description("number of derived equivalence classes")
    public int derivedClasses = 0;

    @Description("number of joined equivalence classes")
    public int joinedClasses = 0;

    @Description("number of derived true literals")
    public int derivedTrueLiterals = 0;


}
