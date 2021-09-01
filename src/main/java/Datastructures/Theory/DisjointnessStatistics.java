package Datastructures.Theory;

import Datastructures.Statistics.Statistic;
import com.sun.org.glassfish.gmbal.Description;

import java.util.ArrayList;


public class DisjointnessStatistics extends Statistic {

    public static ArrayList<DisjointnessStatistics> statistics = new ArrayList<>();

    static {Statistic.statisticsClasses.add(DisjointnessStatistics.class);}

    public DisjointnessStatistics(String id) {
        super(id);
        statistics.add(this);}

    @Description("number of clauses")
    public int clauses = 0;

    @Description("number of submitted true literals")
    public int trueLiterals = 0;

    @Description("number of submitted equivalences")
    public int equivalences = 0;

    @Description("number of submitted derived disjointnesses")
    public int derivedDisjointesses = 0;

    @Description("number of new disjoitnesses subsumed by old ones")
    public int backwardSubsumed = 0;

    @Description("number of old disjointnesses subsumed by new ones")
    public int forwardSubsumed = 0;

    @Description("number of derived false literals")
    public int derivedLiterals = 0;

    @Description("number of literals p != -p")
    public int tautologies = 0;

    @Description("number of resolutions")
    public int resolutions = 0;

    @Description("number of joined disjointness classes")
    public int joinedClasses = 0;

    @Description("number of extended disjointness classes")
    public int extendedClasses = 0;


}
