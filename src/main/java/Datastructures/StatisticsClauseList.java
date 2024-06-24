package Datastructures;

import Datastructures.Statistics.Statistic;
import Utilities.Description;

public class StatisticsClauseList extends Statistic {

    /**
     * Represents a statistics object for the Clause List.
     *
     * This class extends the Statistic class and provides statistics related to the Clause List.
     * The statistics include the number of subsumed clauses, pure literals, singleton literals,
     * and merged resolvents.
     *
     * The statistics are initialized to zero by default. They can be updated by direct access to the
     * variables in this class. The toString() method is used to retrieve a string representation
     * of the statistics.
     */
    public StatisticsClauseList() {
        super("ClauseList");}

    @Description("Subsumed Clauses")
    public int subsumedClauses = 0;

    @Description("Pure Literals")
    public int pureLiterals = 0;

    @Description("Singleton Literals")
    public int singletonLiterals = 0;

    @Description("Merged Resolvents")
    public int mergedResolvents = 0;

    /**
     * Returns a string representation of the Clause List Statistics.
     *
     * This method constructs a StringBuilder object and appends the different statistics related to
     * the Clause List. The StringBuilder is then converted to a string using the toString() method
     * of the StringBuilder class.
     *
     * The different statistics appended to the StringBuilder object include:
     * - Subsumed Clauses
     * - Pure Literals
     * - Singleton Literals
     * - Merged Resolvents
     *
     * Only the statistics with non-zero values will be appended to the StringBuilder. Each statistic
     * is formatted with a label and its corresponding value. The labels are indented for better readability.
     *
     * @return a string representation of the Clause List Statistics
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        st.append("Clause List Statistics:");
        if(subsumedClauses > 0)    st.append("\n  Subsumed Clauses:   ").append(subsumedClauses);
        if(pureLiterals > 0)       st.append("\n  Pure Literals:      ").append(pureLiterals);
        if(singletonLiterals > 0)  st.append("\n  Singleton Literals: ").append(singletonLiterals);
        if(mergedResolvents > 0)   st.append("\n  Merged Resolvents:  ").append(mergedResolvents);
        return st.toString();}

    }
