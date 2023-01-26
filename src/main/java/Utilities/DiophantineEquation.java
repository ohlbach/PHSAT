package Utilities;

import it.unimi.dsi.fastutil.ints.IntArrayList;

//import javax.xml.bind.ValidationEvent;

/** This class solves constrained diophantine equations of a particular size:
 *  n_1*x_1 + ... + n_k*x_k + y = z<br>
 *  where x_i are 0 or 1 and y is constrained by 0...n
 */
public class DiophantineEquation {
    private final IntArrayList binaryVariables;
    private final int constrainedVariable;
    private int result;
    public boolean allNeeded = false;

    /** constructs a diophantine equation
     *
     * @param binaryVariables      the coefficients of then binary variable
     * @param constrainedVariable  the maximum of the constrained variable
     * @param result               the result of the equation
     */
    public DiophantineEquation(IntArrayList binaryVariables, int constrainedVariable, int result) {
        this.binaryVariables = binaryVariables;
        this.constrainedVariable = constrainedVariable;
        this.result = result;}

    /** returns the maximum value the left side of the equation can take
     *
     * @return the maximum value the left side of the equation can take
     */
    public int maxValue() {
        int value = 0;
        for(int val : binaryVariables) value += val;
        return value + constrainedVariable;}

    /** checks if the equation is solvable
     *
     * @return true if the equation is solvable
     */
    public boolean isSolvable() {
        int size = binaryVariables.size();
        if(constrainedVariable >= result) return true;
        for(int n = 1; n <= size; ++n) {
            if(n == size) allNeeded = true;
            int minValue = Integer.MAX_VALUE;
            for(int i : Utilities.combinations(size,n)) {
                int value = 0;
                for(int j = 0; j < size; ++j) {
                    if(((1 << j) & i) != 0) value += binaryVariables.getInt(j);}
                minValue = Math.min(minValue,value);
                int diff = result-value;
                if(0 <= diff && diff <= constrainedVariable) return true;}
            if(minValue + constrainedVariable > result) return false;}
        return false;}

    /** computes the smallest value >= result the left side can take on.
     *
     * @return the smallest value >= result the left side can take on.
     */
    public int minSolution() {
        int oldResult = result;
        int max = maxValue();
        try{for(; result <= max; ++result) {
                if(isSolvable()) return result;}}
        finally{result = oldResult;}
        return -1;}

    /** computes those variable indices whose value = 1 is needed for solving the equation
     *
     * @return null or the list of variable indices whose value = 1 is needed for solving the equation
     */
    public IntArrayList needed(int result) {
        if(!allNeeded) return null;
        IntArrayList needed = null;
        int maxValue = maxValue();
        for(int i = 0; i < binaryVariables.size(); ++i) {
            int value = binaryVariables.getInt(i);
            if(maxValue - value < result) {
                if(needed == null) needed = new IntArrayList();
                needed.add(i);}}
        return needed;}

    /** returns the equation as string
     *
     * @return the equation as string
     */
    public String toString() {
        StringBuilder st = new StringBuilder();
        int size = binaryVariables.size();
        for(int i = 0; i < size; ++i) {
            st.append(binaryVariables.getInt(i)).append("*x_").append(i);
            if(i < size-1) st.append(" + ");}
        if(constrainedVariable != 0) st.append(" + y(<=").append(constrainedVariable).append(")");
        st.append(" = ").append(result);
        return st.toString();}

}
