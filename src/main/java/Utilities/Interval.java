package Utilities;

/** This class provides integer intervals with non-negative integers
 */
public class Interval {
    public int min; // the lower bound
    public int max; // the upper bound

    /** constructs an interval
     *
     * @param min the lower bound
     * @param max the upper bound
     */
    public Interval(int min, int max) {
        assert min >= 0; assert max >= 0;
        assert min <= max;
        this.min = min;
        this.max = max;}

    /** clones the interval
     *
     * @return a clone of the interval
     */
    public Interval clone() {
        return new Interval(min,max);}


    /** checks the two intervals for equalit
     *
     * @param interval an interval
     * @return true if the two intervals ar identical.
     */
    public boolean equals(Interval interval) {
        return min == interval.min && max == interval.max;}

    /** computes the size of the interval.
     * Examples: [3,3] -> 1,  [3,4] -> 2
     *
     * @return the size of the interval (including the bounds)
     */
    public int size() {
        return max - min + 1;}

    /** checks if the interval is a singleton [n,n]
     *
     * @return -1 if the interval is no singleton, otherwise the singleton value.
     */
    public int isSingleton() {
        return (min == max) ? min : -1;}

    /** decrements both bounds by 1 */
    public void decrement() {
        assert max > 0;
        min = Math.max(0,min-1);
        max -= 1;}

    /** decrements the upper bound by 1, if possible
     *
     * @return true if decrementing was possible
     */
    public boolean decrementMax() {
        assert max > 0;
        if(max > min) {max -= 1; return true;}
        return false;}

    /** checks if this is a subset of interval
     *
     * @param interval an interval
     * @return true if this is a subset of interval
     */
    public boolean isSubset(Interval interval) {
        return min >= interval.min && max <= interval.max;}

    /** intersects the interval with another interval
     *
     * @param interval an interval
     * @return the intersection interval, or null if the intersection is empty.
     */
    public Interval intersect(Interval interval) {
        int newMin = Math.max(min,interval.min);
        int newMax = Math.min(max,interval.max);
        return newMin <= newMax ? new Interval(newMin,newMax) : null;}

    /** a string representation of the interval
     *
     * @return a string representation, either just n, or [n,m]
     */
    public String toString() {
        return min == max ? Integer.toString(min) : "["+min+","+max+"]";}

}
