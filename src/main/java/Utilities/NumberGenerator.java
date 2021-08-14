package Utilities;

/** This class provides a generator for a sequence of integers.
 *  It can for example be used to enumerate clauses in different solvers
 *
 */
public class NumberGenerator {
    /** the number */
    public int number = 0;

    /** starts the generator
     *
     * @param start the initial value
     */
    public NumberGenerator(int start) {
        number = start;}

    /** yields the next number
     *
     * @return the next number
     */
    public synchronized int next() {
        return number++;}

}
