package Utilities;

import java.util.Iterator;

/** The iterator enumerates all combination of m integers as int[] of length n.<br>
 *  Example: combinationsIterator(5,2) yields.<br>
 * [0, 1]<br>
 * [0, 2]<br>
 * [0, 3]<br>
 * [0, 4]<br>
 * [1, 2]<br>
 * [1, 3]<br>
 * [1, 4]<br>
 * [2, 3]<br>
 * [2, 4]<br>
 * [3, 4]<br>
 * In total these are m over n combinations. (5 over 2 = 10).
 */
public class CombinationsIterator implements Iterator<int[]> {
    public final int m;
    public final int n;
    private final int[] pattern;
    private boolean start = true;

    /** generates the iterator
     *
     * @param m the number of integers
     * @param n the length of the array.
     */
    public CombinationsIterator(int m, int n) {
        pattern = new int[n];
        for(int i = 0; i < n; ++i) pattern[i] = i;
        this.m = m - 1;
        this.n = n - 1;}

    /** checks if there is another combination.
     *
     * @return true if there is another combination.
     */
    @Override
    public boolean hasNext() {
        if(start) {start = false; return true;}
        if(pattern[n] < m) {++pattern[n]; return true;}
        for(int i = n-1; i >= 0; --i) {
            int j = pattern[i];
            if(j < m-(n-i)) {
                ++pattern[i]; ++j;
                for(int k = i+1; k <= n; ++k) pattern[k] = ++j;
                return i > 0 || j <= m;}}
        return false;}

    /** gets the next combination.
     *
     * @return the next combination.
     */
    @Override
    public int[] next() {return pattern;}

    /*
    public static void main(String[] args) {
        CombinationsIterator it = new CombinationsIterator(5,2);
        while(it.hasNext()) {
            System.out.println(Arrays.description(it.next()));}}
            */

}
