import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.util.Comparator;
import java.util.concurrent.PriorityBlockingQueue;

/**
 * Created by ohlbach on 23.10.2018.
 */
public class Test {

    public static void main(String[] args) throws InterruptedException{
        int counter = 0;
        int n = 5;
        for(int i1 = 0; i1 < n; ++i1) {
            for(int i2 = 0; i2 < n; ++i2) {
                if(i2 == i1) continue;
                for(int i3 = 0; i3 < n; ++i3) {
                    if(i3 == i1 || i3 == i2) continue;
                        for(int i4 = 0; i4 < n; ++i4) {
                            if(i4 == i1 || i4 == i2 || i4 == i3) continue;
                                for(int i5 = 0; i5 < n; ++i5) {
                                if(i5 == i1 || i5 == i2 || i5 == i3 || i5 == i4) continue;
                                ++counter;
                                System.out.println(i1 + " " + i2 + " " + i3 + " " + i4 + " " + i5);}
                }
            }
        }}
        System.out.println(counter);
        System.out.println(1*2*3*4*5*6*7*8*9*10*11*12);

    }

}
