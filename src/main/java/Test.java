import com.sun.org.glassfish.gmbal.Description;

import javax.swing.plaf.basic.BasicIconFactory;
import java.lang.reflect.Field;
import java.util.BitSet;
import java.util.function.Function;

/**
 * Created by ohlbach on 23.10.2018.
 */
public class Test {
    Test(){}

    Test(int n) {this.n = n;}

    @Description("test description")
    public int n;
    public int m;

    public Function<Integer,Integer> plus  = m -> m + n;

    public static void main(String[] args) throws Exception {
        BitSet b = new BitSet(10000000);
        b.set(0,true);
        b.set(9999999,true);
        System.out.println(b.size());
        System.out.println(b.length());
        int x = 0;
        long time =System.nanoTime();
        for(int i = 1; i < 10000; ++i) {
            x = b.nextSetBit(1);}
        System.out.println(System.nanoTime()-time);
        System.out.println(x);
    }

}
